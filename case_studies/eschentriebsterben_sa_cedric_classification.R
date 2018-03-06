############################################################################
# Eschentriebsterben SA Cedric: RandomForest Classification
#
# (c) by Dominique Weber, HAFL
############################################################################

library(raster)
library(rgdal)
library(randomForest)
library(sp)

###########################
# USER CONFIG

# path (WITH BACKSLASH) and dates for stacks
STACK_PATH = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/"
STACK_DATES = c("20170430","20170510")

# path
SP.PATH = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Eschentriebsterben/SA_Cedric/Data/Field/samples_moy_2000m2_wgs84.shp"

# S2 band names and indices for the corresponding raster stacks
BAND_NAMES = c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B11","B12")
BAND_INDICES = 1:12

TARGET_VALUE_NAME = "class" # WARNING: CHANGE TARGET VALUE FOR THE RANDOM FOREST MODEL MANUALLY FURTHER DOWN (ca. L142)

# write predicted file to
PREDICTION.FILE = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Eschentriebsterben/SA_Cedric/rf_prediction.tif"
PROB.FILE = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Eschentriebsterben/SA_Cedric/rf_prob.tif"

# crop extent
ext = extent(352510, 356430, 5201570, 5204170)

###########################
# LOAD RASTER DATA

print("Loading raster data..")

# prepare stackList
stackList = list(
  stack = vector(),
  columnStart = vector()
)

# filter .tif files
fileNames = list.files(STACK_PATH)
fileNames = fileNames[grep("tif$", fileNames)]

# load stacks and add them to stacklist
lapply(STACK_DATES, function(date) {
  tmp_stk <-  stack(paste(
    STACK_PATH, 
    fileNames[grep(date, fileNames)][[1]],
    sep=""
  ))
  
  stackList$stack        <<- c(stackList$stack, tmp_stk)
  stackList$columnStart  <<- c(stackList$columnStart, paste(date, "_", sep=""))
})

# check if stackList.lenght == columnStart.lenght
try(if (length(stackList$stack) != length(stackList$columnStart)) 
  stop("length of stack not equal to lenght of columnStarts") 
)

# apply names and select bands
for(i in 1:length(stackList$stack)) {
  # select relevant bands
  stackList$stack[[i]] = stackList$stack[[i]][[BAND_INDICES]]
  # name stacks
  names(stackList$stack[[i]]) = c(BAND_NAMES)
}

###########################
# LOAD TRAINING DATA

print("Loading training data...")
# sdata <- readOGR(dsn = SHAPE.PATH, layer = SHAPE.NAME)
sdata <- readOGR(dsn =SP.PATH)
sdata@data[,TARGET_VALUE_NAME] = as.factor(sdata@data[,TARGET_VALUE_NAME])

# add ctrl data
sdata_crtl = readOGR("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Eschentriebsterben/SA_Cedric/Data/Control/30_punkte_wald_perimeter_keine_esche_buffer_wgs84.shp")
sdata_crtl$class = rep("crtl", length(sdata_crtl))

# merge and remove all other columns
sdata = rbind(sdata[,"class"], sdata_crtl[,"class"])

plotRGB(stackList$stack[[1]][[4:2]], stretch="lin", axes=T)
plot(sdata, add=T, col="red", border="red", lwd=3)

###########################
# PREPARE DATASET

print("Prepare predictors and response")

df <- data.frame()

for(i in 1:length(stackList$stack)) {
  print(paste("loading stack:", i, "of", length(stackList$stack)))
  
  stk <- stackList$stack[[i]]
  colStart <- stackList$columnStart[[i]]
  
  # extract mean spectral values per plot and bands
  print("extract mean spectral values per plot and bands")
  e = extract(stk, sdata, fun=mean, sp=T, na.rm=T)
  
  e.df = data.frame(e@data[,TARGET_VALUE_NAME], e@data[,c(BAND_NAMES)])
  names(e.df)[1] = TARGET_VALUE_NAME
  
  # copy target variable only in first subset
  if (i==1) {
    df_tmp = data.frame(e.df[,TARGET_VALUE_NAME],e.df[,c(BAND_NAMES)])
    colnames(df_tmp) = c(TARGET_VALUE_NAME, BAND_NAMES)
  } else {
    df_tmp = data.frame(e.df[,c(BAND_NAMES)])
    colnames(df_tmp) = BAND_NAMES
  }
  
  # calc some indices (see http://www.sciencedirect.com/science/article/pii/S092427161300107X)
  print("calc VIs...")
  df_tmp$NDVI = (df_tmp$B8 - df_tmp$B4)/(df_tmp$B8 + df_tmp$B4)
  df_tmp$IRECI = (df_tmp$B7-df_tmp$B4)/(df_tmp$B5/df_tmp$B6)
  df_tmp$NDI45 = (df_tmp$B5-df_tmp$B4)/(df_tmp$B5+df_tmp$B4)
  df_tmp$MCARI = ((df_tmp$B5-df_tmp$B4)-0.2*(df_tmp$B5-df_tmp$B3))*(df_tmp$B5-df_tmp$B4)
  df_tmp$GNDVI = (df_tmp$B7-df_tmp$B3)/(df_tmp$B7+df_tmp$B3)
  
  # set colnames again, adding colStart to make colnames unique. Add dt_tmp to df
  if (length(df) == 0) {
    names(df_tmp)[-1] = paste(colStart, colnames(df_tmp)[-1], sep = "")
    df <- df_tmp
  } else {
    names(df_tmp) = paste(colStart, colnames(df_tmp), sep = "")
    df <- cbind(df, df_tmp)
  }
}

###########################
# RUN RF MODEL

print("Run random forest model")
df = do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x),NA)))
rf.mdl <- randomForest(class ~ ., data=df, ntree=1000, importance=TRUE, na.action=na.omit)
plot(rf.mdl)
varImpPlot(rf.mdl, type=1, n.var=10)
print(rf.mdl)

# save model
saveRDS(rf.mdl, "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Eschentriebsterben/SA_Cedric/rf_mdl.rds")

###########################
# RUN RF PREDICTION

# crop to study area
stackList$stack = lapply(stackList$stack, function(i) crop(i, ext))

print("Prepare raster data for prediction")
finalStack <- NA

for(i in 1:length(stackList$stack)) {
  print(paste("Creating stack for prediction:", i, "of", length(stackList$stack)))
  
  pred <- stackList$stack[[i]]
  colStart <- stackList$columnStart[[i]]
  
  names(pred) = c(BAND_NAMES)
  
  pred$NDVI = (pred$B8 - pred$B4)/(pred$B8 + pred$B4)
  pred$IRECI = (pred$B7-pred$B4)/(pred$B5/pred$B6)
  pred$NDI45 = (pred$B5-pred$B4)/(pred$B5+pred$B4)
  pred$MCARI = ((pred$B5-pred$B4)-0.2*(pred$B5-pred$B3))*(pred$B5-pred$B4)
  pred$GNDVI = (pred$B7-pred$B3)/(pred$B7+pred$B3)
  
  # add prefix to every column
  names(pred) = paste(colStart, names(pred), sep = "")
  
  #add stack to final stack
  if (i == 1) 
    finalStack = pred
  else
    finalStack = stack(finalStack, pred)
}

print("Run model prediction")
beginCluster()
rf.prediction <- clusterR(finalStack, predict, args = list(model = rf.mdl, type="response", index=1, na.rm=TRUE, progress="text"))
rf.prediction.prob1 <- clusterR(finalStack, predict, args = list(model = rf.mdl, type="prob", index=1, na.rm=TRUE, progress="text"))
rf.prediction.prob2 <- clusterR(finalStack, predict, args = list(model = rf.mdl, type="prob", index=2, na.rm=TRUE, progress="text"))
rf.prediction.prob3 <- clusterR(finalStack, predict, args = list(model = rf.mdl, type="prob", index=3, na.rm=TRUE, progress="text"))
rf.prediction.prob4 <- clusterR(finalStack, predict, args = list(model = rf.mdl, type="prob", index=4, na.rm=TRUE, progress="text"))
rf.prediction.prob5 <- clusterR(finalStack, predict, args = list(model = rf.mdl, type="prob", index=5, na.rm=TRUE, progress="text"))
endCluster()

# probabilities corresponding the predicted class
rf.prob = calc(stack(c(rf.prediction.prob1, rf.prediction.prob2, rf.prediction.prob3, rf.prediction.prob4, rf.prediction.prob5)), max)

print("Write prediction file")
writeRaster(rf.prediction, PREDICTION.FILE, overwrite=T)
writeRaster(rf.prob, PROB.FILE, overwrite=T)