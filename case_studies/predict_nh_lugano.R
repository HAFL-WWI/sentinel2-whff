############################################################################
# Predict forest type (coniferous/mixed/deciduous)
#
# Using Sentinel-2 data and a randomForest model trained in Sementina-Gudo TI.
#
# (c) by Dominique Weber, HAFL, BFH

############################################################################

library(raster)
library(rgdal)
library(randomForest)
library(sp)

setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/TBk_Lugano/")

# path (WITH BACKSLASH) and dates for stacks
STACK_PATH = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2017/"
STACK_DATES = c("20170328", "20170815", "20170924", "20171014", "20171024")

# S2 band names and indices for the corresponding raster stacks
BAND_NAMES = c("B2","B3","B4","B5","B6","B7","B8","B8A","B11","B12")
BAND_INDICES = 1:10

TARGET_VALUE_NAME = "Class" # WARNING: CHANGE TARGET VALUE FOR THE RANDOM FOREST MODEL MANUALLY FURTHER DOWN (ca. L142)

#write predicted file to
PREDICTION.FILE = "forest_type_canton_ti.tif"
PROBABILITY.FILE= "forest_type_canton_ti_prob.tif"

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
# PREPARE DATA

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

###########################
# LOAD RF MODEL (from TBk Sementina-Gudo)

rf.mdl = loadRDS("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/TBk_Sementina_Gudo/")

###########################
# RUN PREDICTION

print("Run model prediction")
beginCluster()
rf.prediction <- clusterR(finalStack, predict, args = list(model = rf.mdl, type="response", index=1, na.rm=TRUE, progress="text"))
rf.prediction.prob1 <- clusterR(finalStack, predict, args = list(model = rf.mdl, type="prob", index=1,na.rm=TRUE, progress="text"))
rf.prediction.prob2 <- clusterR(finalStack, predict, args = list(model = rf.mdl, type="prob", index=2,na.rm=TRUE, progress="text"))
rf.prediction.prob3 <- clusterR(finalStack, predict, args = list(model = rf.mdl, type="prob", index=3,na.rm=TRUE, progress="text"))
rf.prediction.prob4 <- clusterR(finalStack, predict, args = list(model = rf.mdl, type="prob", index=4,na.rm=TRUE, progress="text"))

stk = stack(c(rf.prediction.prob1,rf.prediction.prob2,rf.prediction.prob3,rf.prediction.prob4))
rMaxProb = calc(stk, max)
endCluster()

print("Write prediction file")
writeRaster(rf.prediction, PREDICTION.FILE, overwrite=T)
writeRaster(rMaxProb, PROBABILITY.FILE, overwrite=T)

###########################
# ESTIMATE CLASS PROBABILITIES & SAVE OUTPUTS

# Probabilities

x <- predict(rf.mdl, df, type = "prob")
write.csv(x, "class_probabilities.csv")

# other output
png(filename = "map_samples.png", width = 1000, height = 800)
par(mar=c(4,5,3,2))
plotRGB(stackList$stack[[1]][[3:1]], stretch="lin", axes=T)
plot(sdata, add=T, col="red", border="red", lwd=3)
dev.off()

png(filename = "map_prediction.png", width = 1000, height = 800)
plot(rf.prediction)
dev.off()

png(filename = "accuracy.png", width = 500, height = 700)
varImpPlot(rf.mdl, type=1)
dev.off()

capture.output(rf.mdl, file = "stats.txt")