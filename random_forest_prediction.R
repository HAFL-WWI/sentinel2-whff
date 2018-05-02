############################################################################
# 
# Predict tree species composition using multi-temporal sentinel-2 data and 
# a random forest model with image interpretation training data.
#
# (c) by Dominique Weber, HAFL
#
# adapted by Michaela Volekova, HAFL
#
############################################################################

library(raster)
library(rgdal)
library(randomForest)
library(sp)

###########################
# USER CONFIG

# working directory
setwd("//home/vom1/RF_output/")

# path (WITH BACKSLASH) and dates for stacks
STACK_PATH = "//home/vom1/Sentinel-2_final_selection/"
STACK_DATES = c("20170311","20170410","20170430","20170510","20170619","20170719","20170818","20171007","20171017")

# path (WITHOUT BACKSLASH) and name (NO FILE ENDING) for training data
SHAPE.PATH = "//home/vom1/MOTI_Data"
SHAPE.NAME = "20180205_MOTI_PPSS_BGB_buffer6m"


# S2 band names and indices for the corresponding raster stacks
BAND_NAMES = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B11", "B12")
BAND_INDICES = 1:12

# response variable
TARGET_VALUE_NAME = "BAUMART" 

# write predicted file and probability file to
PREDICTION.FILE = "Baumarten_BGB.tif"
PROBABILITY_MAX.FILE = "Baumarten_BGB_prob_max.tif"

# Load the model
load("myrf.RData")


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
SP.PATH = paste(SHAPE.PATH, "/", SHAPE.NAME, ".shp", sep="")
# sdata <- readOGR(dsn = SHAPE.PATH, layer = SHAPE.NAME) # Windows
sdata <- readOGR(dsn = SP.PATH)                          # Linux
sdata@data[,TARGET_VALUE_NAME] = as.factor(sdata@data[,TARGET_VALUE_NAME])

plotRGB(stackList$stack[[1]][[3:1]], stretch="lin", axes=T)
plot(sdata, add=T, col="red", border="red", lwd=3)

###########################
# RUN RF PREDICTION

# prepare data
print("Prepare raster data for prediction")
finalStack <- NA

for(i in 1:length(stackList$stack)) {
  print(paste("Creating stack for prediction:", i, "of", length(stackList$stack)))
  
  pred <- stackList$stack[[i]]
  colStart <- stackList$columnStart[[i]]
  
  names(pred) = c(BAND_NAMES)
  
  # calculate vegetation indices
  pred$NDVI = (pred$B08 - pred$B04)/(pred$B08 + pred$B04)
  pred$IRECI = (pred$B07-pred$B04)/(pred$B05/pred$B06)
  pred$MCARI = ((pred$B05-pred$B04)-0.2*(pred$B05-pred$B03))*(pred$B05-pred$B04)
  pred$GNDVI = (pred$B07-pred$B03)/(pred$B07+pred$B03)

  # add prefix to every column
  names(pred) = paste(colStart, names(pred), sep = "")
  
  #add stack to final stack
  if (i == 1) 
    finalStack = pred
  else
    finalStack = stack(finalStack, pred)
}

# run the prediction (function predict from the raster package)
print("Run model prediction")
rf.prediction <- predict(finalStack, rf.mdl, type="response", na.rm=TRUE)
writeRaster(rf.prediction, PREDICTION.FILE, overwrite=T)

# calculate probability for each of the classes/species separately (function predict from the raster package), than save maximum value of the classes only
# how hight is the probability of the selected class (the class with highest probability)
stk <- NULL
for (i in 1:length(levels(rf.mdl$predicted))) {
  rf.prediction.prob <- predict(finalStack, rf.mdl, type="prob", index=i, na.rm=FALSE)

  if (i == 1)
    stk = rf.prediction.prob
  else
    stk = stack(stk, rf.prediction.prob) 
}

rMaxProb = calc(stk, max)
writeRaster(rMaxProb, PROBABILITY_MAX.FILE, overwrite=T)

###########################
# SAVE OUTPUTS

# output of the random forest run
capture.output(rf.mdl, file = "stats.txt")

# plots
png(filename = "map_samples.png", width = 1000, height = 800)
par(mar=c(4,5,3,2))
plotRGB(stackList$stack[[1]][[3:1]], stretch="lin", axes=T)
plot(sdata, add=T, col="red", border="red", lwd=3)
dev.off()

png(filename = "map_prediction.png", width = 1000, height = 800)
plot(rf.prediction)
dev.off()

png(filename = "map_probability.png", width = 1000, height = 800)
plot(rMaxProb)
dev.off()

png(filename = "variable_importance.png", width = 850, height = 850)
varImpPlot(rf.mdl, type=1, main="", cex=2)
dev.off()

png(filename = "variable_importance_highest10.png", width = 850, height = 850)
varImpPlot(rf.mdl, type=1, main="", n.var=10, cex=2)
dev.off()

