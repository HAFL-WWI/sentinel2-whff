###########################################################################################
# 
# Extract values of bands from raster objects at the locations of spatial data (shapefile)
#
# (c) by Dominique Weber, HAFL, BFH
#
# adapted by Michaela Volekova, HAFL, BFH
#
###########################################################################################

# load libraries
library(raster)
library(rgdal)
library(sp)
library(foreach)
library(doParallel)

start_time <- Sys.time()

###########################
# USER CONFIG

# set working directory
setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Baumarten_BGB/")

# path (WITH BACKSLASH) to raster stacks and dates for stacks
STACK_PATH = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI2Ap/GeoTIFF/T32TLT/2017/"
STACK_DATES = c("20170410","20170430","20170510","20170619","20170719","20170818","20171007","20171017")

# Sentinel-2 band names and indices for the corresponding raster stacks
BAND_NAMES = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B11", "B12")

# path (WITHOUT BACKSLASH) and name (NO FILE ENDING) of the shapefile
SHAPE.PATH = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/MOTI/Trainingsdaten_MOTI_PPSS/"
SHAPE.NAME = "20180205_MOTI_PPSS_BGB_buffer6m"

# ID of the locations from shapefile
TARGET_VALUE_NAME = "BAUMART"

# if shapefile represents points or lines, set MYFUN to: NULL
# if shapefile represents polygons, set MYFUN to:
# function such as: mean, median to return a summarized value of raster cells covered by a polygon
MYFUN <- mean

# name of the output csv file (WITH .csv SUFFIX)
MYFILE <- "spectral_data_l2a.csv"

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
  stackList$stack[[i]] = stackList$stack[[i]][[1:length(BAND_NAMES)]]
  # name stacks
  names(stackList$stack[[i]]) = c(BAND_NAMES)
}

###########################
# LOAD TRAINING DATA

print("Loading training data...")
# sdata <- readOGR(dsn = SHAPE.PATH, layer = SHAPE.NAME) # on Windows
SP.PATH = paste(SHAPE.PATH, "/", SHAPE.NAME, ".shp", sep="") # on Linux
sdata <- readOGR(dsn = SP.PATH) # on Linux
sdata@data[,TARGET_VALUE_NAME] = as.factor(sdata@data[,TARGET_VALUE_NAME])

plotRGB(stackList$stack[[1]][[c("B04","B03","B02")]], stretch="lin", axes=T)
plot(sdata, add=T, col="red", border="red", lwd=3)

###########################
# PREPARE DATASET

print("Prepare predictors and response")

# prepare multi-core
# TODO Paralellisation could be improved if all stacks are in one list and not separately for each time
# TODO No solution found yet to print out progress of foreach loop
cl = makeCluster(detectCores() -1)
registerDoParallel(cl)
iterations <- length(stackList$stack)

# start raster value extraction
print(paste("processing", iterations, "stacks parallel"))
df <- data.frame()
df = foreach(i=1:iterations, .packages = c("raster"), .combine = "cbind") %dopar% {
  stk <- stackList$stack[[i]]
  colStart <- stackList$columnStart[[i]]
  
  # extract spectral values
  print("extract spectral values...")
  df_tmp = extract(stk, sdata, fun=MYFUN, na.rm=T, df=T)[,-1]
  colnames(df_tmp) = paste(colStart, BAND_NAMES, sep = "")

  # copy target variable only in first subset
  if (i==1) {
    df_tmp = cbind(sdata@data[,TARGET_VALUE_NAME], df_tmp)
    colnames(df_tmp)[1] = TARGET_VALUE_NAME
  }
  return(df_tmp)
}

#end cluster
stopCluster(cl)

end_time <- Sys.time()
end_time - start_time

# remove "X"
# TODO Where is "X" coming from?!
colnames(df) = gsub("^X", "",  colnames(df))

###########################
# SAVE OUTPUT
write.csv(df, MYFILE, row.names = FALSE)