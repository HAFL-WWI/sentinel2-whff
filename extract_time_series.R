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

###########################
# USER CONFIG

# set working directory
setwd("//home/vom1/RF_output/")

# path (WITH BACKSLASH) to raster stacks and dates for stacks
STACK_PATH = "//home/vom1/Sentinel-2_final_selection/"
STACK_DATES = c("20170311","20170410","20170430","20170510","20170619","20170719","20170818","20171007","20171017")

# Sentinel-2 band names and indices for the corresponding raster stacks
BAND_NAMES = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B11", "B12")

# path (WITHOUT BACKSLASH) and name (NO FILE ENDING) of the shapefile
SHAPE.PATH = "//home/vom1/MOTI_Data"
SHAPE.NAME = "20180205_MOTI_PPSS_BGB_buffer6m"

# ID of the locations from shapefile
TARGET_VALUE_NAME = "ID__NAME_"

# if shapefile represents points or lines, set MYFUN to: NULL
# if shapefile represents polygons, set MYFUN to:
# function such as: mean, median to return a summarized value of raster cells covered by a polygon
MYFUN <- mean

# name of the output csv file (WITH .csv SUFFIX)
MYFILE <- "bands_test.csv"



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

plotRGB(stackList$stack[[1]][[3:1]], stretch="lin", axes=T)
plot(sdata, add=T, col="red", border="red", lwd=3)

###########################
# PREPARE DATASET

print("Prepare predictors and response")

df <- data.frame()

for(i in 1:length(stackList$stack)) {
  print(paste("loading stack:", i, "of", length(stackList$stack)))
  
  stk <- stackList$stack[[i]]
  colStart <- stackList$columnStart[[i]]
  
  # extract spectral values per plot and bands
  print("extract spectral values per plot and bands")
  e = extract(stk, sdata, fun=MYFUN, sp=T, na.rm=T)
  
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
# SAVE OUTPUT
write.csv(df, MYFILE, row.names = FALSE)


