############################################################################
# Crop many Sentinel-2 images using some search filters
# and multi-core processing
#
# (c) by Dominique Weber, HAFL, BFH
############################################################################


# raster libraries
library("rgdal")
library("raster")

# multi-core processing libraries
library(foreach)
library(doParallel)

# load helper functions
source("//home/wbd3/sentinel2-whff/classification/classification_helper.R")

###########################
# USER CONFIG

# working folder
setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/AF-53/")

# Sentinel-2 L1C bandnames 
BAND_NAMES = c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B10","B11","B12")

# filter sentinel-2 images
fileNames = list.files("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2018/", full.names=T)
fileNames = fileNames[grep("R108", fileNames)]
fileNames = fileNames[grep("tif$", fileNames)]

# define area of interest (aoi) to crop images
aoi = extent(c(366000,386000,5195000,5215000))
###########################

# prepare multi-core
print("Prepare multi-core processing...")
start_time <- Sys.time()
cl = makeCluster(detectCores() -1)
registerDoParallel(cl)

# extract spectral values and calculate VIs
foreach(i=1:length(fileNames), .packages = c("raster")) %dopar% {
  stk <- stack(fileNames[i])
  names(stk) = BAND_NAMES
  crop(stk, aoi, filename=paste(substr(basename(fileNames[i]),1,19),"clip.tif",sep="_"))
}

#end cluster
stopCluster(cl)