############################################################################
# Predict forest type for Surselva (GR) - Gebueschwaelder
#
# (c) by Dominique Weber, HAFL, BFH
############################################################################

library(raster)
library(rgdal)
library(randomForest)
library(sp)

# load helper functions
source("//home/wbd3/sentinel2-whff/classification/classification_helper.R")

###########################
# USER CONFIG

setwd("/mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Surselva_Geb/")

# path (WITH BACKSLASH) and dates for stacks
STACK_PATH = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2017/"
STACK_DATES = c("20170328", "20170527", "20170626","20171014")

# path for training data shapefile
SP.PATH = "training_data_geostat.shp"

# study area
EXTENT = extent(readOGR("Bezirk_Surselva_wgs84.shp"))

# S2 band names and indices for the corresponding raster stacks
BAND_NAMES = c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B10","B11","B12")
BAND_INDICES = 1:13

# response variable
TARGET_VALUE_NAME = "Class" # WARNING: CHANGE TARGET VALUE FOR THE RANDOM FOREST MODEL MANUALLY FURTHER DOWN (ca. L142)

# write predicted file to
PREDICTION.FILE = "forest_type.tif"
PROBABILITY.FILE= "forest_type_prob.tif"

###########################
# LOAD SENTINEL-2 IMAGES
stackList = get_stack_list(STACK_PATH, STACK_DATES, BAND_NAMES, BAND_INDICES)
plot_images(stackList, EXTENT)

###########################
# LOAD TRAINING DATA
training_data <- readOGR(dsn = SP.PATH)
training_data@data[,TARGET_VALUE_NAME] = as.factor(training_data@data[,TARGET_VALUE_NAME])

###########################
# EXTRACT TRAINING DATASET
df = extract_time_series(stackList, training_data, TARGET_VALUE_NAME)

# Additional predictors
df2 = extract_raster_values(raster("VHM_10m_max_wgs84.tif"), training_data, TARGET_VALUE_NAME)
df3 = extract_raster_values(raster("DTM_10m_mean_wgs84.tif"), training_data, TARGET_VALUE_NAME)
df4 = extract_raster_values(raster("Slope_10m_mean_wgs84.tif"), training_data, TARGET_VALUE_NAME)
df5 = extract_raster_values(raster("Aspect_10m_mean_wgs84.tif"), training_data, TARGET_VALUE_NAME)
df_all = cbind(df, VHM=df2$VHM_10m_max_wgs84, DEM=df3$DTM_10m_mean_wgs84, SLP=df4$Slope_10m_mean_wgs84, ASP=df5$Aspect_10m_mean_wgs84)
df = df_all[complete.cases(df_all),]

###########################
# RUN RF MODEL
df = do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x),NA)))
rf.mdl <- randomForest(Class ~ ., data=df, ntree=1000, importance=TRUE, na.action=na.omit)
print(rf.mdl)

###########################
# CREATE RASTER STACK FOR PREDICTION
predictionStack = create_prediction_stack(stackList, EXTENT)

# Additional predictors
vhm = crop(raster("VHM_10m_max_wgs84.tif"), extent(predictionStack))
dem = crop(raster("DTM_10m_mean_wgs84.tif"), extent(predictionStack))
slp = crop(raster("Slope_10m_mean_wgs84.tif"), extent(predictionStack))
asp = crop(raster("Aspect_10m_mean_wgs84.tif"), extent(predictionStack))
# TODO: Topographic-Position-Index and Solar-Radiation (and topo-soil-moisture) not tested yet. Could help, but probably not much.

# names
names(vhm) = "VHM"
names(dem) = "DEM"
names(slp) = "SLP"
names(asp) = "ASP"

# add to stack
predictionStack = addLayer(predictionStack, vhm, dem, slp, asp)

###########################
# RUN PREDICTION
print("Run model prediction")
beginCluster()
nClasses = length(rf.mdl$classes)
rf.prediction <- clusterR(predictionStack, predict, args = list(model = rf.mdl, type="response", na.rm=TRUE, progress="text"))
rf.prediction.prob <- clusterR(predictionStack, predict, args = list(model = rf.mdl, type="prob", index=1:nClasses,na.rm=TRUE, progress="text"))
rMaxProb = calc(rf.prediction.prob, max)
endCluster()

# write final rasters
writeRaster(rf.prediction, PREDICTION.FILE, overwrite=T)
writeRaster(rMaxProb, PROBABILITY.FILE, overwrite=T)

###########################
# SOME ADDTIONAL OUTPUTS
write_additional_model_outputs(stackList, training_data, rf.mdl, rf.prediction)