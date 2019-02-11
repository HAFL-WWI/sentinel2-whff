############################################################################
# Predict shrub types for GR
#
# (c) by Dominique Weber, HAFL, BFH
############################################################################

library(raster)
library(rgdal)
library(randomForest)
library(sp)

#################

# set wd
setwd("//home/wbd3/GebWald/")

# load helper functions
source("R/classification_helper.R")

TARGET_VALUE_NAME = "Class"
PREDICTION.FILE = "forest_type.tif"
PROBABILITY.FILE= "forest_type_prob.tif"

########################################
## Load data

## REFERENCE
training_data <- readOGR("data/reference/old/training_gr_and_dw.shp")
training_data@data[,TARGET_VALUE_NAME] = as.factor(training_data@data[,TARGET_VALUE_NAME])

## SENTINEL-2
# Sentinel-2 band names and subset
DATES = c("20170328", "20170527", "20170626", "20171014")
BAND_INDICES = c(1:13)
BAND_NAMES = c("B01","B02","B03","B04","B05","B06","B07","B08","B08A","B09","B10","B11","B12")
BAND_NAMES = BAND_NAMES[BAND_INDICES]

# load sentinel2 stack
S2_20170328 = stack("data/sentinel2/S2_L1C_mosaic_GR_20170328.tif")[[BAND_INDICES]]
S2_20170527 = stack("data/sentinel2/S2_L1C_mosaic_GR_20170527.tif")[[BAND_INDICES]]
S2_20170626 = stack("data/sentinel2/S2_L1C_mosaic_GR_20170626.tif")[[BAND_INDICES]]
S2_20171014 = stack("data/sentinel2/S2_L1C_mosaic_GR_20171014.tif")[[BAND_INDICES]]

# set names
names(S2_20170328) = paste(DATES[1], BAND_NAMES, sep="_")
names(S2_20170527) = paste(DATES[2], BAND_NAMES, sep="_")
names(S2_20170626) = paste(DATES[3], BAND_NAMES, sep="_")
names(S2_20171014) = paste(DATES[4], BAND_NAMES, sep="_")

# NDVI max composite
ndvi_max = raster("data/sentinel2/S2_L1C_ndvi_max_2017_GR.tif")

## SENTINEL-1
S1 = stack("data/sentinel1/VH_20170302-20170325_GR.tif",
           "data/sentinel1/VH_20170829-20170921_GR.tif",
           "data/sentinel1/VV_20170302-20170325_GR.tif",
           "data/sentinel1/VV_20170829-20170921_GR.tif")
names(S1) = c("VH_20170302-20170325", "VH_20170829-20170921", "VV_20170302-20170325", "VV_20170829-20170921")

## OTHERS
DTM_VHM = stack("data/dtm/dtm_10m_GR.tif",
                "data/dtm/slope_10m_GR.tif",
                "data/dtm/aspect_10m_GR.tif",
                "data/vhm/VHM_LFI_10m_max_GR.tif")
names(DTM_VHM) = c("dtm", "slope", "aspect", "vhm")


# all predictors stack
allPredictors = stack(S2_20170328, S2_20170527, S2_20170626, S2_20171014, S1, DTM_VHM, ndvi_max)

########################################
## EXTRACT TRAINING DATASET

df = extract_raster_values(allPredictors, training_data, TARGET_VALUE_NAME)
df = df[,-2]

########################################
## Random Forest Model
# df = do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x),NA)))
rf.mdl <- randomForest(Class ~ ., data=df, ntree=1000, importance=TRUE, na.action=na.omit)
saveRDS(rf.mdl, "rf.mdl")
print(rf.mdl)
pdf("rf_plot.pdf")
plot(rf.mdl)
dev.off()

###########################
# RUN PREDICTION
print("Run model prediction...")

# surselva_region = extent(readOGR("data/perimeter/Bezirk_Surselva_wgs84.shp"))

# forest type prediction
rf.prediction = predict(allPredictors, rf.mdl, type="response", na.rm=TRUE, progress="text")
writeRaster(rf.prediction, PREDICTION.FILE, overwrite=T)

# probability
nClasses = length(rf.mdl$classes)
rf.prediction.prob = predict(allPredictors, rf.mdl, type="prob", index=1:nClasses, na.rm=TRUE, progress="text")
rMaxProb = calc(rf.prediction.prob, max)
writeRaster(rMaxProb, PROBABILITY.FILE, overwrite=T)

###########################
# SOME ADDTIONAL OUTPUTS
write_additional_model_outputs(allPredictors[[c(3,2,1)]], training_data, rf.mdl, rf.prediction)