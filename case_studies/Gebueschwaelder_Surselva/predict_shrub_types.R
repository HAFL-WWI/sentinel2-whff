############################################################################
# Predict shrub types for GR
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
STACK_PATH = "S2_mosaics_new/"
STACK_DATES = c("20170328", "20170527", "20170626","20171014")

# path for training data shapefile
SP.PATH = "training_gr_and_dw.shp"

# study area
# EXTENT = extent(readOGR("Bezirk_Surselva_wgs84.shp"))

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
plot_images(stackList)

###########################
# LOAD ADDITIONAL PREDICTORS
rVHM = raster("VHM/VHM_LFI_10m_max_wgs84.tif")
rDEM = raster("DEM_Variables/GR_dtm_wgs84.tif")
rSLOPE = raster("DEM_Variables/GR_slope_wgs84.tif")
rASPECT = raster("DEM_Variables/GR_aspect_wgs84.tif")
s1Stack = stack("S1_mosaics/VH_20170302-20170325_GR_wgs84.tif",
                "S1_mosaics/VH_20170829-20170921_GR_wgs84.tif",
                "S1_mosaics/VV_20170302-20170325_GR_wgs84.tif",
                "S1_mosaics/VV_20170829-20170921_GR_wgs84.tif")

###########################
# LOAD TRAINING DATA
training_data <- readOGR(dsn = SP.PATH)
training_data@data[,TARGET_VALUE_NAME] = as.factor(training_data@data[,TARGET_VALUE_NAME])

# TODO Select study area training data
training_data = crop(training_data, extent(stack(stackList[[1]])))

###########################
# EXTRACT TRAINING DATASET
df = extract_time_series(stackList, training_data, TARGET_VALUE_NAME)

# additional NDVI metrices
df_sub = df[,grep("_NDVI",names(df))]
df$NDVI_SD = apply(df_sub, 1, FUN=sd)

# Additional predictors
dfVHM = extract_raster_values(rVHM, training_data, TARGET_VALUE_NAME)
dfDEM = extract_raster_values(rDEM, training_data, TARGET_VALUE_NAME)
dfSLOPE = extract_raster_values(rSLOPE, training_data, TARGET_VALUE_NAME)
dfASPECT = extract_raster_values(rASPECT, training_data, TARGET_VALUE_NAME)
dfS1 = extract_raster_values(s1Stack, training_data, TARGET_VALUE_NAME)
df_all = cbind(df, VHM=dfVHM$VHM_LFI_10m_max_wgs84, DEM=dfDEM$GR_dtm_wgs84, SLP=dfSLOPE$GR_slope_wgs84, ASP=dfASPECT$GR_aspect_wgs84,
               vhmarch=dfS1$VH_20170302.20170325_GR_wgs84, vhsept=dfS1$VH_20170829.20170921_GR_wgs84, 
               vvmarch=dfS1$VV_20170302.20170325_GR_wgs84, vvsept=dfS1$VV_20170829.20170921_GR_wgs84)
df = df_all[complete.cases(df_all),]

# caret visualisation
library(caret)
# df_sub = df[df$Class =="NH" | df$Class =="LH" | df$Class =="Lae", ]
df_sub = df[df$Class =="AErl" | df$Class =="Hasel" | df$Class =="LFoe" | df$Class =="NH" | df$Class =="LH" | df$Class =="Lae", ]
df_sub$Class = as.factor(as.character(df_sub$Class))
pdf("caret.pdf")
featurePlot(x = df_sub[,c(76,83,80,74)], y = df_sub$Class,  plot = "pairs", auto.key = list(columns = 3))
featurePlot(x = df_sub[,-1], y = df_sub$Class, plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(5,1 ), 
            auto.key = list(columns = 2))
dev.off()
###########################
# RUN RF MODEL
df = do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x),NA)))
rf.mdl <- randomForest(Class ~ ., data=df, ntree=1000, importance=TRUE, na.action=na.omit)
saveRDS(rf.mdl, "rf.mdl")
print(rf.mdl)

###########################
# CREATE RASTER STACK FOR PREDICTION

# TODO Test area Surselva
EXTENT = extent(readOGR("Kanton_GR_wgs84.shp"))

predictionStack = create_prediction_stack(stackList, EXTENT)

# additional NDVI metrices
stk_sub = predictionStack[[grep("_NDVI",names(predictionStack))]]
predictionStack$NDVI_SD = calc(stk_sub, sd)

# Additional predictors
rVHM = crop(rVHM, extent(predictionStack))
rDEM = crop(rDEM, extent(predictionStack))
rSLOPE = crop(rSLOPE, extent(predictionStack))
rASPECT = crop(rASPECT, extent(predictionStack))
s1Stack =  crop(s1Stack, extent(predictionStack))

# names
names(rVHM) = "VHM"
names(rDEM) = "DEM"
names(rSLOPE) = "SLP"
names(rASPECT) = "ASP"
names(s1Stack) = c("vhmarch", "vhsept", "vvmarch", "vvsept")

# add to stack
predictionStack = addLayer(predictionStack, rVHM, rDEM, rSLOPE, rASPECT, s1Stack)

# Test if everythin calculated and sorted correctly...
# test = extract(predictionStack, training_data[111,], median, rm.na=T)

###########################
# RUN PREDICTION
print("Run model prediction")
# beginCluster()

# rf.prediction <- clusterR(predictionStack, predict, args = list(model = rf.mdl, type="response", na.rm=TRUE, progress="text"))
rf.prediction = predict(predictionStack, rf.mdl, type="response", na.rm=TRUE, progress="text")
writeRaster(rf.prediction, PREDICTION.FILE, overwrite=T)

# rf.prediction.prob <- clusterR(predictionStack, predict, args = list(model = rf.mdl, type="prob", index=1:nClasses,na.rm=TRUE, progress="text"))
nClasses = length(rf.mdl$classes)
rf.prediction.prob = predict(predictionStack, rf.mdl, type="prob", index=1:nClasses, na.rm=TRUE, progress="text")
rMaxProb = calc(rf.prediction.prob, max)
writeRaster(rMaxProb, PROBABILITY.FILE, overwrite=T)
# endCluster()


###########################
# SOME ADDTIONAL OUTPUTS
write_additional_model_outputs(stackList, training_data, rf.mdl, rf.prediction)
