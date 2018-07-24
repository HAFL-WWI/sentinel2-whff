############################################################################
# Generate training dataset from GeoStat Arealstatistik 2004/09
# 
# (c) by Dominique Weber, HAFL, BFH
############################################################################

library(rgdal)
library(sp)
library(raster)
library(dplyr)


###########################
# USER CONFIG

setwd("/mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Surselva_Geb/")

# load Arealstatistik (cliped to AOI, projected to WGS84)
sp = readOGR("AREA_NOAS04_17_161114_surselva_clip_wgs84.shp")

###########################

# build groups
sp$Class = rep(NA, nrow(sp))
sp$Class[sp$AS09_4 == 1] = "Siedlung"
sp$Class[sp$AS09_4 == 2] = "Landwirtschaft"
sp$Class[sp$AS09_17 == 10] = "Wald"
sp$Class[sp$AS09_17 == 11] = "Gebuesch"
sp$Class[sp$AS09_4 == 4] = "KeineVegetation"
sp = sp[!is.na(sp$Class),]

# select random samples
df.sub = sp@data%>% group_by(Class) %>% sample_n(size = 500)
sp.sub = sp[sp$RELI%in%df.sub$RELI,]

# write shape
writeOGR(sp.sub, ".", "training_data_geostat", driver="ESRI Shapefile")