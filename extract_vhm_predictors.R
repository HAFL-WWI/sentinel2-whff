###########################################################################################
# 
# Extract VHM predictors
# (c) by Dominique Weber, HAFL, BFH
#
###########################################################################################

# load libraries
library(raster)

# set working directory
setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Baumarten_BGB/")

# load data
vhm.mean = raster("VHM_data/VHM_mean.tif")
vhm.max = raster("VHM_data/VHM_max.tif")
vhm.sd = raster("VHM_data/VHM_sd.tif")
sp = readOGR("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/MOTI/Trainingsdaten_MOTI_PPSS/20180205_MOTI_PPSS_BGB_buffer6m.shp")

# extract values
e.mean = extract(vhm.mean, sp, mean, df=T)
e.max = extract(vhm.max, sp, mean, df=T)
e.sd = extract(vhm.sd, sp, mean, df=T)

# add to predictor dataset
df = read.csv("spectral_data_l1c_l2a.csv")
df.new = cbind(df,VHM_mean=e.mean$VHM_mean, VHM_max=e.max$VHM_max, VHM_sd=e.sd$VHM_sd)

# write csv
write.csv(df.new, "spectral_data_l1c_l2a_and_VHM.csv", row.names=FALSE)