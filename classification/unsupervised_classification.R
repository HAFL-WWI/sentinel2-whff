###########################################################################################
# 
# Unsupervised classification with kmeans
#
# (c) by Dominique Weber, HAFL, BFH
#
###########################################################################################

require(sp)
require(rgdal)
require(raster)

#################################################################
# START CONFIGURATION PARAMETERS

# ouput directory for results (PDFs, spatial data)
output_path = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/TBk_Furna/"

# load stacks
s2_20170328 = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNT/2017/S2A_MSIL1C_20170328T102021_N0204_R065_T32TNT_20170328T102018.tif")
s2_20170626 = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNT/2017/S2A_MSIL1C_20170626T102021_N0205_R065_T32TNT_20170626T102321.tif")

# load mask
sp_mask = readOGR("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/TBk_Furna/wald_gde_furna_wgs84.shp")

# layer names (see: https://sentinel.geodata.linux.bfh.ch/, Table2)
BAND_NAMES = c("B01","B02","B03","B04","B05","B06","B07","B08","B8A","B09","B10","B11","B12")

# END CONFIGURATION PARAMETERS
#################################################################

# set out path
setwd(output_path)

# name and crop data
names(s2_20170328) = BAND_NAMES
names(s2_20170626) = BAND_NAMES
s2_20170328 = crop(s2_20170328, sp_mask)
s2_20170626 = crop(s2_20170626, sp_mask)

# clip to forest mask
s2_20170328 = mask(s2_20170328, sp_mask)
s2_20170626 = mask(s2_20170626, sp_mask)

# plot
plotRGB(s2_20170328[[4:2]], stretch = "hist")
plotRGB(s2_20170626[[4:2]], stretch = "hist")

# stack all
stk = stack(s2_20170328, s2_20170626)

# returns the values of the raster dataset and write them in a matrix. 
v <- getValues(stk)
i <- which(!is.na(v))
v <- na.omit(v)
km <- kmeans(v, 10, iter.max = 100, nstart = 10)
kmeans_raster <- raster(stk)
kmeans_raster[i] <- km$cluster
plot(kmeans_raster)

# write rasters
writeRaster(s2_20170328, "S2_L1C_20170328.tif")
writeRaster(s2_20170626, "S2_L1C_20170626.tif")
writeRaster(kmeans_raster, "kmeans_10_classes.tif")
