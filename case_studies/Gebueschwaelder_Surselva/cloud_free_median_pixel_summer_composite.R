############################################################
# Cloud-free summer composite using L2A scene classification.
#
# Mask all clouds, shadows, NAs etc. and create cloud free composite.
# Median pixel values is taken if more than one value is available.
#
# by Dominique Weber, HAFL, BFH
############################################################
library(raster)
library(rgdal)

# wd
setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Surselva_Geb/")

# params
band_names = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B10","B11", "B12")
dates_2017_filter = c("20170619", "20170626", "20170706", "20170719")
sp = readOGR("Bezirk_Surselva_wgs84.shp")

# 2017 T32TMS
images_path = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2017/"
scl_paths = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI2Ap/SAFE/T32TMS/2017/"

# get tifs
fileNames = list.files(images_path)
fileNames = fileNames[grep("tif$", fileNames)]
fileNames = fileNames[grepl(paste(dates_2017_filter, collapse="|"), fileNames)]
fileNames = paste(images_path, fileNames, sep="")

# get scene classification layer (see https://earth.esa.int/web/sentinel/technical-guides/sentinel-2-msi/level-2a/algorithm)
fileNamesScl = list.files(scl_paths, "SCL_20m.jp2", recursive = T)
fileNamesScl = fileNamesScl[grepl(paste(dates_2017_filter, collapse="|"), fileNamesScl)]
fileNamesScl = paste(scl_paths, fileNamesScl, sep="")

# create images stack list
stack_list = list()
for(i in fileNames){
  print(i)
  stk_tmp = crop(stack(i), extent(sp))
  names(stk_tmp) = band_names
  stack_list = c(stack_list, stk_tmp)
}
# write images for documentation
# writeRaster(stack_list[[1]], "20170619.tif")
# writeRaster(stack_list[[2]], "20170626.tif")
# writeRaster(stack_list[[3]], "20170706.tif")
# writeRaster(stack_list[[4]], "20170719.tif")

# create masks stack
masks = stack()
for(i in fileNamesScl){
  print(i)
  scl_tmp = crop(disaggregate(raster(i), 2), extent(sp))
  m_tmp = reclassify(scl_tmp, c(0,3,NA, 3,7,1, 7,10,NA, 10,11,1))
  masks = addLayer(masks, m_tmp)
}
# write masks for documentation
# writeRaster(masks[[1]], "mask_20170619.tif")
# writeRaster(masks[[2]], "mask_20170626.tif")
# writeRaster(masks[[3]], "mask_20170706.tif")
# writeRaster(masks[[4]], "mask_20170719.tif")

# mask images
for(i in 1:length(stack_list)){
  print(paste("mask", i, "from", length(stack_list)))
  stack_list[[i]] = mask(stack_list[[i]], masks[[i]])
}

# calculate cloud-free median pixel composite
nL = nlayers(stack_list[[1]])
stk_composite = stack()
beginCluster()
for(i in 1:nL){
  print(paste(i, "from", nL))
  tmp_stk = stack(sapply(stack_list, function(x) x[[i]]))
  tmp_median <- clusterR(tmp_stk, calc, args=list(fun=median, na.rm=T))
  stk_composite = addLayer(stk_composite, tmp_median)
}
endCluster()

# proper naming
names(stk_composite) = band_names

# plot cloud-free composite RGB
pdf("composite.pdf")
plotRGB(stk_composite[[c("B04","B03","B02")]], stretch="hist")
plotRGB(stk_composite[[c("B11","B08","B02")]], stretch="hist")
dev.off()

# write final composite raster
writeRaster(stk_composite, "composite.tif")