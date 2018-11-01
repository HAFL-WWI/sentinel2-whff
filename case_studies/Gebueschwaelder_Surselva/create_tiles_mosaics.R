###########################################################################################
# 
# Mosaic and crop sentinel-2 tiles
#
# COMPUTATION INFO: ca. 7h for 4x2 tiles mosaic
# 
# (c) by Dominique Weber, HAFL, BFH
#
###########################################################################################

# load libraries
library(raster)
library(rgdal)
library(sp)

start_time <- Sys.time()

# set working directory
setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Surselva_Geb/S2_mosaics_new/")

# perimeter
sp = readOGR("Kanton_GR_wgs84.shp")

# load stacks
stk1_tns = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNS/2017/S2A_MSIL1C_20170328T102021_N0204_R065_T32TNS_20170328T102018.tif")
stk1_tms = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2017/S2A_MSIL1C_20170328T102021_N0204_R065_T32TMS_20170328T102018.tif")
stk1_tnt = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNT/2017/S2A_MSIL1C_20170328T102021_N0204_R065_T32TNT_20170328T102018.tif")

# create mosaic & crop to perimeter
stk1_mosaic = merge(stk1_tns, stk1_tms, stk1_tnt)
stk1_mosaic = crop(stk1_mosaic, sp)
stk1_mosaic = mask(stk1_mosaic, sp)

# write mosaic
writeRaster(stk1_mosaic, "S2_mosaic_20170328.tif")

####################
# load stacks
stk2_tns = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNS/2017/S2A_MSIL1C_20170527T102031_N0205_R065_T32TNS_20170527T102301.tif")
stk2_tms = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2017/S2A_MSIL1C_20170527T102031_N0205_R065_T32TMS_20170527T102301.tif")
stk2_tnt = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNT/2017/S2A_MSIL1C_20170527T102031_N0205_R065_T32TNT_20170527T102301.tif")

# create mosaic & crop to perimeter
stk2_mosaic = merge(stk2_tns, stk2_tms, stk2_tnt)
stk2_mosaic = crop(stk2_mosaic, sp)
stk2_mosaic = mask(stk2_mosaic, sp)

# write mosaic
writeRaster(stk2_mosaic, "S2_mosaic_20170527.tif")

####################
# load stacks
stk3_tns = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNS/2017/S2A_MSIL1C_20170626T102021_N0205_R065_T32TNS_20170626T102321.tif")
stk3_tms = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2017/S2A_MSIL1C_20170626T102021_N0205_R065_T32TMS_20170626T102321.tif")
stk3_tnt = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNT/2017/S2A_MSIL1C_20170626T102021_N0205_R065_T32TNT_20170626T102321.tif")

# create mosaic & crop to perimeter
stk3_mosaic = merge(stk3_tns, stk3_tms, stk3_tnt)
stk3_mosaic = crop(stk3_mosaic, sp)
stk3_mosaic = mask(stk3_mosaic, sp)

# write mosaic
writeRaster(stk3_mosaic, "S2_mosaic_20170626.tif")

####################
# load stacks
stk4_tns = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNS/2017/S2A_MSIL1C_20171014T102021_N0205_R065_T32TNS_20171014T102235.tif")
stk4_tms = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2017/S2A_MSIL1C_20171014T102021_N0205_R065_T32TMS_20171014T102235.tif")
stk4_tnt = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNT/2017/S2A_MSIL1C_20171014T102021_N0205_R065_T32TNT_20171014T102235.tif")

# create mosaic & crop to perimeter
stk4_mosaic = merge(stk4_tns, stk4_tms, stk4_tnt)
stk4_mosaic = crop(stk4_mosaic, sp)
stk4_mosaic = mask(stk4_mosaic, sp)

# write mosaic
writeRaster(stk4_mosaic, "S2_mosaic_20171014.tif")

####################
# load stacks
stk5_tns = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNS/2017/S2A_MSIL1C_20171103T102201_N0206_R065_T32TNS_20171106T195236.tif")
stk5_tms = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2017/S2A_MSIL1C_20171103T102201_N0206_R065_T32TMS_20171106T195236.tif")
stk5_tnt = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNT/2017/S2A_MSIL1C_20171103T102201_N0206_R065_T32TNT_20171106T195236.tif")

# create mosaic & crop to perimeter
stk5_mosaic = merge(stk5_tns, stk5_tms, stk5_tnt)
stk5_mosaic = crop(stk5_mosaic, sp)
stk5_mosaic = mask(stk5_mosaic, sp)

# write mosaic
writeRaster(stk5_mosaic, "S2_mosaic_20171103.tif")

print(Sys.time() - start_time)