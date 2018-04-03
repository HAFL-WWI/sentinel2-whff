############################################################
# Generate GeoTIFF raster stacks from Sentinel-2 SAFE format
# Uses this custom function: /mnt/cephfs/data/HAFL/WWI-Sentinel-2/R_demo_Skripts/build_geotiff_from_safe_format.R
#
# SAFE format description: https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/data-formats
#
# by Dominique Weber, HAFL, BFH
# 
############################################################

library(sp)
library(rgdal)
library(raster)

# Dummy area of interest (near Wiliwald)
e = extent(380020.9, 387340.9, 5203050.0, 5208040.9)

# source custom function to build geotiff subsets
source("preprocessing/build_geotiff_from_safe_format.R")

# process two sample Sentinel-2 images
in_dir = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/SAFE/T32TLT/2017/S2A_MSIL1C_20170510T103031_N0205_R108_T32TLT_20170510T103025.SAFE/GRANULE/L1C_T32TLT_A009829_20170510T103025/IMG_DATA/"
stk1 = build_raster_stack(in_dir=in_dir, out_dir=NULL, ext=e, L2A = F)
in_dir = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/SAFE/T32TLT/2017/S2A_MSIL1C_20170619T103021_N0205_R108_T32TLT_20170619T103021.SAFE/GRANULE/L1C_T32TLT_A010401_20170619T103021/IMG_DATA/"
stk2 = build_raster_stack(in_dir=in_dir, out_dir=NULL, ext=e, L2A = F)

# plot examples
par(mfrow=c(1,2))
plotRGB(stk1[[4:2]], stretch="lin", axes=T)
plotRGB(stk2[[4:2]], stretch="lin", axes=T)
par(mfrow=c(1,1))
