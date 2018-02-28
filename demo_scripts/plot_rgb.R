############################################################
# Plot some Sentinel-2 RGB images from the server
#
# by Dominique Weber, HAFL, BFH
# 
############################################################

library(raster)

# set wd
setwd("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/")

# load raster stack
stk = stack("GeoTIFF/T32TLT/2017/S2A_MSIL1C_20170619T103021_N0205_R108_T32TLT_20170619T103021.tif")

# plot true color RGB
plotRGB(stk[[4:2]], stretch = "lin")

# plot vegetation RGB
plotRGB(stk[[c(12,8,4)]], stretch = "lin")

# plot detail of some area (Wiliwald)
e = extent(380020.9, 387340.9, 5203050.0, 5208040.9)
plotRGB(stk[[c(12,8,4)]], stretch = "lin", ext=e)
