############################################################
# Apply cloud mask
#
# by Dominique Weber, HAFL, BFH
############################################################

library(raster)

# set wd
setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/")

# load sentinel-2 image
stk.20170818 = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/S2A_MSIL1C_20170818T103021_N0205_R108_T32TLT_20170818T103421.tif")

# load cloud mask (generated from Madlene with fmask algorithm)
c.20170818 = raster("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/fmask/T32TLT/2017/S2A_MSIL1C_20170818T103021_N0205_R108_T32TLT_20170818T103421-cloud40.tif")

# plot Vegetation RGB and dummy area of interest (AOI)
plotRGB(stk.20170818[[c(12,8,4)]], stretch = "lin", axes=T)
e = extent(330073.4,  365649.7, 5194335.1, 5215578.5)
plot(e, add=T, col="red")

# crop image and cloud mask to AOI (just to increase speed for this test area)
stk.20170818 = crop(stk.20170818, e)
c.20170818 = crop(c.20170818, e)

# plot vegetation RGBs
plotRGB(stk.20170818[[c(12,8,4)]], stretch = "lin", axes=T)

# plot cloud masks
plot(c.20170818)

# apply cloud mask (keep only clear land pixels)
clear_pixels = (c.20170818 == 1)
clear_pixels[clear_pixels!=1] = NA
stk.20170818.masked = mask(stk.20170818, clear_pixels)

# plot masked vegetatin RGB
plotRGB(stk.20170818.masked[[c(12,8,4)]], stretch = "lin", colNA='red', axes=T, main="Vegetation-RGB with non-clear pixels shown in red")
