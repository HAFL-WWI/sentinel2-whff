############################################################
# Plot images for surselva
#
# by Dominique Weber, HAFL, BFH
############################################################

# load packages
library(sp)
library(rgdal)

# source functions
source("/home/wbd3/sentinel2-whff/data_exploration/plot_functions.R")

# wd
setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Surselva_Geb/")

# global params
band_names = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B10","B11", "B12")
dates_2017_filter = c("201703")
rgb_bands = c("B11", "B08", "B04")
sp = readOGR("Bezirk_Surselva_wgs84.shp")

# T32TMS
images_path = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2017/"
plot_rgb_images(images_path, dates_2017_filter, band_names, c(rgb_bands), sp, "images_2017_t32tms.pdf")

# 2017 T32TNS
images_path = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNS/2017/"
dates_2017_filter = c("20170328",  "20170517", "20170527", "20170626", "20170825", "20171014", "20171103")
plot_rgb_images(images_path, dates_2017_filter, band_names, c(rgb_bands), sp, "images_2017_t32tns.pdf")
