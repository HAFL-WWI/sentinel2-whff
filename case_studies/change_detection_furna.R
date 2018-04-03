######################################################################
# Change dectection for Furna, using Sentinel-2 data from 2015 and 2017.
#
# (C) Dominique Weber, HAFL, BFH
######################################################################

# source custom function to build geotiff subsets
source("change_detection/change_detection_bitemporal.R")

# ouput directory for results (PDFs, spatial data)
output_path = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/TBk_Furna/Change_Detection/"

# Sentinel-2 images
s2_2015 = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Sentinel-2/Furna/L1C_32TNT_20150826_AWS.tif"
s2_2017 = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNT/2017/S2A_MSIL1C_20170825T102021_N0205_R065_T32TNT_20170825T102114.tif"
# no good cloud free summer scenes for 2016!
# cloud free composite need from 21.07.2016 and evtl. 01.07.2016 and evtl. 30.08.2016 -> but will still be difficult to cover whole area

# forest mask
sp_mask = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/TBk_Furna/wald_gde_furna_wgs84.shp"

# Run change detection
change = change_detection_bitemporal(s2_2015, s2_2017, sp_mask, 0.1, output_path)
plot(change)
