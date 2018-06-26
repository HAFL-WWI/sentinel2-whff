######################################################################
# Change dectection Valle Bedretto (Lawinen, Sturm)
#
# (C) Dominique Weber, HAFL, BFH
######################################################################

# source custom function to build geotiff subsets
source("change_detection/change_detection_bitemporal.R")

# ouput directory for results (PDFs, spatial data)
output_path = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/ValleBedretto/Resultate/"

# Sentinel-2 images
s2_2017 = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2017/S2A_MSIL1C_20170706T102021_N0205_R065_T32TMS_20170706T102301.tif"
s2_2018 = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2018/S2A_MSIL1C_20180614T103021_N0206_R108_T32TMS_20180614T124154.tif"

# forest mask
sp_mask = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/ValleBedretto/valle_bedretto.shp"

# Run change detection
change = change_detection_bitemporal(s2_2017, s2_2018, "NBR", sp_mask, 0.05, output_path, c("2017", "2018"), type_neg=T, type_pos=F)