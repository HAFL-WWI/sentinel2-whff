######################################################################
# Change dectection for forest fire in Bremgarten BE
#
# (C) Dominique Weber, HAFL, BFH
######################################################################


# source custom function to build geotiff subsets
source("change_detection/change_detection_bitemporal.R")

# ouput directory for results (PDFs, spatial data)
out = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Waldbrand_Bremgarten/"

# Sentinel-2 images
s2_pre_fire = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/S2A_MSIL1C_20170410T103021_N0204_R108_T32TLT_20170410T103020.tif"
s2_post_fire = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/S2A_MSIL1C_20170430T103021_N0205_R108_T32TLT_20170430T103024.tif" 

# BGB forest mask
sp_mask = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Waldmaske/BGB/fbb_bremgarten.shp"

# Run change detection
change = change_detection_bitemporal(s2_post_fire, s2_pre_fire, "NBR", sp_mask, 0.1, out, c("post_fire", "pre_fire"))
