######################################################################
# Change dectection for storm Trubschachen  BE summer 2017
#
# (C) Dominique Weber, HAFL, BFH
######################################################################


# source custom function to build geotiff subsets
source("change_detection/change_detection_bitemporal.R")

# ouput directory for results (PDFs, spatial data)
output_path = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Sturm_Trubschachen/Resultate/"

# Sentinel-2 images
r_pre_storm = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C//GeoTIFF/T32TMT/2017/S2A_MSIL1C_20170719T103021_N0205_R108_T32TMT_20170719T103023.tif"
r_post_storm = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMT/2017/S2A_MSIL1C_20170818T103021_N0205_R108_T32TMT_20170818T103421.tif"

# load mask
sp_mask = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Sturm_Trubschachen/wald_mask_trubschachen_wgs84.shp"

# Run change detection
change = change_detection_bitemporal(r_pre_storm, r_post_storm, "NBR", sp_mask, 0.07, output_path, c("Vor Sturm", "Nach Sturm"))