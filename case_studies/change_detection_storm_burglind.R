######################################################################
# Change dectection for storm damages Burglind BGB
#
# (C) Dominique Weber, HAFL, BFH
######################################################################


# source custom function to build geotiff subsets
source("change_detection/change_detection_bitemporal.R")

# ouput directory for results (PDFs, spatial data)
output_path = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Sturm_Burglind/Resultate/"

# Sentinel-2 images
# TODO: Change image dates -> needs to be later in season
r_pre_storm = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C//GeoTIFF/T32TLT/2017/S2A_MSIL1C_20170510T103031_N0205_R108_T32TLT_20170510T103025.tif"
r_post_storm = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2018/S2B_MSIL1C_20180420T103019_N0206_R108_T32TLT_20180420T114307.tif"

# load mask
sp_mask = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Sturm_Burglind/mask_kb.shp"

# Run change detection
change = change_detection_bitemporal(r_pre_storm, r_post_storm, "B11", sp_mask, 400, output_path, c("Vor Sturm", "Nach Sturm"))
