######################################################################
# Change dectection for storm damages Egg ZH summer 2017
#
# (C) Dominique Weber, HAFL, BFH
######################################################################


# source custom function to build geotiff subsets
source("change_detection/change_detection_bitemporal.R")

# ouput directory for results (PDFs, spatial data)
output_path = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Sturm_Pfannenstiel/Resultate_neu2/"

# Sentinel-2 images
r_pre_storm = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C//GeoTIFF/T32TMT/2017/S2A_MSIL1C_20170706T102021_N0205_R065_T32TMT_20170706T102301.tif"
r_post_storm = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMT/2017/S2A_MSIL1C_20170815T102021_N0205_R065_T32TMT_20170815T102513.tif"

# load mask
sp_mask = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Sturm_Pfannenstiel/Perimeter/mask_pfannenstiel.shp"

# Run change detection
change = change_detection_bitemporal(r_post_storm, r_pre_storm, "NBR", sp_mask, 0.07, output_path, c("nach Sturm", "vor Sturm"))
