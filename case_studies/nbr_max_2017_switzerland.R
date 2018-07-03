############################################################
# Calculate nbr max composite for Switzerland
#
# multi-core processing -> ca. 25min
#
# by Dominique Weber, HAFL, BFH
############################################################

start_time <- Sys.time()

# source functions
source("/home/wbd3/sentinel2-whff/composites/calc_pixel_composites.R")
source("/home/wbd3/sentinel2-whff/vegetation_indices/calc_vegetation_indices.R")

# wd
setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/AnnualComposites/")

# global params
band_names = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B10","B11", "B12")
dates_2017_filter = c("201706", "201707")

# 2017 T32TLT
images_path = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/"
nbr_max_2017_t32tlt = calc_pixel_composites(images_path, band_names, dates_2017_filter, "2017_t32tlt", calc_nbr, max)

# 2017 T32TLS
images_path = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLS/2017/"
nbr_max_2017_t32tls = calc_pixel_composites(images_path, band_names, dates_2017_filter, "2017_t32tls", calc_nbr, max)

# 2017 T32TMT
images_path = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMT/2017/"
nbr_max_2017_t32tmt = calc_pixel_composites(images_path, band_names, dates_2017_filter, "2017_t32tmt", calc_nbr, max)

# 2017 T32TMS
images_path = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2017/"
nbr_max_2017_t32tms = calc_pixel_composites(images_path, band_names, dates_2017_filter, "2017_t32tms", calc_nbr, max)

# 2017 T32TNT
images_path = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNT/2017/"
nbr_max_2017_t32tnt = calc_pixel_composites(images_path, band_names, dates_2017_filter, "2017_t32tnt", calc_nbr, max)

# 2017 T32TNS
images_path = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNS/2017/"
nbr_max_2017_t32tns = calc_pixel_composites(images_path, band_names, dates_2017_filter, "2017_t32tns", calc_nbr, max)

# 2017 T32TMR
images_path = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMR/2017/"
nbr_max_2017_t32tmr = calc_pixel_composites(images_path, band_names, dates_2017_filter, "2017_t32tmr", calc_nbr, max)

# 2017 T31TGM (-> UTM zone 31!)
images_path = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T31TGM/2017/"
nbr_max_2017_t31tgm = calc_pixel_composites(images_path, band_names, dates_2017_filter, "2017_t31tgm", calc_nbr, max)

# Problem different utm zones
# There is no easy solution. We tried projectRaster to UTM32 and to LV95 but then rasters have different origins, resolution or row/col numbers

# The only solution that worked so far is this (but somehow then ArcGIS had problem with the resulting mosaic):
# reproject, extend and resample
# nbr_max_2017_t31tgm_32 = projectRaster(nbr_max_2017_t31tgm, crs=crs(nbr_max_2017_t32tls))
# extend_32tls = extend(nbr_max_2017_t32tls, nbr_max_2017_t31tgm_32)
# nbr_max_2017_t31tgm_32_resampled = resample(nbr_max_2017_t31tgm_32, extend_32tls)
# mosaic all...

# create mosaic
nbr_max_2017_ch = mosaic(nbr_max_2017_t32tlt,
                          nbr_max_2017_t32tls, 
                          nbr_max_2017_t32tmt, 
                          nbr_max_2017_t32tms,
                          nbr_max_2017_t32tnt,
                          nbr_max_2017_t32tns,
                          nbr_max_2017_t32tmr,
                          fun=max, 
                          filename="nbr_max_2017_ch.tif")

# write T31TGM separately
writeRaster(nbr_max_2017_t31tgm, "nbr_max_2017_t31tgm.tif")

print(Sys.time() - start_time)

# TODO
# Addtional tiles - missing on the BFH server!
#
# T32TPT (GR)
# T32TPS (GR)
# T32TLR (VS)

