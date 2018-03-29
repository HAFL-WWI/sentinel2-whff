######################################################################
# Map forest changes using bi-temporal Sentinel-2 differences.
#
# (C) Dominique Weber, HAFL, BFH
######################################################################

library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)

#################################################################
# START CONFIGURATION PARAMETERS

# ouput directory for results (PDFs, spatial data)
output_path = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/TBk_Furna/Change_Detection/"

# load stacks
s2_2015 = stack("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Sentinel-2/Furna/L1C_32TNT_20150826_AWS.tif") # 26.08.2015!
# no good cloud free summer scenes for 2016!
# cloud free composite need from 21.07.2016 and evtl. 01.07.2016 and evtl. 30.08.2016 -> but will still be difficult to cover whole area
# s2_2016 = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNT/2016/") # 
# 26.06.2017 also cloud free

s2_2017 = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNT/2017/S2A_MSIL1C_20170825T102021_N0205_R065_T32TNT_20170825T102114.tif")

# load mask
sp_mask = readOGR("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/TBk_Furna/wald_gde_furna_wgs84.shp")

# layer names (see: https://sentinel.geodata.linux.bfh.ch/, Table2)
BAND_NAMES = c("B01","B02","B03","B04","B05","B06","B07","B08","B8A","B09","B10","B11","B12")

# crop
crop_extent = extent(sp_mask)

# END CONFIGURATION PARAMETERS
#################################################################

# name and crop data
names(s2_2015) = BAND_NAMES
names(s2_2017) = BAND_NAMES
s2_2015 = crop(s2_2015, crop_extent)
s2_2017 = crop(s2_2017, crop_extent)

# clip to forest mask
s2_2015 = mask(s2_2015, sp_mask)
s2_2017 = mask(s2_2017, sp_mask)

# plot true colour RGBs
rgb_bands = c("B04", "B03", "B02")
plotRGB(s2_2015[[rgb_bands]], stretch="lin")
plotRGB(s2_2017[[rgb_bands]], stretch="lin")
pdf(file=paste(output_path, "rgb_plots.pdf", sep=""))  
plotRGB(s2_2015[[rgb_bands]], stretch="lin", axes=T, main="2015")
plotRGB(s2_2017[[rgb_bands]], stretch="lin", axes=T, main="2017")
dev.off()

# calculate some indices
s2_2015$NDVI = (s2_2015$B08 - s2_2015$B04)/(s2_2015$B08 + s2_2015$B04)
s2_2015$NBR = (s2_2015$B08 - s2_2015$B12)/(s2_2015$B08 + s2_2015$B12)
s2_2015$NDII = (s2_2015$B08 - s2_2015$B11)/(s2_2015$B08 + s2_2015$B11) # http://www.sciencedirect.com/science/article/pii/S0168192309002251

s2_2017$NDVI = (s2_2017$B08 - s2_2017$B04)/(s2_2017$B08 + s2_2017$B04)
s2_2017$NBR = (s2_2017$B08 - s2_2017$B12)/(s2_2017$B08 + s2_2017$B12)
s2_2017$NDII = (s2_2017$B08 - s2_2017$B11)/(s2_2017$B08 + s2_2017$B11)
# further indices tested: MSI, mNDVI705, IRECI, EVI -> they seem not usefull for forests
# see (https://www.sentinel-hub.com/develop/documentation/eo_products/Sentinel2EOproducts)

# calculate differences
r_diff = s2_2017 - s2_2015

# plot differences within all bands and indices
pdf(file=paste(output_path, "diff_plots.pdf", sep=""))  
for(i in 1:nlayers(r_diff)) plot(r_diff[[i]], main=names(r_diff[[i]]))
dev.off()

# set threshold and map changes
change_positive = r_diff$NBR > 0.1
change_negative = r_diff$NBR < -0.1
plot(change_positive, main= "change positive", col=c("darkolivegreen3", "black"))
plot(change_negative, main= "change negative", col=c("darkolivegreen3", "black"))

# apply median filter
change_positive_3x3 = focal(change_positive, w=matrix(1,3,3), fun=modal, na.rm=T)
change_negative_3x3 = focal(change_negative, w=matrix(1,3,3), fun=modal, na.rm=T)
plot(change_positive_3x3, main= "change positive", col=c("darkolivegreen3", "black"))
plot(change_negative_3x3, main= "change negative", col=c("darkolivegreen3", "black"))

# apply mask again
change_positive_3x3 = mask(change_positive_3x3, sp_mask)
change_negative_3x3 = mask(change_negative_3x3, sp_mask)
plot(change_positive_3x3, main= "change positive", col=c("darkolivegreen3", "black"))
plot(change_negative_3x3, main= "change negative", col=c("darkolivegreen3", "black"))

# create polygons for changed areas
change_positive_poly = rasterToPolygons(change_positive_3x3, fun=function(x){x>0}, dissolve = T)
change_negative_poly = rasterToPolygons(change_negative_3x3, fun=function(x){x>0}, dissolve = T)
changed_areas <- rbind(change_positive_poly, change_negative_poly)
changed_areas@data$change_type[1] = "positive"
changed_areas@data$change_type[2] = "negative"
changed_areas@data$change_type = as.factor(changed_areas@data$change_type)

# calculate area
area_m2 = sum(area(changed_areas))

# plot change
plotRGB(s2_2017[[rgb_bands]], stretch="lin", axes=T, main=paste("Changed area ", "(", area_m2, " m2)", sep=""))
plot(changed_areas, col=c("green","red"), border=NA, add=T)

# plot to PDF
pdf(file=paste(output_path, "changed_area.pdf", sep=""))  
plotRGB(s2_2017[[rgb_bands]], stretch="lin", axes=T, main=paste("Changed area ", "(", area_m2, " m2)", sep=""))
plot(changed_areas, col=c("green","red"), border=NA, add=T)
dev.off()

# write 
writeOGR(changed_areas, output_path, "changed_areas", driver="ESRI Shapefile")

# write Sentinel-2 images
writeRaster(s2_2015, paste(output_path, "Sentinel2_Aug_2015.tif", sep=""))
writeRaster(s2_2017, paste(output_path, "Sentinel2_Aug_2017.tif", sep=""))