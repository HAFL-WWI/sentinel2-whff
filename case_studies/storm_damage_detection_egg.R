######################################################################
# Detect storm damages with Sentinel-2.
#
# Method used here is similar to: http://www.sciencedirect.com/science/article/pii/S0168192309002251#fig4
# (C) Dominique Weber, HAFL, BFH
######################################################################

library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)

#################################################################
# START CONFIGURATION PARAMETERS

# ouput directory for results (PDFs, spatial data)
output_path = "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Sturm_Pfannenstiel/Resultate/"

# load stacks
r_pre_storm = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI2Ap/GeoTIFF/T32TMT/2017/S2A_MSIL2A_20170706T102021_N0205_R065_T32TMT_20170706T102301.tif")
r_post_storm = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI2Ap/GeoTIFF/T32TMT/2017/S2A_MSIL2A_20170815T102021_N0205_R065_T32TMT_20170815T102513.tif")

# load mask
sp_mask = readOGR("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Sturm_Pfannenstiel/Perimeter/mask_pfannenstiel.shp")

# layer names (see: https://sentinel.geodata.linux.bfh.ch/, Table2)
BAND_NAMES = c("B01","B02","B03","B04","B05","B06","B07","B08","B8A","B09","B11","B12")

# crop
crop_extent = c(472000, 477000, 5237000, 5241000)

# END CONFIGURATION PARAMETERS
#################################################################

# name and crop data
names(r_pre_storm) = BAND_NAMES
names(r_post_storm) = BAND_NAMES
r_pre_storm = crop(r_pre_storm, crop_extent)
r_post_storm = crop(r_post_storm, crop_extent)

# clip to forest mask
r_pre_storm = mask(r_pre_storm, sp_mask)
r_post_storm = mask(r_post_storm, sp_mask)

# plot
rgb_bands = c("B04", "B03", "B02")
plotRGB(r_pre_storm[[rgb_bands]], stretch="lin")
plotRGB(r_post_storm[[rgb_bands]], stretch="lin")
pdf(file=paste(output_path, "rgb_plots.pdf", sep=""))  
plotRGB(r_pre_storm[[rgb_bands]], stretch="lin", axes=T, main="Pre storm")
plotRGB(r_post_storm[[rgb_bands]], stretch="lin", axes=T, main="Post storm")
dev.off()

# calculate some indices
r_pre_storm$NDVI = (r_pre_storm$B08 - r_pre_storm$B04)/(r_pre_storm$B08 + r_pre_storm$B04)
r_pre_storm$NBR = (r_pre_storm$B08 - r_pre_storm$B12)/(r_pre_storm$B08 + r_pre_storm$B12)
r_pre_storm$NDII = (r_pre_storm$B08 - r_pre_storm$B11)/(r_pre_storm$B08 + r_pre_storm$B11) # http://www.sciencedirect.com/science/article/pii/S0168192309002251
r_post_storm$NDVI = (r_post_storm$B08 - r_post_storm$B04)/(r_post_storm$B08 + r_post_storm$B04)
r_post_storm$NBR = (r_post_storm$B08 - r_post_storm$B12)/(r_post_storm$B08 + r_post_storm$B12)
r_post_storm$NDII = (r_post_storm$B08 - r_post_storm$B11)/(r_post_storm$B08 + r_post_storm$B11)

# calculate differences
r_diff_storm = r_pre_storm - r_post_storm

# plot differences between pre and post storm values
pdf(file=paste(output_path, "pre_post_storm_diff.pdf", sep=""))  
for(i in 1:nlayers(r_diff_storm)) plot(r_diff_storm[[i]], main=names(r_diff_storm[[i]]))
dev.off()

# DRAW SAMPLES FOR HISTOGRAMS ...
# Density plot for threshold estimation and storm classification
# take sample, covering storm and non-storm areas (50 / 50 if possible)
plot(r_diff_storm$B11)
print("Draw line by clicking twice, and then press Finish. Line should cover 50% storm and 50% no-storm.")
sp_all = drawLine()
print("Draw line by clicking twice, and then press Finish. Line should cover only storm.")
sp_storm = drawLine()
print("Draw line by clicking twice, and then press Finish. Line should cover no-storm areas.")
sp_no_storm = drawLine()

# B11 -> seems to work very well!!
# extract values
v_all = extract(r_diff_storm$B11, sp_all)[[1]]
v_all[is.na(v_all)] = 0
v_storm = extract(r_diff_storm$B11, sp_storm)[[1]]
v_storm[is.na(v_storm)] = 0
v_no_storm= extract(r_diff_storm$B11, sp_no_storm)[[1]]
v_no_storm[is.na(v_no_storm)] = 0

# plot densities
d_all = data.frame(v=v_all, group="all")
d_storm = data.frame(v=v_storm, group="storm")
d_no_storm = data.frame(v=v_no_storm, group="no storm")
dat = rbind(d_all, d_storm, d_no_storm)
ggplot(dat, aes(v, fill=group, colour=group)) +
  scale_x_continuous(breaks = round(seq(min(dat$v), max(dat$v), length.out=10), 0)) +
  geom_density(alpha=0.5, lwd=1, adjust=1) +
  ggtitle("B11")

# Automatic threshold or manually input value
threshold = 12
print(paste("Threshold:", threshold))
storm_classes = r_diff_storm$B11 < threshold
plot(storm_classes, main= paste("B11 <", threshold), col=c("darkolivegreen3", "black"))

# Apply median filter
storm_classes_3x3<-focal(storm_classes, w=matrix(1,3,3), fun=modal, na.rm=T)
plot(storm_classes_3x3, main= paste("B11 <", threshold), col=c("darkolivegreen3", "black"))

# Apply mask to reduce edge effect
new_mask <- gBuffer(sp_mask, width=-10, byid=TRUE) 
storm_classes_3x3 = mask(storm_classes_3x3, new_mask)
plot(storm_classes_3x3, main= paste("B11 <", threshold), col=c("darkolivegreen3", "black"))

# create polygons for damaged areas
damaged_areas = rasterToPolygons(storm_classes_3x3, fun=function(x){x>0}, dissolve = T)
area_m2 = area(damaged_areas)
plotRGB(r_post_storm[[rgb_bands]], stretch="lin", axes=T, main=paste("Strom damage areas ", "(", area_m2, " m2)", sep=""))
plot(damaged_areas, add=T, border="red")
writeOGR(damaged_areas, output_path, "damaged_areas", driver="ESRI Shapefile")

# print as PDF
pdf(file=paste(output_path, "strom_damage_areas.pdf", sep=""))  
plotRGB(r_post_storm[[rgb_bands]], stretch="lin", axes=T, main="Strom damage areas")
plot(damaged_areas, add=T, border="red")
dev.off()


# ...change detection could also be performed with NDVI, NBR or NDII. But B11 seems to work well in this case.


