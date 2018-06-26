######################################################################
# Map forest change using bi-temporal Sentinel-2 differences.
#
# (C) Dominique Weber, HAFL, BFH
######################################################################

# Load packages
library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)

# Custom change detection function
change_detection_bitemporal <- function(date1, date2, band, aoi, threshold, out_dir, dates_text, min_area=10, type_neg=T, type_pos=T) {
  # create out dir
  dir.create(out_dir, showWarnings = T, recursive = T)
  
  # load Sentinel-2 image stacks
  print("Loading data...")
  r_date1 = stack(date1)
  r_date2 = stack(date2)
  
  # load mask for area of interest
  sp_mask = readOGR(aoi)

  # set proper band names 
  print("Preparing data...")
  # layer names (see: https://sentinel.geodata.linux.bfh.ch/, Table2)
  BAND_NAMES = c("B01","B02","B03","B04","B05","B06","B07","B08","B8A","B09","B10","B11","B12")
  names(r_date1) = BAND_NAMES
  names(r_date2) = BAND_NAMES
  
  # crop and mask sentinel-2 images to area of interest
  crop_extent = extent(sp_mask)
  r_date1 = crop(r_date1, crop_extent)
  r_date2 = crop(r_date2, crop_extent)
  r_date1 = mask(r_date1, sp_mask)
  r_date2 = mask(r_date2, sp_mask)
  
  # plot true colour RGBs to PDF
  rgb_bands = c("B04", "B03", "B02")
  pdf(file=paste(out_dir, "rgb_plots.pdf", sep=""))  
  plotRGB(r_date1[[rgb_bands]], stretch="lin", axes=T, main=dates_text[1])
  plotRGB(r_date2[[rgb_bands]], stretch="lin", axes=T, main=dates_text[2])
  dev.off()
  
  # calculate some vegetation indices
  # NDII -> http://www.sciencedirect.com/science/article/pii/S0168192309002251
  r_date1$NDVI = (r_date1$B08 - r_date1$B04)/(r_date1$B08 + r_date1$B04)
  r_date1$NBR = (r_date1$B08 - r_date1$B12)/(r_date1$B08 + r_date1$B12)
  r_date1$NDII = (r_date1$B08 - r_date1$B11)/(r_date1$B08 + r_date1$B11) 
  r_date2$NDVI = (r_date2$B08 - r_date2$B04)/(r_date2$B08 + r_date2$B04)
  r_date2$NBR = (r_date2$B08 - r_date2$B12)/(r_date2$B08 + r_date2$B12)
  r_date2$NDII = (r_date2$B08 - r_date2$B11)/(r_date2$B08 + r_date2$B11)
  # further indices tested: MSI, mNDVI705, IRECI, EVI -> they seem not usefull for forests
  # see (https://www.sentinel-hub.com/develop/documentation/eo_products/Sentinel2EOproducts)
  
  # calculate differences
  print("Calculate differences and detect change...")
  r_diff = r_date2 - r_date1
  
  # plot differences for all bands and indices to PDF
  pdf(file=paste(out_dir, "diff_plots.pdf", sep=""))  
  for(i in 1:nlayers(r_diff)) plot(r_diff[[i]], main=names(r_diff[[i]]))
  dev.off()
  
  # set threshold and map changes
  change_positive = r_diff[[band]] > threshold
  change_negative = r_diff[[band]] < -threshold

  # apply median filter (to remove isolated pixels)
  change_positive_3x3 = focal(change_positive, w=matrix(1,3,3), fun=modal, na.rm=T)
  change_negative_3x3 = focal(change_negative, w=matrix(1,3,3), fun=modal, na.rm=T)

  # apply mask again
  change_positive_3x3 = mask(change_positive_3x3, sp_mask)
  change_negative_3x3 = mask(change_negative_3x3, sp_mask)

  # create polygons for changed areas
  print("Create shapefile...")
  change_positive_poly = rasterToPolygons(change_positive_3x3, fun=function(x){x>0}, dissolve = T)
  change_negative_poly = rasterToPolygons(change_negative_3x3, fun=function(x){x>0}, dissolve = T)
  changed_areas <- rbind(change_positive_poly, change_negative_poly)
  changed_areas@data$change[1] = "positive"
  changed_areas@data$change[2] = "negative"
  changed_areas@data$change = as.factor(changed_areas@data$change)
  
  # disaggregate and calculate mean change per polygon
  changed_areas = disaggregate(changed_areas)
  changed_areas$area_m2 = area(changed_areas)
  changed_areas = changed_areas[changed_areas$area_m2 >= min_area, ]
  changed_areas$mean = as.numeric(extract(r_diff[[band]], changed_areas, mean))

  
  # Filter negative or positive change
  if (!type_neg){
    changed_areas = changed_areas[changed_areas$change != "negative",]}
  if (!type_pos){
    changed_areas = changed_areas[changed_areas$change != "positive",]}
  
  # FINAL PLOTS -> PDFs
  print("Final plot and write data...")
  pdf(file=paste(out_dir, "changed_area.pdf", sep=""))  
  plotRGB(r_date2[[rgb_bands]], stretch="lin", axes=T, main=paste("Changed area ", dates_text[1], " - ", dates_text[2], " (", sum(changed_areas$area_m2), " m2)", sep=""))
  plot(changed_areas, col=c("red","green")[as.numeric(changed_areas$change)], border=NA, add=T)
  legend("bottomright", legend=c("Positive change", "Negative change"), col=c("green", "red"), lwd=2.5, cex=0.7)
  dev.off()

  print("Overview plot...")
  pdf(file=paste(out_dir, "change_overview.pdf", sep="")) 
  plotRGB(r_date1[[rgb_bands]], stretch="lin", axes=T, main=dates_text[1])
  plotRGB(r_date2[[rgb_bands]], stretch="lin", axes=T, main=dates_text[2])
  plotRGB(r_date2[[rgb_bands]], stretch="lin", axes=T, main="Change")
  plot(changed_areas, col=c("red","green")[as.numeric(changed_areas$change)], border=NA, add=T)
  plotRGB(r_date2[[rgb_bands]], stretch="lin", axes=T, main=paste(band, "Diff"))
  plot(r_diff[[band]], axes=T, add=T)
  dev.off()
  
  # write diff raster
  writeRaster(r_diff[[band]], paste(out_dir, "diff_", names(r_diff[[band]]), ".tif",  sep=""))

  # write shapefile
  writeOGR(changed_areas, out_dir, "changed_areas", driver="ESRI Shapefile")
  
  # write Sentinel-2 images
  writeRaster(r_date1, paste(out_dir, "Sentinel2_image_", dates_text[1], ".tif", sep=""))
  writeRaster(r_date2, paste(out_dir, "Sentinel2_image_", dates_text[2], ".tif", sep=""))

  return(changed_areas)
}