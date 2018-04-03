######################################################################
# Map forest changes using bi-temporal Sentinel-2 differences.
#
# (C) Dominique Weber, HAFL, BFH
######################################################################

library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)


change_detection_bitemporal <- function(date1, date2, aoi, threshold, out_dir=NULL) {

  print("Loading data...")
  # load stacks
  r_date1 = stack(date1)
  r_date2 = stack(date2)
  
  # load mask
  sp_mask = readOGR(aoi)
  
  # layer names (see: https://sentinel.geodata.linux.bfh.ch/, Table2)
  BAND_NAMES = c("B01","B02","B03","B04","B05","B06","B07","B08","B8A","B09","B10","B11","B12")
  
  # crop
  crop_extent = extent(sp_mask)
  
  print("Preparing data...")
  # name and crop data
  names(r_date1) = BAND_NAMES
  names(r_date2) = BAND_NAMES
  r_date1 = crop(r_date1, crop_extent)
  r_date2 = crop(r_date2, crop_extent)
  
  # clip to forest mask
  r_date1 = mask(r_date1, sp_mask)
  r_date2 = mask(r_date2, sp_mask)
  
  # plot true colour RGBs
  rgb_bands = c("B04", "B03", "B02")
  pdf(file=paste(out_dir, "rgb_plots.pdf", sep=""))  
  plotRGB(r_date1[[rgb_bands]], stretch="lin", axes=T, main="2015")
  plotRGB(r_date2[[rgb_bands]], stretch="lin", axes=T, main="2017")
  dev.off()
  
  # calculate some indices
  r_date1$NDVI = (r_date1$B08 - r_date1$B04)/(r_date1$B08 + r_date1$B04)
  r_date1$NBR = (r_date1$B08 - r_date1$B12)/(r_date1$B08 + r_date1$B12)
  r_date1$NDII = (r_date1$B08 - r_date1$B11)/(r_date1$B08 + r_date1$B11) # http://www.sciencedirect.com/science/article/pii/S0168192309002251
  
  r_date2$NDVI = (r_date2$B08 - r_date2$B04)/(r_date2$B08 + r_date2$B04)
  r_date2$NBR = (r_date2$B08 - r_date2$B12)/(r_date2$B08 + r_date2$B12)
  r_date2$NDII = (r_date2$B08 - r_date2$B11)/(r_date2$B08 + r_date2$B11)
  # further indices tested: MSI, mNDVI705, IRECI, EVI -> they seem not usefull for forests
  # see (https://www.sentinel-hub.com/develop/documentation/eo_products/Sentinel2EOproducts)
  
  print("Calculate differences and detect change...")
  # calculate differences
  r_diff = r_date2 - r_date1
  
  # plot differences within all bands and indices
  pdf(file=paste(out_dir, "diff_plots.pdf", sep=""))  
  for(i in 1:nlayers(r_diff)) plot(r_diff[[i]], main=names(r_diff[[i]]))
  dev.off()
  
  # set threshold and map changes
  change_positive = r_diff$NBR > threshold
  change_negative = r_diff$NBR < -threshold

  # apply median filter
  change_positive_3x3 = focal(change_positive, w=matrix(1,3,3), fun=modal, na.rm=T)
  change_negative_3x3 = focal(change_negative, w=matrix(1,3,3), fun=modal, na.rm=T)

  # apply mask again
  change_positive_3x3 = mask(change_positive_3x3, sp_mask)
  change_negative_3x3 = mask(change_negative_3x3, sp_mask)

  print("Create shapefile...")
  # create polygons for changed areas
  change_positive_poly = rasterToPolygons(change_positive_3x3, fun=function(x){x>0}, dissolve = T)
  change_negative_poly = rasterToPolygons(change_negative_3x3, fun=function(x){x>0}, dissolve = T)
  changed_areas <- rbind(change_positive_poly, change_negative_poly)
  changed_areas@data$change_type[1] = "positive"
  changed_areas@data$change_type[2] = "negative"
  changed_areas@data$change_type = as.factor(changed_areas@data$change_type)
  
  # calculate area
  area_m2 = sum(area(changed_areas))

  print("Final plot and write data...")
  # plot to PDF
  pdf(file=paste(out_dir, "changed_area.pdf", sep=""))  
  plotRGB(r_date2[[rgb_bands]], stretch="lin", axes=T, main=paste("Changed area ", "(", area_m2, " m2)", sep=""))
  plot(changed_areas, col=c("green","red"), border=NA, add=T)
  dev.off()
  
  if (!is.null(out_dir)){
    # write 
    writeOGR(changed_areas, out_dir, "changed_areas", driver="ESRI Shapefile")
    
    # write Sentinel-2 images
    writeRaster(r_date1, paste(out_dir, "Date1_image.tif", sep=""))
    writeRaster(r_date2, paste(out_dir, "Date2_image.tif", sep=""))
  }
  return(changed_areas)
}