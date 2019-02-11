############################################################
# Build GeoTiff raster stacks from JP2 Sentinel-2 data
# -> For a defined extent and resampled to 10m
#
# by Dominique Weber, HAFL, BFH
############################################################

library(raster)
library(rgdal)

build_raster_stack <- function(in_dir, out_dir=NULL, ext=NULL, L2A=F) {
  # CONFIG
  band_names_l1c = c("B01","B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A","B09", "B10", "B11", "B12")
  band_names_l2a = c("B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12", "B112")
  l2a_pattern = "B02_10m.jp2$|B03_10m.jp2$|B04_10m.jp2$|B05_20m.jp2$|B06_20m.jp2$|B07_20m.jp2$|B08_10m.jp2$|B8A_20m.jp2$|B11_20m.jp2$|B12_20m.jp2$"
  
  if (L2A) { 
    files <- list.files(in_dir, pattern = l2a_pattern, full.names=T, recursive = T)
  } else { 
    files <- list.files(in_dir, pattern = "\\.jp2$", full.names=T) 
  }
  
  # TODO multi-core implementation
  # build resampled stack
  stk = stack()
  print("Stacking layers...")
  for (f in files){
    print(f)
    rtmp = raster(f)
    fact = res(rtmp)[1]/10
    if (fact > 1) rtmp = disaggregate(rtmp, fact)
    stk = stack(stk, rtmp)
  }
  
  # sort bands etc.
  if (L2A) { 
    stk = stk[[sapply(band_names_l2a, function(i) which(grepl(i, basename(files))))]]
  } else { 
    stk = stk[[sapply(band_names_l1c, function(i) which(grepl(i, basename(files))))]]
  }
  
  # crop and write subset
  if (!is.null(ext)){
    print("Cropping layers...")
    stk_sub = crop(stk, ext)    
  } else {
    stk_sub = stk
  }

  # create out dir
  if (!is.null(out_dir)){
    file_name = basename(files[[1]])
    out_path = paste(out_dir, substr(file_name, 1, 19), "_stack",".tif", sep="")
    print(paste("write raster:", out_path))
    dir.create(out_dir, showWarnings = T)
    writeRaster(stk_sub, out_path, datatype="INT2U")
  }
  return(stk_sub)
}

