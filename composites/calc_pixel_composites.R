############################################################
# Greenest pixel (NDVI max, NBR min) composite
#
# by Dominique Weber, HAFL, BFH
############################################################

# Pixel composites based on VI and composite function
calc_pixel_composites <- function(stack_path, band_names, dates, txt, stack_fun, composite_fun=max) {
  # load packages
  library(raster)
  library(rgdal)
  library(foreach)
  library(doParallel)
  
  # filter .tif files and dates
  fileNames = list.files(stack_path)
  fileNames = fileNames[grep("tif$", fileNames)]
  fileNames = fileNames[grepl(paste(dates, collapse="|"), fileNames)]
  fileNames = paste(stack_path, fileNames, sep="")
  
  # prepare stack
  vi_stk = stack()
  print("Images to process:")
  print(fileNames)

  # register for paralell processing
  print("starting multi-core processing, applying stack function...")
  cl = makeCluster(detectCores() -1)
  registerDoParallel(cl)
  
  # apply stack function
  vi_stk = foreach(i=1:length(fileNames), .packages = c("raster"), .combine = "addLayer") %dopar% {
    print(paste("processing", i, "of", length(fileNames),"..."))
    # get stack
    stk_tmp = stack(fileNames[i])
    names(stk_tmp) = band_names

    # calculate indices
    vi_tmp = stack_fun(stk_tmp)
    return(vi_tmp)
  }
  
  # calculate composite based on function provided (i.e. max)
  print("Building composite...")
  pixel_composite = calc(vi_stk, composite_fun, na.rm=T)
  
  # plot composites in PDF
  pdf(paste("composite_", txt, ".pdf", sep=""))
  plot(pixel_composite)
  dev.off()
  
  # stop cluster only here, otherwise tmp files may get lost
  stopCluster(cl)
  return(pixel_composite)
}

# TODO not working yet...
# BAP composite function
# the idea is to composite the entire stack (band 1-13) based on i.e. NDVI max
calc_bap_composite <- function(stack_path, band_names, quality_stack, quality_band) {
  # load packages
  library(raster)
  library(rgdal)
  
  # filter .tif files and dates
  fileNames = list.files(stack_path)
  fileNames = fileNames[grep("tif$", fileNames)]
  fileNames = fileNames[grepl(paste(dates, collapse="|"), fileNames)]
  fileNames = paste(stack_path, fileNames, sep="")
  
  # init stack
  bap_composite = stack(fileNames[1])
  names(bap_composite) = band_names
  
  for(i in 1:length(fileNames)){
    # get stack
    stk_tmp = stack(fileNames[i])
    names(stk_tmp) = band_names
    
    # add masked quality pixels to bap composite
    quality_mask = (quality_stack[i] == quality_band)
    for(i in 1:nlayers(bap_composite)){
      bap_composite[i][quality_mask] = stk_tmp[i][quality_mask]
    }
  }
  return(bapcomposite)
}