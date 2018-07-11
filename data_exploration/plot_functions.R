############################################################
# Plot images
#
# by Dominique Weber, HAFL, BFH
############################################################

# Pixel composites based on VI and composite function
plot_rgb_images <- function(stack_path, dates, band_names, rgb_bands, aoi, out_pdf) {
  # load packages
  library(raster)
  library(rgdal)
  
  # filter .tif files and dates
  fileNames = list.files(stack_path)
  fileNames = fileNames[grep("tif$", fileNames)]
  fileNames = fileNames[grepl(paste(dates, collapse="|"), fileNames)]
  fileNames = paste(stack_path, fileNames, sep="")
  
  # open PDF
  pdf(out_pdf)  
  
  # apply stack function
  for(i in 1:length(fileNames)){
    print(paste("plotting", i, "of", length(fileNames),"..."))
    
    # get stack
    stk_tmp = stack(fileNames[i])
    names(stk_tmp) = band_names
    
    # plot RGB
    plotRGB(stk_tmp[[rgb_bands]], stretch="hist", axes=T, main=paste(basename(fileNames[i])), ext=extent(aoi))
    plot(aoi, col="transparent", border="black", add=T)
  }
  # close pdf
  dev.off()
  
}