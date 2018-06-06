# NBR max test
#
# comment: Wurde getestet für evergreen forests. Dort macht es mehr Sinn.
# Bei uns ist der Winter für diese Methode wohl nicht so geeignet (kleinere NBR Werte für Nadelhölzer). 
# Es werden noch zusätzliche Schritte angewandt: z.B. Self-referencing rNBR und delta dNBR zwischen zwei Jahren.
# Methode gibt es als GEE script: https://github.com/Andi1974/Forest-degradation-monitoring
# see publication: http://www.mdpi.com/2072-4292/10/4/544/htm

# load libraries
library(raster)
library(rgdal)
library(sp)


###########################
# USER CONFIG

# set working directory
setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/BGB_Change/")

# path (WITH BACKSLASH) to raster stacks and dates for stacks
STACK_PATH = "//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/"
STACK_DATES = c("20170311","20170410","20170430","20170510","20170619","20170719","20170818","20171007","20171017")

# Sentinel-2 band names and indices for the corresponding raster stacks
BAND_NAMES = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B11", "B12")

###########################
# LOAD RASTER DATA

print("Loading raster data..")

# prepare stackList
stackList = list(
  stack = vector(),
  columnStart = vector()
)

# filter .tif files
fileNames = list.files(STACK_PATH)
fileNames = fileNames[grep("tif$", fileNames)]

# load stacks and add them to stacklist
lapply(STACK_DATES, function(date) {
  tmp_stk <-  stack(paste(
    STACK_PATH, 
    fileNames[grep(date, fileNames)][[1]],
    sep=""
  ))
  
  stackList$stack        <<- c(stackList$stack, tmp_stk)
  stackList$columnStart  <<- c(stackList$columnStart, paste(date, "_", sep=""))
})

# check if stackList.lenght == columnStart.lenght
try(if (length(stackList$stack) != length(stackList$columnStart)) 
  stop("length of stack not equal to lenght of columnStarts") 
)

# apply names and select bands
for(i in 1:length(stackList$stack)) {
  # select relevant bands
  stackList$stack[[i]] = stackList$stack[[i]][[1:length(BAND_NAMES)]]
  # name stacks
  names(stackList$stack[[i]]) = c(BAND_NAMES)
}

###########################
# Calc NBR max

# BGB forest mask
sp_mask = readOGR("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/BGB_Change/fbb_aoi_and_wiliwald.shp")

# apply crop
nbrStk = stack()
for(i in 1:length(stackList$stack)) {
  print(paste("exract", i, "of", length(stackList$stack)))
  stackList$stack[[i]] = crop(stackList$stack[[i]], sp_mask)
  stackList$stack[[i]]$NBR = (stackList$stack[[i]]$B08 - stackList$stack[[i]]$B12)/(stackList$stack[[i]]$B08 + stackList$stack[[i]]$B12)
  nbrStk = addLayer(nbrStk, stackList$stack[[i]]$NBR)
}

# calc nbr max
nbrMax = calc(nbrStk, fun=max, na.rm=T)
nbrMax = mask(nbrMax, sp_mask)
pdf("nbrMax")
plot(nbrMax, main="NBR max for 2017 - 'Offener Boden'")
dev.off()