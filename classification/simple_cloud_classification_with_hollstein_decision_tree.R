############################################################################
# Implementation of hollstein cloud detection algorithm with simple ready-to-use
# decision trees. Should achive 91% accuracy... Qaulity of results depends stronlgy
# on the scene. In our example: a lot of false-positive detections for cirrus and shadows.
#
# References:
# https://github.com/sentinel-hub/custom-scripts/tree/master/sentinel-2/hollstein
# http://www.mdpi.com/2072-4292/8/8/666/htm
#
# compare with online version: https://apps.sentinel-hub.com/sentinel-playground/?source=S2&lat=46.70483909428304&lng=8.953617091756314&zoom=12&preset=CUSTOM&layers=B01,B02,B03&maxcc=20&gain=1.0&gamma=1.0&time=2015-01-01%7C2017-06-26&atmFilter=&showDates=false&evalscript=ZnVuY3Rpb24gUyAoYSwgYikgeyByZXR1cm4gYSAtIGIgfTsKZnVuY3Rpb24gUiAoYSwgYikgeyByZXR1cm4gYSAvIGIgfTsKCmxldCBnYWluID0gMi41OwoKbGV0IG5hdHVyYWxDb2xvdXIgPSBbQjA0LCBCMDMsIEIwMl0ubWFwKGEgPT4gZ2FpbiAqIGEpOwoKbGV0IENMRUFSICA9IG5hdHVyYWxDb2xvdXI7CmxldCBTSEFET1cgPSBuYXR1cmFsQ29sb3VyOwpsZXQgV0FURVIgID0gWzAuMSwwLjEsMC43XTsKbGV0IENJUlJVUyA9IFswLjgsMC4xLDAuMV07CmxldCBDTE9VRCAgPSBbMC4zLDAuMywxLjBdOwpsZXQgU05PVyAgID0gWzEuMCwwLjgsMC40XTsKCnJldHVybiAoQjAzIDwgMC4zMTkpCj8gKEI4QSA8IDAuMTY2KQogICAgPyAoUyhCMDMsQjA3KSA8IDAuMDI3KQogICAgICAgID8gKFMoQjA5LEIxMSkgPCAtMC4wOTcpID8gQ0xFQVIgOiBTSEFET1cKICAgICAgICA6IChTKEIwOSxCMTEpIDwgMC4wMjEpID8gV0FURVIgOiBTSEFET1cKICAgIDogKFIoQjAyLEIxMCkgPCAxNC42ODkpCiAgICAgICAgPyAoUihCMDIsQjA5KSA8IDAuNzg4KSA%2FIENMRUFSIDogQ0lSUlVTCiAgICAgICAgOiBDTEVBUgo6IChSKEIwNSxCMTEpIDwgNC4zMykKICAgID8gKFMoQjExLCBCMTApIDwgMC4yNTUpCiAgICAgICAgPyAoUyhCMDYsIEIwNykgPCAtMC4wMTYpID8gQ0xPVUQgOiBDSVJSVVMKICAgICAgICA6IChCMDEgPCAwLjMpID8gQ0xFQVIgOiBDTE9VRAogICAgOiAoQjAzIDwgMC41MjUpCiAgICAgICAgPyAoUihCMDEsIEIwNSkgPCAxLjE4NCkgPyBDTEVBUiA6IFNIQURPVwogICAgICAgIDogU05PVzsK&evalscripturl=https://raw.githubusercontent.com/sentinel-hub/customScripts/master/sentinel-2/hollstein/script.js
#

# 
# (c) by Dominique Weber, HAFL, BFH
############################################################################

library(raster)
library(rgdal)
library(sp)

###########################
# USER CONFIG

setwd("/mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Cloud_classification_hollstein/")

# study area
EXTENT = extent(readOGR("/mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Surselva_Geb/Bezirk_Surselva_wgs84.shp"))

# S2 band names and indices for the corresponding raster stacks
BAND_NAMES = c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B10","B11","B12")

# load stack
stk = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TMS/2017/S2A_MSIL1C_20170626T102021_N0205_R065_T32TMS_20170626T102321.tif")
names(stk) = BAND_NAMES
stk = crop(stk, EXTENT)

diff <- function(a, b){  return(a - b)}
ratio <- function(a, b){  return(a / b)}

stk = stk/10000

# decisions
L = stk$B3 < 0.319
LL = stk$B8A<0.166
LLL = diff(stk$B3, stk$B7)<0.027
LLLL = diff(stk$B9, stk$B11)< -0.097
LLLR = !LLLL
LLR = !LLL
LLRL = diff(stk$B9, stk$B11)< -0.021
LLRR = !LLRL
LR = !LL
LRL = ratio(stk$B2, stk$B10)<14.689
LRLL = ratio(stk$B2, stk$B9)<0.788
LRLR = !LRLL
LRR = !LRL

R = !L
RL = ratio(stk$B5, stk$B11)<4.33
RLL = diff(stk$B11, stk$B10)<0.255
RLLL = diff(stk$B6, stk$B7)< -0.016
RLLR = !RLLL
RLR = !RLL
RLRL = stk$B3<0.3
RLRR = !RLRL
RR = !RL
RRL = stk$B3 <0.525
RRLL = ratio(stk$B1, stk$B5)<1.184
RRLR = !RRLL
RRR = !RRL

# classify
clear = 1
cloud = 2
shadow = 3
cirrus = 4
snow = 5
water = 6
stk$scm = stk$B1

# decision tree
stk$scm[L&LL&LLL&LLLL] = clear
stk$scm[L&LL&LLL&LLLR] = shadow
stk$scm[L&LL&LLR&LLRL] = water
stk$scm[L&LL&LLR&LLRR] = shadow
stk$scm[L&LR&LRL&LRLL] = clear
stk$scm[L&LR&LRL&LRLR] = cirrus
stk$scm[L&LR&LRR] = clear
stk$scm[R&RL&RLL&RLLL] = cloud
stk$scm[R&RL&RLL&RLLR] = cirrus
stk$scm[R&RL&RLR&RLRL] = clear
stk$scm[R&RL&RLR&RLRR] = cloud
stk$scm[R&RR&RRL&RRLL] = clear
stk$scm[R&RR&RRL&RRLR] = shadow
stk$scm[R&RR&RRR] = snow

pdf("scm.pdf")
plot(stk$scm)
dev.off()


writeRaster(stk, "l1c_20170626_and_scm.tif")