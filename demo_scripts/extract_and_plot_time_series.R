############################################################
# Access and plot Sentinel-2 time series
#
# by Dominique Weber, HAFL, BFH
# 
############################################################

library(raster)
library(rgdal)
library(reshape2)
library(ggplot2)

# set wd
setwd("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/")

# load sentinel-2 images
stk.20170510 = stack("S2A_MSIL1C_20170510T103031_N0205_R108_T32TLT_20170510T103025.tif")
stk.20170619 = stack("S2A_MSIL1C_20170619T103021_N0205_R108_T32TLT_20170619T103021.tif")
stk.20170818 = stack("S2A_MSIL1C_20170818T103021_N0205_R108_T32TLT_20170818T103421.tif")

# read moti data
sdata <- readOGR("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/test_data/MOTI_PPSS_Michaela/20180123-2_MOTI_PPSS.shp")
plot(sdata)

# plot true color RGB
par(mfrow=c(1,3))
plotRGB(stk.20170510[[4:2]], stretch = "lin")
plot(extent(sdata), add=T, col="red")
plotRGB(stk.20170619[[4:2]], stretch = "lin")
plot(extent(sdata), add=T, col="red")
plotRGB(stk.20170818[[4:2]], stretch = "lin")
plot(extent(sdata), add=T, col="red")
par(mfrow=c(1,1))

# colnames
names = c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B10","B11","B12")
names(stk.20170510) = names
names(stk.20170619) = names
names(stk.20170818) = names

# extract
e1 = extract(stk.20170510, sdata, df=T)
e2 = extract(stk.20170619, sdata, df=T)
e3 = extract(stk.20170818, sdata, df=T)

# calc NDVI
e1$NDVI = (e1$B8 - e1$B4)/(e1$B8 + e1$B4)
e2$NDVI = (e2$B8 - e2$B4)/(e2$B8 + e2$B4)
e3$NDVI = (e3$B8 - e3$B4)/(e3$B8 + e3$B4)

# plot NDVI boxplots
df = as.data.frame(cbind(id=e1$ID, NDVI.20170510=e1$NDVI, NDVI.20170619=e2$NDVI, NDVI.20170818=e3$NDVI))
df.long = melt(df, id.vars="id", variable.name="Date", value.name ="NDVI")
ggplot(df.long, aes(x=Date, y=NDVI)) + 
  geom_boxplot()