############################################################################
# Eschentriebsterben SA Cedric: Boxplots and data mining
#
# (c) by Dominique Weber, HAFL
############################################################################

library(raster)
library(rgdal)
library(sp)

# load Sentinel-2 data
s2_l1c_20170410 = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/S2A_MSIL1C_20170410T103021_N0204_R108_T32TLT_20170410T103020.tif")
s2_l1c_20170430 = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/S2A_MSIL1C_20170430T103021_N0205_R108_T32TLT_20170430T103024.tif")
s2_l1c_20170510 = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/S2A_MSIL1C_20170510T103031_N0205_R108_T32TLT_20170510T103025.tif")
s2_l1c_20170619 = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/S2A_MSIL1C_20170619T103021_N0205_R108_T32TLT_20170619T103021.tif")
s2_l1c_20170818 = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/S2A_MSIL1C_20170818T103021_N0205_R108_T32TLT_20170818T103421.tif")
s2_l1c_20171012= stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TLT/2017/S2B_MSIL1C_20171012T103009_N0205_R108_T32TLT_20171012T103249.tif")

# band names for L1C
names = c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B10","B11","B12")
# band names for L2A
# names = c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B11","B12")

names(s2_l1c_20170410) = names
names(s2_l1c_20170430) = names
names(s2_l1c_20170510) = names
names(s2_l1c_20170619) = names
names(s2_l1c_20170818) = names
names(s2_l1c_20171012) = names

# load field data
sp_field = readOGR("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Eschentriebsterben/SA_Cedric/Data/Field/samples_moy_2000m2_wgs84.shp")
sp_crtl = readOGR("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Eschentriebsterben/SA_Cedric/Data/Control/30_punkte_wald_perimeter_keine_esche_buffer_wgs84.shp")

# crop extent
ext = extent(352510, 356430, 5201570, 5204170)

# crop
s2_l1c_20170410 = crop(s2_l1c_20170410, ext)
s2_l1c_20170430 = crop(s2_l1c_20170430, ext)
s2_l1c_20170510 = crop(s2_l1c_20170510, ext)
s2_l1c_20170619 = crop(s2_l1c_20170619, ext)
s2_l1c_20170818 = crop(s2_l1c_20170818, ext)
s2_l1c_20171012 = crop(s2_l1c_20171012, ext)

# plot overview
plotRGB(s2_l1c_20170619[[4:2]], stretch="lin", axes=T)
plot(sp_field, add=T, col="red")
plot(sp_crtl, add=T, col="grey")

# NDVI
s2_l1c_20170410$NDVI = (s2_l1c_20170410$B8 - s2_l1c_20170410$B4)/(s2_l1c_20170410$B8 + s2_l1c_20170410$B4)
s2_l1c_20170430$NDVI = (s2_l1c_20170430$B8 - s2_l1c_20170430$B4)/(s2_l1c_20170430$B8 + s2_l1c_20170430$B4)
s2_l1c_20170510$NDVI = (s2_l1c_20170510$B8 - s2_l1c_20170510$B4)/(s2_l1c_20170510$B8 + s2_l1c_20170510$B4)
s2_l1c_20170619$NDVI = (s2_l1c_20170619$B8 - s2_l1c_20170619$B4)/(s2_l1c_20170619$B8 + s2_l1c_20170619$B4)
s2_l1c_20170818$NDVI = (s2_l1c_20170818$B8 - s2_l1c_20170818$B4)/(s2_l1c_20170818$B8 + s2_l1c_20170818$B4)
s2_l1c_20171012$NDVI = (s2_l1c_20171012$B8 - s2_l1c_20171012$B4)/(s2_l1c_20171012$B8 + s2_l1c_20171012$B4)

# nice names
names(s2_l1c_20170410) = paste(names(s2_l1c_20170410), "20170410", sep=".")
names(s2_l1c_20170430) = paste(names(s2_l1c_20170430), "20170430", sep=".")
names(s2_l1c_20170510) = paste(names(s2_l1c_20170510), "20170510", sep=".")
names(s2_l1c_20170619) = paste(names(s2_l1c_20170619), "20170619", sep=".")
names(s2_l1c_20170818) = paste(names(s2_l1c_20170818), "20170818", sep=".")
names(s2_l1c_20171012) = paste(names(s2_l1c_20171012), "20171012", sep=".")

# stack all
stk = stack(s2_l1c_20170410, s2_l1c_20170430, s2_l1c_20170510, s2_l1c_20170619, s2_l1c_20170818, s2_l1c_20171012)

# extract
e_field = extract(stk, sp_field, fun=mean, sp=T, na.rm=T)
e_crtl = extract(stk, sp_crtl, fun=mean, sp=T, na.rm=T)

# add group variable
e_field$group = rep("field", length(e_field))
e_crtl$group = rep("crtl", length(e_crtl))

# fix col names
names(e_crtl)[2] = "X_Coord"
names(e_crtl)[3] = "Y_Coord"

# combine datasets
df = rbind(e_field[,c(names(stk), "group", "X_Coord", "Y_Coord")],
           e_crtl[,c(names(stk), "group", "X_Coord", "Y_Coord")])
df$group = as.factor(df$group)

# compare field and control
boxplot(df$NDVI.20170510 ~ df$group, main="Sentinel-2 Bild 5. Mai 2017", ylab="NDVI")

# add field data
df$class = rep("crtl", length(df))
df$class[df$group=="field"] = sp_field$class
df$class = as.factor(df$class)

# write CSV
write.csv(df, "//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Eschentriebsterben/SA_Cedric/Data/s2_l1c_spectral_data.csv")

# plot
par(mfrow = c(3, 2),oma=c(0,0,2,0))
plot(df$NDVI.20170410 ~ df$class, main="Sentinel-2 Bild 10. April 2017", xlab="Schadensklasse und Kontrolle (1 tief, 4 hoch)", ylab="NDVI")
plot(df$NDVI.20170430 ~ df$class, main="Sentinel-2 Bild 30. April 2017", xlab="Schadensklasse und Kontrolle (1 tief, 4 hoch)", ylab="NDVI")
plot(df$NDVI.20170510 ~ df$class, main="Sentinel-2 Bild 5. Mai 2017", xlab="Schadensklasse und Kontrolle (1 tief, 4 hoch)", ylab="NDVI")
plot(df$NDVI.20170619 ~ df$class, main="Sentinel-2 Bild 6. Juni 2017", xlab="Schadensklasse und Kontrolle (1 tief, 4 hoch)", ylab="NDVI")
plot(df$NDVI.20170818 ~ df$class, main="Sentinel-2 Bild 18. August 2017", xlab="Schadensklasse und Kontrolle (1 tief, 4 hoch)", ylab="NDVI")
plot(df$NDVI.20171012 ~ df$class, main="Sentinel-2 Bild 12. Oktober 2017", xlab="Schadensklasse und Kontrolle (1 tief, 4 hoch)", ylab="NDVI")
title("SA Cédric Eschentriebsterben", outer=TRUE)
par(mfrow = c(1, 1))
