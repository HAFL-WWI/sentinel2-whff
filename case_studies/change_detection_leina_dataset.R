############################################################
# Change detection and LeiNa dataset (GR)
#
# by Dominique Weber, HAFL, BFH
# 
############################################################

library(raster)
library(rgdal)

setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/LeiNa_Change/")
out_dir = "Results/"

r_date1 = stack("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Sentinel-2/LeiNa/L1C_32TNS_20150826_AWS.tif")
r_date2 = stack("//mnt/cephfs/data/BFH/Geodata/World/Sentinel-2/S2MSI1C/GeoTIFF/T32TNS/2017/S2A_MSIL1C_20170825T102021_N0205_R065_T32TNS_20170825T102114.tif")

# load mask for area of interest
leina_data = readOGR("Data/LeiNa_2016_sav_wgs84.shp")

# set proper band names 
BAND_NAMES = c("B01","B02","B03","B04","B05","B06","B07","B08","B8A","B09","B10","B11","B12")
names(r_date1) = BAND_NAMES
names(r_date2) = BAND_NAMES

# plot true colour RGBs to PDF
rgb_bands = c("B11", "B08", "B04")
pdf(file=paste(out_dir, "overview.pdf", sep=""))  
plotRGB(r_date1[[rgb_bands]], stretch="lin", axes=T)
plot(leina_data, add=T, col="red", border=0)
plotRGB(r_date2[[rgb_bands]], stretch="lin", axes=T)
plot(leina_data, add=T, col="red", border=0)
dev.off()

# # TEST subset
# leina_data$HOLZMENGE = as.numeric(as.character(leina_data$HOLZMENGE))
# leina_data = leina_data[leina_data$HOLZMENGE > 1000,]
# x_coords = coordinates(leina_data)[,1]
# y_coords = coordinates(leina_data)[,2]
# selection_north_west = x_coords < 540000 & x_coords > 500000 & y_coords < 5200000 & y_coords > 5170000
# leina_data = leina_data[selection_north_west, ]

# leina_data = leina_data[1,]
leina_data = crop(leina_data, r_date1)
# r_date1 = crop(r_date1, extent(leina_data))
# r_date2 = crop(r_date2, extent(leina_data))
# r_date1 = mask(r_date1, leina_data)
# r_date2 = mask(r_date2, leina_data)

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
r_diff = r_date2 - r_date1

# extract multi-core
library(foreach)
library(doParallel)

start_time <- Sys.time()
cl = makeCluster(detectCores() -1)
registerDoParallel(cl)
iterations <- length(leina_data)

# start raster value extraction
print(paste("processing", iterations, "polys parallel"))
df <- data.frame()
df = foreach(i=1:iterations, .packages = c("raster"), .combine = "rbind") %dopar% {
  # extract spectral values
  print("extract spectral values...")
  df_tmp = extract(r_diff, leina_data[i,], fun=mean, na.rm=T, df=T)
  return(df_tmp)
}

#end cluster
stopCluster(cl)

end_time <- Sys.time()
end_time - start_time

# names
colnames(df)[-1] = names(r_date1)

# add some leina info
leina_data$HOLZMENGE = as.numeric(as.character(leina_data$HOLZMENGE))
df$volume = leina_data$HOLZMENGE
df$area = leina_data$SHAPE_AREA
df$intensity = df$volume / df$area

# write CSV
write.csv(df, "Results/mean_spectral_diff.csv")

# PLOTS
df= read.csv("Results/mean_spectral_diff.csv")

# simple Filter
df_sub= df[df$volume > 0 & df$area >= 500,]

# plot results volume
pdf(file=paste(out_dir, "results_volume.pdf", sep=""))  
d.lm = lm(df_sub$NDVI ~ df_sub$volume)
plot(df_sub$NDVI ~ df_sub$volume, main = paste("r2.adj.", round(summary(d.lm)$adj.r.squared, 2),", p-value:", round(summary(d.lm)$coefficients[,4][2],2)))
abline(d.lm, col="red")
d.lm = lm(df_sub$NBR ~ df_sub$volume)
plot(df_sub$NBR ~ df_sub$volume, main = paste("r2.adj.", round(summary(d.lm)$adj.r.squared, 2),", p-value:", round(summary(d.lm)$coefficients[,4][2],2)))
abline(d.lm, col="red")
plot(df_sub$NDII ~ df_sub$volume)
plot(df_sub$B11 ~ df_sub$volume)
plot(df_sub$B08 ~ df_sub$volume)
plot(df_sub$B04 ~ df_sub$volume)
plot(df_sub$B03 ~ df_sub$volume)
dev.off()

# plot results intensity
pdf(file=paste(out_dir, "results_intensity.pdf", sep=""))  
d.lm = lm(df_sub$NDVI ~ df_sub$intensity)
plot(df_sub$NDVI ~ df_sub$intensity, main = paste("r2.adj.", round(summary(d.lm)$adj.r.squared, 2),", p-value:", round(summary(d.lm)$coefficients[,4][2],2)))
abline(d.lm, col="red")
d.lm = lm(df_sub$NBR ~ df_sub$intensity)
plot(df_sub$NBR ~ df_sub$intensity, main = paste("r2.adj.", round(summary(d.lm)$adj.r.squared, 2),", p-value:", round(summary(d.lm)$coefficients[,4][2],2)))
abline(d.lm, col="red")
plot(df_sub$NDII ~ df_sub$intensity)
plot(df_sub$B11 ~ df_sub$intensity)
plot(df_sub$B08 ~ df_sub$intensity)
plot(df_sub$B04 ~ df_sub$intensity)
plot(df_sub$B03 ~ df_sub$intensity)
dev.off()

library(ggplot2)
pdf(file=paste(out_dir, "results_volume_groups.pdf", sep=""))  

df_sub$group = rep("<50m3", nrow(df_sub))
df_sub$group[df_sub$volume<50] = "<50m3"
df_sub$group[df_sub$volume>50] = ">50m3"
df_sub$group[df_sub$volume>500] = ">500m3"
df_sub$group[df_sub$volume>1000] = ">1000m3"
df_sub$group = ordered(df_sub$group, levels = c("<50m3", ">50m3", ">500m3", ">1000m3"))

ggplot(df_sub, aes(df_sub$NBR,fill=group, colour=group)) +
  geom_density(alpha=0.5, lwd=1, adjust=1) +
  ggtitle("NBR")
ggplot(df_sub, aes(df_sub$NDVI,fill=group, colour=group)) +
  geom_density(alpha=0.5, lwd=1, adjust=1) +
  ggtitle("NDVI")
ggplot(df_sub, aes(df_sub$NDII,fill=group, colour=group)) +
  geom_density(alpha=0.5, lwd=1, adjust=1) +
  ggtitle("NDII")
ggplot(df_sub, aes(df_sub$B04,fill=group, colour=group)) +
  geom_density(alpha=0.5, lwd=1, adjust=1) +
  ggtitle("B4 Red")
ggplot(df_sub, aes(df_sub$B03,fill=group, colour=group)) +
  geom_density(alpha=0.5, lwd=1, adjust=1) +
  ggtitle("B3 Green")
ggplot(df_sub, aes(df_sub$B08,fill=group, colour=group)) +
  geom_density(alpha=0.5, lwd=1, adjust=1) +
  ggtitle("B8 NIR")
ggplot(df_sub, aes(df_sub$B03,fill=group, colour=group)) +
  geom_density(alpha=0.5, lwd=1, adjust=1) +
  ggtitle("B11 SWIR")
ggplot(df_sub, aes(df_sub$B12,fill=group, colour=group)) +
  geom_density(alpha=0.5, lwd=1, adjust=1) +
  ggtitle("B12 SWIR")
dev.off()

pdf(file=paste(out_dir, "volume_groups_boxplots.pdf", sep=""), paper="a4r")
par(mfrow=c(2,2))
par(mar=c(2,2,2,1))
boxplot(df_sub$B03 ~ df_sub$group, main= "d_B3 Green")
boxplot(df_sub$NBR ~ df_sub$group, main= "d_NBR")
boxplot(df_sub$NDVI ~ df_sub$group, main = "d_NDVI")
boxplot(df_sub$NDII ~ df_sub$group, main = "d_NDII")
par(mfrow=c(1,1))
dev.off()

pdf(file=paste(out_dir, "volume_groups_boxplots_more.pdf", sep=""), width=12)
par(mfrow=c(2,4))
par(mar=c(2,2,2,1))
boxplot(df_sub$B04 ~ df_sub$group, main= "Red")
boxplot(df_sub$B03 ~ df_sub$group, main= "Green")
boxplot(df_sub$B02 ~ df_sub$group, main= "Blue")
boxplot(df_sub$B08 ~ df_sub$group, main= "NIR")
boxplot(df_sub$B11 ~ df_sub$group, main= "SWIR (B11)")
boxplot(df_sub$NDVI ~ df_sub$group, main = "NDVI")
boxplot(df_sub$NBR ~ df_sub$group, main= "NBR")
boxplot(df_sub$NDII ~ df_sub$group, main = "NDII")
par(mfrow=c(1,1))
dev.off()


pdf(file=paste(out_dir, "volume_relationship.pdf", sep=""), width=12)
plot(df_sub$volume, df_sub$NDII)
dev.off()

summary(lm(df_sub$volume ~ df_sub$NDII))
