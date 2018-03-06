###############################################################################
# 
# Plot spectral characteristics of tree species calculated as means and
# standard errors of bands and selected vegetation indexes per Sentinel-2 image
#
# Save csv file of extracted band values and response variable (species) for
# further analysis
#
###############################################################################

###### Setup ######

# set working directory
setwd("//bfhfilerbe01.bfh.ch/vom1/Desktop/")

# path (WITHOUT BACKSLASH) and name (NO FILE ENDING) of the shapefile to extract response
SHAPE.PATH = "//bfhfilerbe01.bfh.ch/vom1/Desktop/MOTI_Data_shapefile_nurMOTI_Reinheit80_Dg80"
SHAPE.NAME = "trainingdata_buffer6m"

# response variable
RESP <- "Baumart"

# csv with extracted band values
INCSV <- "20180306_bands_dg80.csv"

# dates of the images previously used to extract band values
DATES <- c("20170311","20170410","20170430","20170510","20170619","20170719","20170818","20171007","20171017")

# output file of bands and response variable (WITH .csv SUFFIX)
OUTCSV <- "20180306_variables_dg80.csv"

# output pdf file with plotted spectral characteristics
OUTPDF <- "spectral_charact.pdf"

################################################################################################

### load libraries

library(raster)
library(rgdal)
library(data.table)
library(ggplot2)

### add response variable (classes/species)

# read csv with extracted band values
df <- read.csv(INCSV)

# load shapefile
moti_shape = readOGR(SHAPE.PATH, SHAPE.NAME)

# add column of class to extracted time series (bands)
df <- cbind(as.data.frame(moti_shape[,RESP]), df)

# save .csv
write.csv(df, OUTCSV, row.names = FALSE)

### calculate vegetation indexes

for (i in DATES) {
  df[,paste("X", i,"_NDVI", sep="")] <- (df[,paste("X", i,"_B08", sep="")] - df[,paste("X", i,"_B04", sep="")])/(df[,paste("X", i,"_B08", sep="")] + df[,paste("X", i,"_B04", sep="")])
  df[,paste("X", i,"_IRECI", sep="")] <- (df[,paste("X", i,"_B07", sep="")] - df[,paste("X", i,"_B04", sep="")])/(df[,paste("X", i,"_B05", sep="")] / df[,paste("X", i,"_B06", sep="")])
  df[,paste("X", i,"_MCARI", sep="")] <- ((df[,paste("X", i,"_B05", sep="")] - df[,paste("X", i,"_B04", sep="")]) - 0.2 * (df[,paste("X", i,"_B05", sep="")] - df[,paste("X", i,"_B03", sep="")])) * (df[,paste("X", i,"_B05", sep="")] - df[,paste("X", i,"_B04", sep="")])
  df[,paste("X", i,"_GNDVI", sep="")] <- (df[,paste("X", i,"_B07", sep="")] - df[,paste("X", i,"_B03", sep="")])/(df[,paste("X", i,"_B07", sep="")] + df[,paste("X", i,"_B03", sep="")])
}

### calculate mean of bands per species

toplot1 <- NULL
for(i in c(3:length(colnames(df)))) {
  a <- tapply(df[,i], df[,RESP], FUN=mean)
  toplot1 <- rbind(toplot1, a)
}
toplot1 <- as.data.frame(toplot1)
colnames(toplot1) <- sort(as.character(unique(df[,RESP])))

# calculate standard error of bands per species
toplot2 <- NULL
for(i in c(3:length(colnames(df)))) {
  b <- tapply(df[,i], df[,RESP], FUN=function(x) sd(x)/sqrt(length(x)))
  toplot2 <- rbind(toplot2, b)
}
toplot2 <- as.data.frame(toplot2)
colnames(toplot2) <- sort(paste(unique(df[,RESP]), "_se", sep=""))

# add date (as DOY)
date <- substr(colnames(df)[c(3:length(colnames(df)))], 2, 9)
date <- as.character(as.Date(date, "%Y%m%d"))
toplot1$doy <- as.numeric(strftime(date, format = "%j"))
toplot2$doy <- as.numeric(strftime(date, format = "%j"))

# add band ID
id <- substr(colnames(df)[c(3:length(colnames(df)))], 11, 13)
toplot1$ID <- id
toplot2$ID <- id

# melt dataframes and merge
toplot1_melted <- melt(toplot1, id.vars=c("doy", "ID"))
toplot2_melted <- melt(toplot2, id.vars=c("doy", "ID"))
toplot1_melted$se <- toplot2_melted$value

# rename indexes
toplot1_melted[toplot1_melted=="NDV"] <- "NDVI"
toplot1_melted[toplot1_melted=="IRE"] <- "IRECI"
toplot1_melted[toplot1_melted=="MCA"] <- "MCARI"
toplot1_melted[toplot1_melted=="GND"] <- "GNDVI"

# plot

pdf(OUTPDF,paper="a4r",height=20,width=30)

for (i in unique(toplot1_melted$ID)) {
  
  toplot <- subset(toplot1_melted, ID==i)
  pd <- position_dodge(0.1)
  p <- ggplot(toplot, aes(x=doy, y=value, colour=variable, shape=variable)) + 
    geom_line(position=pd) +
    geom_point(position=pd, size=2) +
    scale_shape_manual(values=seq(0,15)) +
    geom_errorbar(aes(ymin=value-se, ymax=value+se), width=10, position=pd) +
    theme_classic() +
    scale_x_continuous(breaks=sort(unique(toplot$doy)), labels=sort(unique(date))) +
    theme(axis.text.x = element_text(angle=90, margin = margin(t = 15, b = 5))) +
    ylab(i)
  print(p)
  
}

dev.off()



