############################################################################
# 
# Run random forest model on per image extracted band values and selected
# vegetation indexes, balance sample size, drop selected species
#
# (c) HAFL, BFH
############################################################################

###### Setup ######

# set working directory
setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Baumarten_BGB/")

# input file of bands and response variable (WITH .csv SUFFIX)
INCSV <- "spectral_data.csv"

# name random forest file
OUTRF <- "myrf.RData"

# set number of samples per class
SAMPLESIZE <- 100

# dates of the images previously used to extract band values
DATES <- c("20170311","20170410","20170430","20170510","20170619","20170719","20170818","20171007","20171017")

# drop tree species
DROPSP <- c("Dou", "Foe")

# response variable (CHANGE MANUALLY at line 73!)
RESP <- "BAUMART"

##############################################################################################

### Load library ###

library(randomForest)

### Load data and process ###

# load extracted band values and response
df <- read.csv(INCSV)

# drop ID
df[,2] <- NULL

# drop some species/samples excluded

for (i in DROPSP) df <- subset(df, df[,RESP]!=i)
df[,RESP] <- droplevels(df[,RESP])

# calculate vegetation indices
for (i in DATES) {
  df[,paste("X", i,"_NDVI", sep="")] <- (df[,paste("X", i,"_B08", sep="")] - df[,paste("X", i,"_B04", sep="")])/(df[,paste("X", i,"_B08", sep="")] + df[,paste("X", i,"_B04", sep="")])
  df[,paste("X", i,"_IRECI", sep="")] <- (df[,paste("X", i,"_B07", sep="")] - df[,paste("X", i,"_B04", sep="")])/(df[,paste("X", i,"_B05", sep="")] / df[,paste("X", i,"_B06", sep="")])
  df[,paste("X", i,"_MCARI", sep="")] <- ((df[,paste("X", i,"_B05", sep="")] - df[,paste("X", i,"_B04", sep="")]) - 0.2 * (df[,paste("X", i,"_B05", sep="")] - df[,paste("X", i,"_B03", sep="")])) * (df[,paste("X", i,"_B05", sep="")] - df[,paste("X", i,"_B04", sep="")])
  df[,paste("X", i,"_GNDVI", sep="")] <- (df[,paste("X", i,"_B07", sep="")] - df[,paste("X", i,"_B03", sep="")])/(df[,paste("X", i,"_B07", sep="")] + df[,paste("X", i,"_B03", sep="")])
}

# check number of samples per class
table(df[,RESP])

# draw samples
df1 <- NULL
for (i in levels(df[,RESP])) {
  if (table(df[,RESP])[i] >= SAMPLESIZE) {
    a <- sample(which(df[,RESP]==i), size=SAMPLESIZE)
    b <- df[a,]
    df1 <- rbind(df1, b)
  } else {
    a <-  which(df[,RESP]==i)
    b <- df[a,]
    df1 <- rbind(df1, b)
  }
}

# check number of samples per class
table(df1[,RESP])

### Run random forest ###

rf.mdl <- randomForest(BAUMART ~ ., data=df1, ntree=500, importance=TRUE, na.action=na.omit)
rf.mdl
save(rf.mdl,file = OUTRF)




