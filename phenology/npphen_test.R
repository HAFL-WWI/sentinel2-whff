library(npphen)

df = read.csv("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Baumarten_BGB/spectral_data_l1c_lae_korrigiert.csv")


# calculate vegetation indices
DATES <- c("20170311","20170410","20170430","20170510","20170619","20170719","20170818","20171007","20171017")
for (i in DATES) {
  df[,paste("X", i,"_NDVI", sep="")] <- (df[,paste("X", i,"_B08", sep="")] - df[,paste("X", i,"_B04", sep="")])/(df[,paste("X", i,"_B08", sep="")] + df[,paste("X", i,"_B04", sep="")])
  df[,paste("X", i,"_IRECI", sep="")] <- (df[,paste("X", i,"_B07", sep="")] - df[,paste("X", i,"_B04", sep="")])/(df[,paste("X", i,"_B05", sep="")] / df[,paste("X", i,"_B06", sep="")])
  df[,paste("X", i,"_MCARI", sep="")] <- ((df[,paste("X", i,"_B05", sep="")] - df[,paste("X", i,"_B04", sep="")]) - 0.2 * (df[,paste("X", i,"_B05", sep="")] - df[,paste("X", i,"_B03", sep="")])) * (df[,paste("X", i,"_B05", sep="")] - df[,paste("X", i,"_B04", sep="")])
  df[,paste("X", i,"_GNDVI", sep="")] <- (df[,paste("X", i,"_B07", sep="")] - df[,paste("X", i,"_B03", sep="")])/(df[,paste("X", i,"_B07", sep="")] + df[,paste("X", i,"_B03", sep="")])
}

df = df[,grep("_NDVI",colnames(df))]

# create data frame with regulare ndvi time series
dates_formated = as.Date(DATES, format='%Y%m%d')
doy = as.numeric(strftime(dates_formated, format = "%j"))
doy_all = seq(10,365,10)
dates_all = as.Date(doy_all, origin = "2016-12-31")
df.ndvi = data.frame(date=dates_all, doy=doy_all, ndvi=rep(NA, length.out=length(doy_all)))
df.ndvi$ndvi[df.ndvi$doy %in% doy] = as.numeric(df[1,])

ndvi_ts = Phen(x=df.ndvi$ndvi, dates=df.ndvi$date, h=1, nGS=37, rge=c(0,1))
pdf("plots.pdf")
plot(ndvi_ts)
dev.off()

