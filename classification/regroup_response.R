###########################################################################################
# 
# Regroup response of predictor dataset (spectral extraction)
#
# (c) by Dominique Weber, HAFL, BFH
#
###########################################################################################

# set working directory
setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Baumarten_BGB/")

# input file of bands and response variable (WITH .csv SUFFIX)
df = read.csv("spectral_data.csv")

# build NH and LH classes from tree species
i.nh = which(df$BAUMART=="Fi" | df$BAUMART=="Ta" | df$BAUMART=="Lae" | df$BAUMART=="Foe" | df$BAUMART=="Dou")
i.lh = which(df$BAUMART=="Bu" | df$BAUMART=="Ei" | df$BAUMART=="BAh" | df$BAUMART=="Es")
df$CLASS = rep(NA, times=nrow(df))
df$CLASS[i.nh] = "NH"
df$CLASS[i.lh] = "LH"
df$BAUMART = as.factor(df$CLASS)
df$CLASS = NULL

# write new csv
write.csv(df, "spectral_data_nh_lh.csv")
