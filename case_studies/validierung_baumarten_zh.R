# Validierung ZH

# Baumarten Codes (Raster Klassifikation):
# 1: Es_Ah
# 2: Bu
# 3: Ei
# 4: Foe
# 5: Fi
# 6: Lae
# 7: Ta

# Hauptbaumart (HBCODE)
# (Hinweis: Bei Label HBCODE -10)
# -2 nicht interpretierbar (z. B. Blösse)
# 10 Fichte
# 11 Tanne
# 12 Föhre
# 13 Lärche
# 14 anderes Nadelholz
# 15 Buche
# 16 Eiche
# 17 Esche (wenn Ahorn separat), Esche/Ahorn (wenn zusammen)
# 18 Ahorn
# 19 anderes Laubholz

library(raster)
library(rgdal)
library(sp)

setwd("//mnt/cephfs/data/HAFL/WWI-Sentinel-2/Data/Baumarten_ZH_Validierung/")

# load Baumarten Raster
r = raster("Baumarten_ZH_Clip_hierarchisches_modell.tif")

# load verifizierte BK
sp = readOGR("BESTAND_F_KTZH_fuer_HAFL_20180516_wgs84.shp")

# extract most frequent value
sp_compare = extract(r, sp, fun=modal, sp=T, na.rm =T)

# add comparable fields
# BK
sp_compare$BaumartBK = rep(NA, nrow(sp_compare@data))
sp_compare$BaumartBK[sp_compare$HBCODE == 10] = "Fi"
sp_compare$BaumartBK[sp_compare$HBCODE == 11] = "Ta"
sp_compare$BaumartBK[sp_compare$HBCODE == 12] = "Foe"
sp_compare$BaumartBK[sp_compare$HBCODE == 13] = "Lae"
sp_compare$BaumartBK[sp_compare$HBCODE == 15] = "Bu"
sp_compare$BaumartBK[sp_compare$HBCODE == 16] = "Ei"
sp_compare$BaumartBK[sp_compare$HBCODE == 17] = "Es"
sp_compare$BaumartBK[sp_compare$HBCODE == 18] = "Ah"
sp_compare$BaumartBK[sp_compare$HBCODE == 17 | sp_compare$HBCODE == 18] = "Es_Ah"
# Sentinel
sp_compare$BaumartS2 = rep(NA, nrow(sp_compare@data))
sp_compare$BaumartS2[sp_compare$Baumarten_ZH_Clip_hierarchisches_modell == 1] = "Es_Ah"
sp_compare$BaumartS2[sp_compare$Baumarten_ZH_Clip_hierarchisches_modell == 2] = "Bu"
sp_compare$BaumartS2[sp_compare$Baumarten_ZH_Clip_hierarchisches_modell == 3] = "Ei"
sp_compare$BaumartS2[sp_compare$Baumarten_ZH_Clip_hierarchisches_modell == 4] = "Foe"
sp_compare$BaumartS2[sp_compare$Baumarten_ZH_Clip_hierarchisches_modell == 5] = "Fi"
sp_compare$BaumartS2[sp_compare$Baumarten_ZH_Clip_hierarchisches_modell == 6] = "Lae"
sp_compare$BaumartS2[sp_compare$Baumarten_ZH_Clip_hierarchisches_modell == 7] = "Ta"

# write output
writeOGR(sp_compare,".", "Vergleich_BK_and_S2", driver="ESRI Shapefile", overwrite_layer = T)

# read sp
sp_compare = readOGR("Vergleich_BK_and_S2.shp")

# Analysis
sp_compare@data[,c(15:28, 31, 32)] <- lapply(sp_compare@data[,c(15:28, 31, 32)], function(x) as.numeric(as.character(x)))
sp_compare$DGmax = apply(sp_compare@data[,c(18:26)], 1, function(x) max(x))

sp_sub = sp_compare
sp_sub = sp_sub[sp_sub$DGmax >= 50,]
sp_sub = sp_sub[sp_sub$ESCODE != -2 & sp_sub$ESCODE != 10, ]

# confusion matrix
confusionMatrix(sp_sub$BamrtBK, sp_sub$BamrtS2)

# same same?!
sp_sub$SAME = (sp_sub$BamrtBK == sp_sub$BamrtS2)

# write output
writeOGR(sp_sub,".", "Vergleich_BK_and_S2_filtered", driver="ESRI Shapefile", overwrite_layer = T)

