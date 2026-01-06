library(terra)
#Import PUDL layers (starting with just PHJV, PPJV, and NGPJV to serve as an example)
#value codes
# 0 = Potentially disturbed/other cover
# 1 = Potentially undisturbed grass
# 2 = Potentially disturbed grass
# 3 = Shrub

phjv <- rast("Data/PUDL/PHJV_PUDLmask.tif")
ppjv <- rast("Data/PUDL/PPJV_PUDLmask.tif")
ngpjv <- rast("Data/PUDL/NGPJV_PUDLmask.tif")

#mosaic together
pudlN <- merge(phjv,ppjv,ngpjv)
writeRaster(pudlN, "Data/PUDL/pudlN.tif")



#Import grassland assessment map for high and low conversion and encroachment risk
gam <- rast("Data/CGR_GAM/CGR_GAM_V2_separated.tif")
levels(gam)
#value codes
# 5 = low conversion risk/low encroachment risk
# 100 = high conversion risk
# 111 = high encroachment risk
# 200 = high conversion risk/ high encroachment risk
# 500 = plowed
# 550 = encroached
# 800 = plowed and encroached

#create separate encroachment risk layer
# 1 = low risk
# 2 = high risk
# 3 = encroached
encRisk <- gam
values(encRisk) <- NA
encRisk[gam==5] <- 1 #low risk
encRisk[gam==111|gam==200] <- 2 #high risk
encRisk[gam==550] <- 3 #encroached

#create separate conversion risk layer
# 1 = low risk
# 2 = high risk
# 3 = plowed
conRisk <- gam
values(conRisk) <- NA
conRisk[gam==5] <- 1 #low risk
conRisk[gam==100|gam==200] <- 2 #high risk
conRisk[gam==500] <- 3 #plowed

#load raw conversion risk layer
conRisk <- rast("Data/conRisk/CP_WWF_2021_Intact.tif")

#load landcover rasters
canLUList <- list.files("Data/landcover/", pattern = "Can2020", full.names = T)
canLUrast <- lapply(canLUList, rast)
canLU <- do.call(mosaic, canLUrast)

usaLUList <- list.files("Data/landcover/", pattern = "USA2020", full.names = T)
usaLUrast <- lapply(usaLUList, rast)
usaLU <- do.call(mosaic, usaLUrast)
lu <- mosaic(canLU, usaLU)

