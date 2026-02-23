library(terra)
library(sf)
library(dplyr)
library(rnaturalearth)
######################ONLY RUN 1-3 ONCE. SKIP TO 4 IF ALREADY RUN#################################
# 1. Import layers
#PUDL layers (starting with just PHJV, PPJV, and NGPJV to serve as an example)
#value codes
# 0 = Potentially disturbed/other cover
# 1 = Potentially undisturbed grass
# 2 = Potentially disturbed grass
# 3 = Shrub

phjv <- rast("Data/PUDL/PHJV_PUDLmask.tif")
ppjv <- rast("Data/PUDL/PPJV_PUDLmask.tif")
ngpjv <- rast("Data/PUDL/NGPJV_PUDLmask.tif")
# opjv <- rast("Data/PUDL/OPJV_PUDLmask.tif")
# pljv <- rast("Data/PUDL/PLJV_PUDLmask.tif")
# rbjv <- rast("Data/PUDL/RBPJV_PUDLmask.tif")
# rgjv <- rast("Data/PUDL/RGJV_PUDLmask.tif")

#mosaic together, save a reload
pudlN <- merge(phjv,ppjv,ngpjv)
writeRaster(pudlN, "Data/PUDL/pudlN.tif")
pudlN <- rast("Data/PUDL/pudlN.tif")
rm(phjv,ppjv,ngpjv)

#2. Import grassland assessment map for high and low conversion and encroachment risk and crop to match pudlN
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
# 1000 = Masked (forest and water?)
#crop gam to match extent of pudlN
gam <- project(gam, crs(pudlN), res = res(gam), method = "near")
gamN <- crop(gam, pudlN)

#change mask pixels to NA
gamN <- ifel(gamN == 1000, NA, gamN)
gamN <- ifel(gamN == 0, NA, gamN)

#save and reload to save RAM
writeRaster(gamN, "Data/CGR_GAM/CGR_GAM_N.tif")
gamN <- rast("Data/CGR_GAM/CGR_GAM_N.tif")
plot(gamN)
levels(gamN)

#3. crop pudlN with gamN and resample to 30m to speed up processing time and ensure all layers are snapped together
pudlN <- crop(pudlN, gamN)
pudlN30 <- resample(pudlN, gamN, method = "mode")
plot(pudlN30)

#convert to factor and INT1U to save memory
pudlN30 <- as.int(pudlN30)
pudlN30 <- as.factor(pudlN30)
levels(pudlN30)
writeRaster(pudlN30, "Data/PUDL/pudlN30.tif", datatype = "INT1U", overwrite = T)
pudlN30 <- rast("Data/PUDL/pudlN30.tif")

#save gamN as INT2U to save memory as well
writeRaster(gamN, "Data/CGR_GAM/gamN.tif", datatype = "INT2U", overwrite = T)
gamN <- rast("Data/CGR_GAM/gamN.tif")


#4.Combine gamN and pudlN to find all unique combinations 
#load pudlN and gamN
gamN <- rast("Data/CGR_GAM/gamN.tif")
pudlN30 <- rast("Data/PUDL/pudlN30.tif")

#check for alignment and stack
compareGeom(pudlN30, gamN)

#create new raster with all unique combinations of these 2
stepdown <- app(c(gamN, pudlN30), fun = function(x) {
  if (any(is.na(x))) {NA}
  else {
    x[1] * 10 + x[2]
  }
}, 
filename = "Output/stepdownN.tif", 
wopt = list(datatype = "INT2U"))
#clear RAM
gc()

#change to factor
is.int(stepdown)
stepdown <- as.factor(stepdown)
levels(stepdown)

#mask with gamN
stepdown <- mask(stepdown, gamN)

#remove levels that do not occur
stepdown <- droplevels(stepdown)
levels(stepdown)
writeRaster(stepdown, "Output/stepdown_F.tif")

#########If 1-4 have already been run, start here#####################################################
stepdown <- rast("Output/stepdown_F.tif")

#create Raster attibute table, but do not add to raster because only 1 label column will be included.
levels <- unname(unlist(levels(stepdown)[[1]][1]))
rat <- data.frame(
  value = levels,
  conRisk = c("low", "low", "low", "low", 
              "high", "high", "high", "high", 
              "low", "low", "low", "low", 
              "high", "high", "high", "high", 
              "plowed", "plowed", "plowed", "plowed", 
              "encroached", "encroached", "encroached", "encroached",
              "plowed/encroached", "plowed/encroached", "plowed/encroached", "plowed/encroached"),
  encRisk = c("low", "low", "low", "low",
              "low", "low", "low", "low",
              "high", "high", "high", "high",
              "high", "high", "high", "high",
              "plowed", "plowed", "plowed", "plowed", 
              "encroached", "encroached", "encroached", "encroached",
              "plowed/encroached", "plowed/encroached", "plowed/encroached", "plowed/encroached"),
  PUDL = c("Potentially disturbed/other cover", "Potentially undisturbed grass", "Potentially disturbed grass", "Shrub",
           "Potentially disturbed/other cover", "Potentially undisturbed grass", "Potentially disturbed grass", "Shrub",
           "Potentially disturbed/other cover", "Potentially undisturbed grass", "Potentially disturbed grass", "Shrub",
           "Potentially disturbed/other cover", "Potentially undisturbed grass", "Potentially disturbed grass", "Shrub",
           "Potentially disturbed/other cover", "Potentially undisturbed grass", "Potentially disturbed grass", "Shrub",
           "Potentially disturbed/other cover", "Potentially undisturbed grass", "Potentially disturbed grass", "Shrub",
           "Potentially disturbed/other cover", "Potentially undisturbed grass", "Potentially disturbed grass", "Shrub")
  
)

#first look at frequency of pixel across the entire study area
catFreq <- freq(stepdown)
#add to rat table and convert to millions of acres
rat$count <- catFreq$count
rat$acreMil <- rat$count * prod(res(stepdown)) / 4047 /1000000

#create reclassify matrix (is-becomes) to simplify stepdown raster to less categories
#Here are the categories I'm combining
#1. 5000: all plowed pixels into 1
#2. 5500: all encroached pixels into 1
#3. 5000: all plowed/encroached into plowed (for simplicity)
#4. 50: all potentially disturbed/other into other cover
#5. 53: all shrub into 1

rcl <- matrix(data = c(5001,5000,
                       5002,5000,
                       5003,5000,
                       5501,5500,
                       5502,5500,
                       5503,5500,
                       8000,5000,
                       8001,5000,
                       8002,5000,
                       8003,5000,
                       1000,50,
                       1110,50,
                       2000,50,
                       1003,53,
                       1113,53,
                       2003,53), ncol=2, byrow = T)

#reclassify to minimize number of categories
stepdown_rc <- classify(stepdown, rcl, filename = "Output/stepdownN_rc.tif", overwrite = T)
stepdown_rc <- as.factor(stepdown_rc)
levels(stepdown_rc)

#reclassify stepdown categories into 4 conservation actions. This is just a hypothetical example for now.
#1 = Protection
#2 = Restoration
#3 = Enhancement
#4 = Persistence/retention
rcl2 <- matrix(data = c(51,4,
                        52,3,
                        1001,1,
                        1002,3,
                        1111,3,
                        1112,2,
                        2001,1,
                        2002,4), ncol=2, byrow = T)
conAction <- classify(stepdown_rc, rcl2, filename = "Output/conActionEx.tif")
conAction <- as.factor(conAction)
levels(conAction)

#changed, plowed, encrouached, shrub, and other pixels to NA to speed up processing
rcl3 <- matrix(data = c(50,NA,
                        53,NA,
                        5000,NA,
                        5500,NA), ncol=2, byrow = T)
conAc_simp <- classify(conAction, rcl3)
conAc_simp <- as.factor(conAc_simp)

#6. load JV Polygons and state/prov boundaires and count number of pixels in each category per JV X state/prov
jv <- st_read("Data/JVs/North_American_Joint_Ventures_Albers_121521_Revision.shp") %>%
  filter(JV %in% c("Prairie Pothole", "Prairie Habitat", "Northern Great Plains")) %>%
  st_transform(crs(conAction)) %>%
  select(JV)

can <- ne_states(
  country = "Canada",
  returnclass = "sf"
) %>%
  select(iso_3166_2)

us <- ne_states(
  country = "United States of America",
  returnclass = "sf"
) %>%
  select(iso_3166_2)
state <- rbind(can,us) %>%
  st_transform(crs(conAction))

jvState <- st_intersection(jv,state)
jvState$jv_state <-paste(jvState$JV,jvState$iso_3166_2, sep = "_")
jvState <- select(jvState, jv_state)

#change to SpatVect
jvStateV <- vect(jvState)

#determine number of pixels in each category for each strata
jvStateV$zone <- 1:nrow(jvStateV)
jvConAc <- freq(conAc_simp, zones = jvStateV)

#add JVState names to frequency table
jvConAc_ <- left_join(jvConAc, as.data.frame(jvStateV))
#add acres
jvConAc_$acresM <- jvConAc_$count*30*30/4047/1000000

#create polygon for JV8 for mapping purposes
jv8 <- st_read("Data/JVs/North_American_Joint_Ventures_Albers_121521_Revision.shp") %>%
  filter(JV %in% c("Prairie Pothole", "Prairie Habitat", "Northern Great Plains", "Playa Lakes", "Rainwater Basin", "Oaks and Prairies", "Rio Grande", "Sonoran"))
st_write(jv8, "Data/JVs/jv8.shp")

#5. Load and edit raw conversion risk layer, to be used to asses risk of cropland to identify restoration pixels
conRaw <- rast("Data/conRisk/gp_tillage.tif")


###############Likely not needed############################################
#3. create separate encroachment risk layer
# # 1 = low risk
# # 2 = high risk
# # 3 = encroached
# encRisk <- gamN
# values(encRisk) <- NA
# encRisk[gamN==5] <- 1 #low risk
# encRisk[gamN==111|gamN==200] <- 2 #high risk
# encRisk[gamN==550] <- 3 #encroached
# plot(encRisk)
# 
# #create separate conversion risk layer
# # 1 = low risk
# # 2 = high risk
# # 3 = plowed
# conRisk <- gamN
# values(conRisk) <- NA
# conRisk[gamN==5] <- 1 #low risk
# conRisk[gamN==100|gamN==200] <- 2 #high risk
# conRisk[gamN==500] <- 3 #plowed
# plot(conRisk)



#save risk rasters and load to save RAM
# writeRaster(encRisk, "Data/CGR_GAM/encRiskN.tif")
# writeRaster(conRisk, "Data/CGR_GAM/conRiskN.tif")
# encRisk <- rast("Data/CGR_GAM/encRiskN.tif")
# conRisk <- rast("Data/CGR_GAM/conRiskN.tif")

# #load landcover rasters
# canLUList <- list.files("Data/landcover/", pattern = "Can2020", full.names = T)
# canLUrast <- lapply(canLUList, rast)
# canLU <- do.call(mosaic, canLUrast)
# 
# usaLUList <- list.files("Data/landcover/", pattern = "USA2020", full.names = T)
# usaLUrast <- lapply(usaLUList, rast)
# usaLU <- do.call(mosaic, usaLUrast)
# lu <- mosaic(canLU, usaLU)

