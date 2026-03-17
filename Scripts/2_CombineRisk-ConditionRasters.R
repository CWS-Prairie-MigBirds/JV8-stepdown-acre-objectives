############################################################
#JV8 conservation action raster map: combine risk and condition rasters
#By: Barry Robinson
#March 17, 2026
############################################################

#This script uses output from 1_RasterPreProcessing.R and combines them into a single raster so each pixel represents both grassland condition risk level
#This final raster will be used to assign conservation actions to each category of grass

#load libraries
library(terra)
library(sf)
library(dplyr)
library(rnaturalearth)

#1. Import processed rasters
#Load rasters if starting at this point
# aci90 <- rast("Data/ACI/aci90.tif")
# cpgi90 <- rast("Data/CPGI/cpgi90.tif")
# gam90 <- rast("Data/CGR_GAM/gam90.tif")
pudl90 <- rast("Data/PUDL/PUDLall90.tif")
criskBin <- rast("Data/conRisk/crisk90Bin.tif")
weriskS_Bin <- rast("Data/encRisk/weRisk_Sh_90Bin.tif")
weriskT_Bin <- rast("Data/encRisk/weRisk_Tr_90Bin.tif")

#Canada
#First restrict CPGI to those pixels defines as grass by ACI
#see which areas are defined as grass by CPGI, but not ACI. 
# canTest <- aci90
# canTest[(aci90==0 & cpgi90 >0)] <- 1
# canTest[(aci90==1 & cpgi90>0)] <- 0
# writeRaster(canTest, filename="Output/CanTest.tif")
#Test which pixels are classified as native by ACI, but tame or mixed by CPGI

#CPGI often mis-classifies certain crop types (e.g Spring wheat, barley, lentils) and shrub as tame grass. But I do see cases where ACI classifies tame grass as native
#I will use CPGI to classify grass as native or tame in areas that ACI classifies as either native or tame.



#Use formula to create unique values for all combinations
stepdownG <- app(c(gam90, pudl90), fun = function(x) {
  if (any(is.na(x))) {NA}
  else {
    x[1] * 10 + x[2]
  }
}, 
filename = "Output/stepdownG.tif", 
overwrite = T)

#change to factor and remove levels that do not occur
stepdownG <- rast("Output/stepdownG.tif")
stepdownG <- as.factor(stepdownG)
levels(stepdownG)
stepdownG <- droplevels(stepdownG)
levels(stepdownG)
writeRaster(stepdownG, filename = "Output/stepdownG.tif", overwrite = T)

#Raster attribute table of stepdownG
#ConRisk/EncRisk/GrassType
#50   = low/low/Other cover
#51   = low/low/native
#52   = low/low/tame
#53   = low/low/shrub
#1000 = high/low/other
#1001 = high/low/native
#1002 = high/low/tame
#1003 = high/low/shrub
#1110 = low/high/other
#1111 = low/high/native
#1112 = low/high/tame
#1113 = low/high/shrub
#2000 = high/high/other
#2001 = high/high/native
#2002 = high/high/tame
#2003 = high/high/shrub
#5000 = plowed/other
#5001 = plowed/native
#5002 = plowed/tame
#5003 = plowed/shrub
#5500 = encroached/other
#5501 = encroached/native
#5502 = encroached/tame
#5503 = encroached/shrub
#8000 = plowed/encroached/other
#8001 = plowed/encroached/native
#8002 = plowed/encroached/tame
#8003 = plowed/encroached/shrub
#10000 = other cover/other cover
#10001 = other cover/native
#10002 = other cover/tame
#10003 = other cover/shrub

levels <- unname(unlist(levels(stepdownG)[[1]][1]))
rat <- data.frame(
  value = levels,
  conRisk = c("low", "low", "low", "low", 
              "high", "high", "high", "high", 
              "low", "low", "low", "low", 
              "high", "high", "high", "high", 
              "plowed", "plowed", "plowed", "plowed", 
              "encroached", "encroached", "encroached", "encroached",
              "plowed/encroached", "plowed/encroached", "plowed/encroached", "plowed/encroached",
              "other cover", "other cover", "other cover", "other cover"),
  encRisk = c("low", "low", "low", "low",
              "low", "low", "low", "low",
              "high", "high", "high", "high",
              "high", "high", "high", "high",
              "plowed", "plowed", "plowed", "plowed", 
              "encroached", "encroached", "encroached", "encroached",
              "plowed/encroached", "plowed/encroached", "plowed/encroached", "plowed/encroached",
              "other cover", "other cover", "other cover", "other cover"),
  PUDL = c("potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub")
)

#look at frequency of pixel across the entire study area
catFreq <- freq(stepdownG)
#add to rat table and convert to millions of acres
rat$count <- catFreq$count
rat$acreMil <- rat$count * prod(res(stepdownG)) / 4047 /1000000
write.csv(rat, "Output/RAT_stepdownG.csv", row.names = F)

#reclassify into as few categories as possible

#First just a hypothetical reclass for simple example for presentation. The below reclassification will not work because of disagreement between GAM and PUDL.  
#create reclassify matrix (is-becomes) to simplify stepdown raster to less categories
#Here are the categories I'm combining
#1. 5000: all plowed pixels into 1
#2. 5500: all encroached pixels into 1
#3. 5000: all plowed/encroached into plowed (for simplicity)
#4. 50: all potentially disturbed/other into other cover
#5. 53: all shrub into 1 (also including 10001 and 10002, which seems to be mostly shrubland, especially in Mexico and southern USA)

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
                       10000,50,
                       1003,53,
                       1113,53,
                       2003,53,
                       10001,53,
                       10002,53,
                       10003,53), ncol=2, byrow = T)

#reclassify to minimize number of categories
stepdown_rc <- classify(stepdownG, rcl, filename = "Output/stepdownG_rc.tif", overwrite = T)
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
                        2002,4,
                        5500,2), ncol=2, byrow = T)
conAction <- classify(stepdown_rc, rcl2, filename = "Output/conActionEx2.tif", overwrite = T)
conAction <- as.factor(conAction)
levels(conAction)

#changed, plowed, encrouached, shrub, and other pixels to NA to speed up processing
rcl3 <- matrix(data = c(50,NA,
                        53,NA,
                        5000,NA,
                        5500,NA), ncol=2, byrow = T)
conAc_simp <- classify(conAction, rcl3, filename = "Output/conActionGrassOnly.tif", overwrite = T)
conAc_simp <- as.factor(conAc_simp)

#6. load JV Polygons and state/prov boundaires and count number of pixels in each category per JV X state/prov
jv <- st_read("Data/JVs/North_American_Joint_Ventures_Albers_121521_Revision.shp") %>%
  filter(JV %in% c("Prairie Pothole", "Prairie Habitat", "Northern Great Plains", "Playa Lakes", "Rainwater Basin", "Oaks and Prairies", "Rio Grande", "Sonoran")) %>%
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

mex <- ne_states(
  country = "Mexico",
  returnclass = "sf"
) %>%
  select(iso_3166_2)

state <- rbind(can, us, mex) %>%
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
jvConAc_$acres <- jvConAc_$count*prod(res(conAction))/4047

#isolate RGJV
rgjvAcres <- jvConAc_[grepl("Rio Grande", jvConAc_$jv_state), ]

#create polygon for JV8 for mapping purposes
jv8 <- st_read("Data/JVs/North_American_Joint_Ventures_Albers_121521_Revision.shp") %>%
  filter(JV %in% c("Prairie Pothole", "Prairie Habitat", "Northern Great Plains", "Playa Lakes", "Rainwater Basin", "Oaks and Prairies", "Rio Grande", "Sonoran"))
st_write(jv8, "Data/JVs/jv8.shp")

#5. Load and edit raw conversion risk layer, to be used to asses risk of cropland to identify restoration pixels
conRaw <- rast("Data/conRisk/gp_tillage.tif")