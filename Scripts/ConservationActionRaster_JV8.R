########################################
#JV8 conservation action raster map
#By: Barry Robinson
#Feb 11, 2026
########################################

#This scrip uses datasets for grassland condition (Canadian Prairie Grassland Inventory and PUDL V1 and V2) and grassland risk (cropland conversion risk and woody encroachement risk),
#along with information from individual Habitat Joint Venture to identify the ideal conservation action needed for each pixel to maintain grassland cover over time.
#A different dataset (or combination of datasets) for grassland condition will be used for each JV:
  #1. PHJV: Canadian Prairie Grassland Inventory
  #2. PPJV, NGPJV, OPJV, PLJV, RBJV, and SNJ: PUDL V2
  #3. RGJV: PUDL V1 (for Mexico) and PUDL V2 for USA portion

#1. load libraries
library(terra)
library(sf)
library(dplyr)
library(rnaturalearth)

#1. Import raw rasters, reproject, reclassify (if necessary) and snap together. THIS ONLY NEEDS TO BE DONE ONCE. IF ALREADY COMPLETE, SKIP TO #2 
###########################
#Grassland condition layers
###########################

#Canadian Prairie Grassland Inventory
cpgiAB <- rast("Data/CPGI/AB.tif")
cpgiSK <- rast("Data/CPGI/SK.tif")
cpgiMB <- rast("Data/CPGI/MB.tif")

#CPGI Attribute Table
#0 = Other cover
#2 = Tame
#3 = Native
#5 = Mixed

#Annual Crop Inventory
#CPGI seems to miss-classify some non-grass cover (e.g.urban, water, wetland, shrub, crop, and forest) as various types of grass
#Use the 2024 Annual Crop Inventory to clip out non-grass cover types
aciAB <- rast("Data/ACI/aci_2024_ab_v2.tif")
aciSK <- rast("Data/ACI/aci_2024_sk_v2.tif")
aciMB <- rast("Data/ACI/aci_2024_mb_v2.tif")



#Potentially Undistureb Land layers
#Version1 (for Mexico only)
pudl1 <- rast("Data/PUDL/RGJV_PUDLmask.tif")

#PUDL1 Attribute Table
# 0 = Potentially disturbed/other cover
# 1 = Potentially undisturbed grass
# 2 = Potentially disturbed grass
# 3 = Shrub

#Version 2 (for CONUS)
pudl2 <- rast("Data/PUDL/PUDL_NewCONUS/PotentiallyUndisturbedGrassShrbBareCover.tif")

#PUDL2 Attribute Table
# 0 = Potentially disturbed/other cover
# 1 = Bare
# 2 = Potentially disturbed grass
# 3 = Potentially undisturbed grass
# 4 = Shrub

#########################
#Risk
#########################
gam <- rast("Data/CGR_GAM/CGR_GAM_V2_separated.tif")

#GAM Attribute Table
# 0 = NA mask
# 5 = low conversion risk/low encroachment risk
# 100 = high conversion risk
# 111 = high encroachment risk
# 200 = high conversion risk/ high encroachment risk
# 500 = plowed
# 550 = encroached
# 800 = plowed and encroached
# 1000 = Other cover

#first project to crs of GAM (except PUDL1, which is already in GAM crs)
cpgiABNA <- project(cpgiAB, crs(gam), method = "near", filename = "Data/Temp/cpgiABNA.tif")
cpgiSKNA <- project(cpgiSK, crs(gam), method = "near", filename = "Data/Temp/cpgiSKNA.tif")
cpgiMBNA <- project(cpgiMB, crs(gam), method = "near", filename = "Data/Temp/cpgiMBNA.tif")

aciABNA <- project(aciAB, crs(gam), method = "near", filename = "Data/Temp/aciABNA.tif")
aciSKNA <- project(aciSK, crs(gam), method = "near", filename = "Data/Temp/aciSKNA.tif")
aciMBNA <- project(aciMB, crs(gam), method = "near", filename = "Data/Temp/aciMBNA.tif")

pudl2NA <- project(pudl2, crs(gam), method = "near", filename = "Data/Temp/pudl2NA.tif")

#reclassify rasters to minimize categories
#Canadian Prairie Grassland Inventory


#Annual Crop Inventory
#Used as a mask to clip out other cover types, so only need 3 classes: native grass (110), tame grass (122) and non-grass (all other values)
aciABGrass <- 0 * aciABNA
aciABGrass[aciABNA == 110] <- 1
aciABGrass[aciABNA == 122] <- 2

aciSKGrass <- 0 * aciSKNA 
aciSKGrass[aciSKNA == 110] <- 1
aciSKGrass[aciSKNA == 122] <- 2

aciMBGrass <- 0 * aciMBNA
aciMBGrass[aciMBNA == 110] <- 1
aciMBGrass[aciMBNA == 122] <- 2

writeRaster(aciABGrass, "Data/Temp/aciABGrass.tif", overwrite = T)
writeRaster(aciSKGrass, "Data/Temp/aciSKGrass.tif", overwrite = T)
writeRaster(aciMBGrass, "Data/Temp/aciMBGrass.tif", overwrite = T)

#GAM
#change mask pixels to NA
gam[gam == 0] <- NA
#change plowed and encroached to plowed to minimize categories
gam[gam == 800] <- 500
gam <- droplevels(gam)

writeRaster(gam,"Data/Temp/GAM.tif", overwrite = T)


#PUDL (1 doesn't need any reclassifying)
#add bare ground to potentially disturbed/other category and change to match pudl1
#reclass matrix "is, becomes"
rcl <- matrix(data = c(1,0,
                       3,1,
                       4,3), 
              byrow = T, ncol = 2)
pudl2NA_rcl <- classify(pudl2NA, rcl = rcl, filename = "Data/Temp/pudl2NArcl.tif")

#Load rasters again if starting a separate session
# cpgiABNA <- rast("Data/Temp/cpgiABNA.tif")
# cpgiSKNA <- rast("Data/Temp/cpgiSKNA.tif")
# cpgiMBNA <- rast("Data/Temp/cpgiMBNA.tif")
# aciABGrass <- rast("Data/Temp/aciABGrass.tif")
# aciSKGrass <- rast("Data/Temp/aciSKGrass.tif")
# aciMBGrass <- rast("Data/Temp/aciMBGrass.tif")
# gam <- rast("Data/Temp/GAM.tif")
# pudl2NA_rcl <- rast("Data/Temp/pudl2NArcl.tif")

#create 90m template raster to resample/snap everything to
template <- rast(ext(gam), resolution = res(pudl2NA_rcl), crs = crs(gam))

#resample all rasters to match the template
cpgiAB90 <- resample(
  cpgiABNA,
  template,
  method = "mode",
  filename = "Data/CPGI/AB90.tif"
)

cpgiSK90 <- resample(
  cpgiSKNA,
  template,
  method = "mode",
  filename = "Data/CPGI/SK90.tif"
)

cpgiMB90 <- resample(
  cpgiMBNA,
  template,
  method = "mode",
  filename = "Data/CPGI/MB90.tif"
)

aciAB90 <- resample(
  aciABGrass,
  template,
  method = "mode",
  filename = "Data/ACI/aciAB90.tif", 
  overwrite = T
)

aciSK90 <- resample(
  aciSKGrass,
  template,
  method = "mode",
  filename = "Data/ACI/aciSK90.tif", 
  overwrite = T
)

aciMB90 <- resample(
  aciMBGrass,
  template,
  method = "mode",
  filename = "Data/ACI/aciMB90.tif", 
  overwrite = T
)

gam90 <- resample(
  gam,
  template,
  method = "mode",
  filename = "Data/CGR_GAM/gam90.tif", 
  overwrite = T
)

pudl1_90 <- resample(
  pudl1,
  template,
  method = "mode",
  filename = "Data/PUDL/pudl1_90.tif"
)

pudl2_90 <- resample(
  pudl2NA_rcl,
  template,
  method = "near",
  filename = "Data/PUDL/pudl2_90.tif"
)


#Load rasters if starting at this point
# cpgiAB90 <- rast("Data/CPGI/AB90.tif")
# cpgiSK90 <- rast("Data/CPGI/SK90.tif")
# cpgiMB90 <- rast("Data/CPGI/MB90.tif")
# aciAB90 <- rast("Data/ACI/aciAB90.tif")
# aciSK90 <- rast("Data/ACI/aciSK90.tif")
# aciMB90 <- rast("Data/ACI/aciMB90.tif")
# gam90 <- rast("Data/CGR_GAM/gam90.tif")
# pudl1_90 <- rast("Data/PUDL/pudl1_90.tif")
# pudl2_90 <- rast("Data/PUDL/pudl2_90.tif")
# gc()

#ensure geometries match completely across all layers
compareGeom(gam90, pudl1_90)
compareGeom(gam90, pudl2_90)
compareGeom(pudl1_90, pudl2_90)
compareGeom(cpgiAB90, gam90)
compareGeom(cpgiSK90, gam90)
compareGeom(cpgiMB90, gam90)
compareGeom(aciAB90, gam90)
compareGeom(aciSK90, gam90)
compareGeom(aciMB90, gam90)

#mosaic datasets together where appropriate
#ACI
aci90 <- mosaic(aciAB90, aciSK90, aciMB90, fun = "max", filename = "Data/ACI/aci90.tif", overwrite = T) #rasters are trinomial with value of 0 outside of each provincial boundary, so max works here
levels(aci90)
#CPGI
cpgi90 <- mosaic(cpgiAB90, cpgiSK90, cpgiMB90, fun = "max", filename = "Data/CPGI/cpgi90.tif")
levels(cpgi90)
#PUDL: PUDL1 and PUDL2 overlap and we want to use PUDL2 for CONUS, so use merge instead of mosaic
pudl90 <- merge(pudl2_90, pudl1_90, filename = "Data/PUDL/PUDLall90.tif")
levels(pudl90)

#Load rasters if starting at this point
aci90 <- rast("Data/ACI/aci90.tif")
cpgi90 <- rast("Data/CPGI/cpgi90.tif")
gam90 <- rast("Data/CGR_GAM/gam90.tif")
pudl90 <- rast("Data/PUDL/PUDLall90.tif")

#2. Combine grassland condition and grassland risk rasters to get all unique combinations

#Canada
#First restrict CPGI to those pixels defines as grass by ACI
#see which areas are defined as grass by CPGI, but not ACI. 
canTest <- aci90
canTest[(aci90==0 & cpgi90 >0)] <- 1
canTest[(aci90==1 & cpgi90>0)] <- 0
writeRaster(canTest, filename="Output/CanTest.tif")
#Test which pixels are classified as native by ACI, but tame or mixed by CPGI

#CPGI often mis-classifies certain crop types (e.g Spring wheat, barley, lentils) and shrub as tame grass. But I do see cases where ACI classifies tame grass as native
#I will use CPGI to classify grass as native or tame in areas that ACI classifies as either native or tame.




#USA
#Use formula to create unique values for all combinations
grassUSA <- app(c(gam90, pudl90), fun = function(x) {
  if (any(is.na(x))) {NA}
  else {
    x[1] * 10 + x[2]
  }
}, 
filename = "Output/grassCatUSA-M.tif", 
overwrite = T)

#change to factor and remove levels that do not occur
grassUSA <- rast("Output/grassCatUSA-M.tif")
grassUSA <- as.factor(grassUSA)
levels(grassUSA)
grassUSA <- droplevels(grassUSA)
levels(grassUSA)

#Raster attribute table of grassUSA
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

levels <- unname(unlist(levels(grassUSA)[[1]][1]))
rat <- data.frame(
  value = levels,
  conRisk = c("low", "low", "low", "low", 
              "high", "high", "high", "high", 
              "low", "low", "low", "low", 
              "high", "high", "high", "high", 
              "plowed", "plowed", "plowed", "plowed", 
              "encroached", "encroached", "encroached", "encroached",
              "other cover", "other cover", "other cover", "other cover"),
  encRisk = c("low", "low", "low", "low",
              "low", "low", "low", "low",
              "high", "high", "high", "high",
              "high", "high", "high", "high",
              "plowed", "plowed", "plowed", "plowed", 
              "encroached", "encroached", "encroached", "encroached",
              "other cover", "other cover", "other cover", "other cover"),
  PUDL = c("potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub",
           "potentially disturbed/other", "undistrubed grass", "distrubed grass", "Shrub")
)

#look at frequency of pixel across the entire study area
catFreq <- freq(grassUSA)
#add to rat table and convert to millions of acres
rat$count <- catFreq$count
rat$acreMil <- rat$count * prod(res(grassUSA)) / 4047 /1000000

#reclassify into as few categories as possible
#50: all potentially disturbed/other into other cover, except those defined as plowed (5000)

rcl <- matrix(data = c(1000,50,
                       1110,50,
                       2000,50,
                       5500,50,
                       10000,50,
                       
                       5001,5000,
                       5002,5000,
                       5003,5000,
                       5501,5500,
                       5502,5500,
                       5503,5500,
                       8000,5000,
                       8001,5000,
                       8002,5000,
                       8003,5000,
                       1003,53,
                       1113,53,
                       2003,53), ncol=2, byrow = T)






