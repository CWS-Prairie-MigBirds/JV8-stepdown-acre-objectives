############################################################
#JV8 conservation action raster map: raster pre processing
#By: Barry Robinson
#Feb 11, 2026
############################################################

#This scrip conducts pre processing steps to rasters representing grassland condition (Canadian Prairie Grassland Inventory and PUDL V1 and V2) and grassland risk (cropland conversion risk and woody encroachement risk),
#Pre processing includes reprojecting, resampling, mosaicing, and snapping all rasters together and binning continuous risk rasters into risk categories.
#A different dataset (or combination of datasets) for grassland condition will be used for each JV:
  #1. PHJV: Canadian Prairie Grassland Inventory
  #2. PPJV, NGPJV, OPJV, PLJV, RBJV, and SNJ: PUDL V2
  #3. RGJV: PUDL V1 (for Mexico) and PUDL V2 for USA portion

#load libraries
library(terra)
library(sf)
library(dplyr)
library(rnaturalearth)

#1. Import raw rasters
###############################
#1A. Grassland condition layers
###############################

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
#Version1 (for Canada and Mexico only)
pudl1M <- rast("Data/PUDL/RGJV_PUDLmask.tif")
pudl1C <- rast("Data/PUDL/PHJV_PUDLmask.tif")

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
#1B.Risk
#########################
#Grassland Assesement Map
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

#Cropland conversion risk (Olimb and Robinson 2019 and updated temporal risk from Sarah)
#The original Olimb and Robinson model includes risk estimates for cropland, which can be used to locate restoration pixels
#I will use the updated temporal risk model as primary source for risk on grasslands, then fill in remaining areas with original model
crisk1 <- rast("Data/conRisk/gp_tillage.tif")
crisk2 <- rast("Data/conRisk/CP_WWF_2021_Intact.tif")

#crisk1 doesn't have a proper mask. Masked pixel are 0, but there are also predictions of non-masked pixels that = 0
#crisk2 has proper noData pixels, so use as a mask on crisk1
compareGeom(crisk1, crisk2)

#cells don't match up perfectly due to slightly different origins, so need to shift and crop crisk1 to match crisk2 before masking
origin1 <- origin(crisk1)
origin2 <- origin(crisk2)
dx <- origin2[[1]] - origin1[[1]]
dy <- origin2[[2]] - origin1[[2]]
crisk1 <- shift(crisk1, dx, dy) |>
  crop(crisk2) |>
  mask(crisk2)

#Now convert pixels with a value of 0 in crisk2 (primarily cropland) to values in crisk1
crisk <- ifel(crisk2 == 0, crisk1, crisk2, filename = "Data/conRisk/criskAll.tif", overwrite = T)

#Woody encroachement risk from Dirac Twidwell and Sam. There are two sources of risk modelled separate..trees and shrub.
weriskS <- rast("Data/encRisk/R3_WC20InOnly_CGR_30m_PFG_SHR_81W_2021_Clamped_0_neg200.tif")
weriskT <- rast("Data/encRisk/R3_WC20InOnly_CGR_30m_PFG_TRE_81W_2021_Clamped _0_neg200.tif")
compareGeom(weriskS, weriskT)


#2. reproject, reclassify (if necessary), resample to 90m and snap together.
##########################################
#2A. first project to crs of GAM (except PUDL1M and PUDL1C, which is already in GAM crs)
##########################################
cpgiABNA <- project(cpgiAB, crs(gam), method = "near", filename = "Data/Temp/cpgiABNA.tif")
cpgiSKNA <- project(cpgiSK, crs(gam), method = "near", filename = "Data/Temp/cpgiSKNA.tif")
cpgiMBNA <- project(cpgiMB, crs(gam), method = "near", filename = "Data/Temp/cpgiMBNA.tif")

aciABNA <- project(aciAB, crs(gam), method = "near", filename = "Data/Temp/aciABNA.tif")
aciSKNA <- project(aciSK, crs(gam), method = "near", filename = "Data/Temp/aciSKNA.tif")
aciMBNA <- project(aciMB, crs(gam), method = "near", filename = "Data/Temp/aciMBNA.tif")

pudl2NA <- project(pudl2, crs(gam), method = "near", filename = "Data/Temp/pudl2NA.tif")

criskNA <- project(crisk, crs(gam), method = "bilinear", filename = "Data/Temp/criskNA.tif", overwrite = T) #using bilinear because this raster is continuous.

weriskS_NA <- project(weriskS, crs(gam), method = "bilinear", filename = "Data/Temp/weriskS_NA.tif") #using bilinear because this raster is continuous.
weriskT_NA <- project(weriskT, crs(gam), method = "bilinear", filename = "Data/Temp/weriskT_NA.tif") #using bilinear because this raster is continuous.

#########################################
#2B. reclassify rasters to minimize categories
#########################################
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
gam <- droplevels(gam)
writeRaster(gam,"Data/Temp/GAM.tif", overwrite = T)
#GAM is only being used to identify plowed pixels, so reclassify
plowprint <- ifel((gam == 500|gam == 800), 10000, 0, filename = "Data/Temp/plowprint.tif", overwrite = T)

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
# criskNA <- rast("Data/Temp/criskNA.tif")
# weriskS_NA <- rast("Data/Temp/weriskS_NA.tif")
# weriskT_NA <- rast("Data/Temp/weriskT_NA.tif")

######################################################
#2C. resample and snap everything to a 90m template
######################################################
#create template
pudl2NA_rcl <- rast("Data/Temp/pudl2NArcl.tif")
template <- rast(ext(gam), resolution = res(pudl2NA_rcl), crs = crs(gam))

#resample all rasters to match the template
cpgiAB90 <- resample(cpgiABNA, template, method = "mode", filename = "Data/CPGI/AB90.tif")
cpgiSK90 <- resample(cpgiSKNA, template, method = "mode", filename = "Data/CPGI/SK90.tif")
cpgiMB90 <- resample(cpgiMBNA, template, method = "mode", filename = "Data/CPGI/MB90.tif")
aciAB90 <- resample(aciABGrass, template, method = "mode", filename = "Data/ACI/aciAB90.tif", overwrite = T)
aciSK90 <- resample(aciSKGrass, template, method = "mode", filename = "Data/ACI/aciSK90.tif", overwrite = T)
aciMB90 <- resample(aciMBGrass, template, method = "mode", filename = "Data/ACI/aciMB90.tif", overwrite = T)
pudl1M_90 <- resample(pudl1M, template, method = "mode", filename = "Data/PUDL/pudl1M_90.tif")
pudl1C_90 <- resample(pudl1C, template, method = "mode", filename = "Data/PUDL/pudl1C_90.tif")
pudl2_90 <- resample(pudl2NA_rcl, template, method = "near", filename = "Data/PUDL/pudl2_90.tif") #using near method because pudl2 is already at 90m
gam90 <- resample(gam, template, method = "mode", filename = "Data/CGR_GAM/gam90.tif", overwrite = T)
plowprint90 <- resample(plowprint, template, method = "mode", filename = "Data/CGR_GAM/plowprint90.tif", overwrite = T)
crisk90 <- resample(criskNA, template, method = "bilinear", filename = "Data/conRisk/conRisk90.tif", overwrite = T) #using bilinear because this raster is continuous.
weriskS90 <- resample(weriskS_NA, template, method = "bilinear", filename = "Data/encRisk/weRiskS90.tif", overwrite = T) #using bilinear because this raster is continuous. <- resample(weriskS_NA, template, method = "bilinear", filename = "Data/conRisk/weRiskS90.tif", overwrite = T) #using bilinear because this raster is continuous.
weriskT90 <- resample(weriskT_NA, template, method = "bilinear", filename = "Data/encRisk/weRiskT90.tif", overwrite = T) #using bilinear because this raster is continuous.


#Load rasters if starting at this point
# cpgiAB90 <- rast("Data/CPGI/AB90.tif")
# cpgiSK90 <- rast("Data/CPGI/SK90.tif")
# cpgiMB90 <- rast("Data/CPGI/MB90.tif")
# aciAB90 <- rast("Data/ACI/aciAB90.tif")
# aciSK90 <- rast("Data/ACI/aciSK90.tif")
# aciMB90 <- rast("Data/ACI/aciMB90.tif")
# gam90 <- rast("Data/CGR_GAM/gam90.tif")
# pudl1M_90 <- rast("Data/PUDL/pudl1M_90.tif")
# pudl1C_90 <- rast("Data/PUDL/pudl1C_90.tif")
# pudl2_90 <- rast("Data/PUDL/pudl2_90.tif")
crisk <- rast("Data/conRisk/conRisk90.tif")
weriskS90 <- rast("Data/encRisk/weRiskS90.tif")
weriskT90 <- rast("Data/encRisk/weRiskT90.tif")
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
compareGeom(gam90, crisk90)
compareGeom(werisk90, crisk90)

#################################################
#2D. mosaic datasets together where appropriate
#################################################
#ACI
# aci90 <- mosaic(aciAB90, aciSK90, aciMB90, fun = "max", filename = "Data/ACI/aci90.tif", overwrite = T) #rasters are trinomial with value of 0 outside of each provincial boundary, so max works here
# levels(aci90)
# #CPGI
# cpgi90 <- mosaic(cpgiAB90, cpgiSK90, cpgiMB90, fun = "max", filename = "Data/CPGI/cpgi90.tif")
# levels(cpgi90)
#PUDL: PUDL1 and PUDL2 overlap and we want to use PUDL2 for CONUS portion, so use merge instead of mosaic
pudl90 <- merge(pudl2_90, pudl1C_90, pudl1M_90, filename = "Data/PUDL/PUDLall90.tif", overwrite = T)
levels(pudl90)

##############################################################################
#2E. Bin conversion and encroachment risk rasters into categories (high and low risk)
##############################################################################
#Conversion risk. Cut points obtained from Sarah Olimb
#risk >= 42.33 = high risk = 2000
#risk < 42.33 = low risk = 1000
criskBin <- ifel(crisk < 42.33, 1000, 2000, filename = "Data/conRisk/crisk90Bin.tif", overwrite = T)

#encroachment risk. Cut points obtained from Sam Cady (see "Data/encRisk/README...rtf file)
#Shrub risk classes
#-200 < risk <= -75 = encroached = 300
#-75 < risk <= -25 = high risk = 200
#-25 < risk <= 0 = low risk = 100
#create from, to, becomes reclass matrix
rclShrub <- matrix(c(-201, -75, 300,
                     -75,-25,200,
                     -25,0,100),
                   nrow = 3,
                   byrow = T)
weriskS_Bin <- classify(weriskS90, rcl=rclShrub, filename = "Data/encRisk/weRisk_Sh_90Bin.tif", overwrite = T)

#Tree risk classes
#-200 < risk <= -75 = encroached = 30
#-75 < risk <= -15 = high risk = 20
#-15 < risk <= 0 = low risk = 10
#create from, to, becomes reclass matrix
rclTree <- matrix(c(-201, -75, 30,
                     -75,-15,20,
                     -15,0,10),
                   nrow = 3,
                   byrow = T)
weriskT_Bin <- classify(weriskT90, rcl=rclTree, filename = "Data/encRisk/weRisk_Tr_90Bin.tif", overwrite = T)
