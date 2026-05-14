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
library(stringr)

#1. Import processed rasters and manipulate as needed
#Load rasters if starting at this point
# aci90 <- rast("Data/ACI/aci90.tif")
# cpgi90 <- rast("Data/CPGI/cpgi90.tif")
gam90 <- rast("Data/CGR_GAM/gam90.tif")
pp90 <- rast("Data/CGR_GAM/plowprint90.tif")
pudl90 <- rast("Data/PUDL/PUDLall90.tif")
criskBin <- rast("Data/conRisk/crisk90Bin.tif")
weriskS_Bin <- rast("Data/encRisk/weRisk_Sh_90Bin.tif")
weriskT_Bin <- rast("Data/encRisk/weRisk_Tr_90Bin.tif")

#restrict the analysis to areas where jv8, pudl, plowprint, and WE risk have values. Conversion risk has a more restrictive geographic focus (no mexico), but I'll assume 
#all grass pixels in Mexico are at risk
#Only run the below once.
#mask <- ifel(!is.na(pudl90) & !is.na(pp90) & !is.na(jvRast), 1, NA, filename = "Output/stepdownMask.tif", overwrite = T)

#load mask if the above has already been run
mask <- rast("Output/stepdownMask.tif")

#rasterize JV/state polygons
jv <- st_read("Data/JVs/North_American_Joint_Ventures_Albers_121521_Revision.shp") %>%
  filter(JV %in% c("Prairie Pothole", "Prairie Habitat", "Northern Great Plains", "Playa Lakes", "Rainwater Basin", "Oaks and Prairies", "Rio Grande", "Sonoran")) %>%
  st_transform(crs(pp90)) %>%
  select(JV)

can <- ne_states(
  country = "Canada",
  returnclass = "sf") %>%
  select(iso_3166_2)

us <- ne_states(
  country = "United States of America",
  returnclass = "sf") %>%
  select(iso_3166_2) 

mex <- ne_states(
  country = "Mexico",
  returnclass = "sf") %>%
  select(iso_3166_2)

state <- rbind(can, us, mex) %>%
  st_transform(crs(gam90))

jvState <- st_intersection(jv,state) %>%
  mutate(jv_state = paste(JV, iso_3166_2, sep = "_")) %>%
  select(JV, jv_state)

jvRast <- rasterize(jvState, pp90, field="jv_state") %>%
  mask(mask, filename = "Data/JVs/jv8stateRast.tif", overwrite = T)

jvRast <- mask(jvRast, mask, filename = "Data/JVs/jv8stateRastMask.tif", overwrite = T)

#rasterize USFWS region polygon, also including Canada and Mexico, intersected with JV polygon
fws <- st_read("Data/USFWS_regions/FWS_Legacy_Regional_Boundaries.shp") %>%
  filter(!REGION %in% c(7,1,8,5)) %>% #removing regions that are clearly outside JV8
  st_transform(crs(pp90)) %>%
  select(REGNAME)

can_union <- can |>
  summarise(geometry = st_union(geometry)) %>%
  mutate(REGNAME = "Canada") %>%
  select(REGNAME, geometry)
mex_union <- mex |>
  summarise(geometry = st_union(geometry)) %>%
  mutate(REGNAME = "Mexico") %>%
  select(REGNAME, geometry)

canmex <- rbind(can_union, mex_union) %>%
  st_transform(crs(pp90))

fws_canmex <- rbind(fws,canmex)

fwsJV <- st_intersection(fws_canmex, jv) %>%
  mutate(fws_jv = paste(REGNAME, JV, sep = "_"))

fwsRast <- rasterize(fwsJV, pp90, field = "fws_jv") %>%
  mask(mask, filename = "Data/USFWS_regions/fwsJVRast.tif")

#areas missing from crisk are primarily in Mexico. After chatting with Arvind Punjabi, I've decided to assume all remaining grasslands in Mexico are at a high risk of conversion
#There are a lot of pixels outside of Mexico that are missing conversion risk predictions. Here's what I'm thinking:
levels(jvRast)
  #1. Canada (PHJV = 3): all pixels with missing conversion risk predictions get assigned to low risk (mainly lakes, rugged coulees, river valleys, existing protected areas)
  #2. USA: Pixels on the west side of the JV8 (west of western border of conversion risk mode) get low risk. All pixels on the east side get high risk. I can spatially query by JV/State
      #PPJV-Montana/ND/SD: Low risk; NGPJV: low risk; PLJV: low risk; OPJV: low risk; PPJV/Minnesota: high risk
  #3. Mexico: all grasslands high risk, shrublands low risk

criskBinFill <- ifel(is.na(criskBin) & jvRast %in% c(0:19,26,27,29,44,53), 1000, #NGPJV, OPJV, PLJV, PHJV, PPJV-Montana/ND/SD, RGJV-Texas, SJV-Arizona
                 ifel(is.na(criskBin) & jvRast== 25, 2000, #PPJV/Minnesota
                    ifel(is.na(criskBin) & jvRast %in% c(32:42) & pudl90 %in% c(1,2), 2000, #RGJV-Mexico grassland
                        ifel(is.na(criskBin) & jvRast %in% c(32:42) & pudl90 %in% c(0,3), 1000, criskBin))), #RGJV-Mexico shrubland and other
                 filename = "Data/conRisk/criskBinFill.tif", overwrite = T)

plot(criskBinFill)
rm(stepdownStack, sdRiskCover)
#stack all rasters and mask
stepdownStack <- c(pp90, pudl90, criskBinFill, weriskS_Bin, weriskT_Bin) %>%
  mask(mask, filename = "Output/stepdownStack.tif", overwrite = T)

#2.Combine rasters to get all unique combinations
#Raster category values are designed to that rasters can simply be summed
sdRiskCover <- sum(stepdownStack, filename = "Output/sdRiskCover.tif", overwrite = T)
sdRiskCover <- as.factor(sdRiskCover)
levels(sdRiskCover)
plot(sdRiskCover)

#Create Raster attribute table of stepdownG
#extract level values, add leading 0's and change to character
vals <- levels(sdRiskCover)[[1]]$ID |>
  sprintf(fmt = "%05d")

#Parse out pixel codes into categories
RAT <- as.data.frame(do.call(rbind, strsplit(vals, ""))) %>%
  mutate(ID = vals,
         plowed = recode(V1,
                         "0" = "not plowed",
                         "1" = "plowed"),
         conRisk = recode(V2,
                          "1" = "low",
                          "2" = "high"),
         ShrRisk = recode(V3,
                          "1" = "low",
                          "2" = "high",
                          "3" = "encroached"),
         TrRisk = recode(V4,
                         "1" = "low",
                         "2" = "high",
                         "3" = "encroached"),
         Grass = recode(V5,
                        "0" = "disturbed/other",
                        "1" = "undistrubed grass", 
                        "2" = "distrubed grass", 
                        "3" = "shrub")
         ) %>%
  select(ID, plowed, conRisk, ShrRisk, TrRisk, Grass) %>%
  mutate(encRisk = ifelse((ShrRisk == "encroached" | TrRisk =="encroached") & Grass != "shrub", "encroached", 
                          ifelse((ShrRisk == "high" | TrRisk == "high") & Grass != "shrub", "high",
                                 ifelse(Grass == "shrub" & TrRisk == "high", "high",
                                        ifelse(Grass == "shrub" & TrRisk == "encroached", "encroached", "low")))))
 


#3. reclassify into as few categories as possible
#create final attribute table and table summarizing area of each category in each JV and state/province

#pull IDs of categories that will be lumped together
#Plowed: 1
IDplowed <- RAT %>%
  filter(plowed == "plowed" & Grass == "disturbed/other") %>%
  pull(ID) %>%
  as.numeric() %>%
  cbind(rep(1, length(.)))

#disturbed/other land: 2
IDother <- RAT %>%
  filter((plowed == "not plowed" & Grass == "disturbed/other")) %>%
  pull(ID) %>% 
  as.numeric() %>%
  cbind(rep(2, length(.)))

#low con, low enc, undisturbed grass: 115
IDLowLowUnd <- RAT %>%
  filter((conRisk == "low" & encRisk == "low" & Grass == "undistrubed grass")) %>%
  pull(ID) %>% 
  as.numeric() %>%
  cbind(rep(115, length(.)))
#low con, low enc, disturbed grass: 116
IDLowLowDis <- RAT %>%
  filter((conRisk == "low" & encRisk == "low" & Grass == "distrubed grass")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(116, length(.)))
#low con, low enc, shrub: 117
IDLowLowShr <- RAT %>%
  filter((conRisk == "low" & TrRisk == "low" & Grass == "shrub")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(117, length(.)))

#high con, low enc, undisturbed grass: 215
IDHigLowUnd <- RAT %>%
  filter((conRisk == "high" & encRisk == "low" & Grass == "undistrubed grass")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(215, length(.)))
#high con, low enc, disturbed grass: 216
IDHigLowDis <- RAT %>%
  filter((conRisk == "high" & encRisk == "low" & Grass == "distrubed grass")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(216, length(.)))
#high con, low enc, shrub:217
IDHigLowShr <- RAT %>%
  filter((conRisk == "high" & TrRisk == "low" & Grass == "shrub")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(217, length(.)))

#high con, high enc, undisturbed grass: 225
IDHigHigUnd <- RAT %>%
  filter((conRisk == "high" & encRisk == "high" & Grass == "undistrubed grass")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(225, length(.)))
#high con, high enc, disturbed grass:226
IDHigHigDis <- RAT %>%
  filter((conRisk == "high" & encRisk == "high" & Grass == "distrubed grass")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(226, length(.)))
#high con, high enc, shrub: 227
IDHigHigShr <- RAT %>%
  filter((conRisk == "high" & TrRisk == "high" & Grass == "shrub")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(227, length(.)))

#low con, high enc, undisturbed grass: 125
IDLowHigUnd <- RAT %>%
  filter((conRisk == "low" & encRisk == "high" & Grass == "undistrubed grass")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(125, length(.)))
#low con, high enc, disturbed grass: 126
IDLowHigDis <- RAT %>%
  filter((conRisk == "low" & encRisk == "high" & Grass == "distrubed grass")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(126, length(.)))
#low con, high enc, shrub: 127
IDLowHigShr <- RAT %>%
  filter((conRisk == "low" & TrRisk == "high" & Grass == "shrub")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(127, length(.)))

#low con, encroached, undisturbed grass: 135
IDLowEncUnd <- RAT %>%
  filter((conRisk == "low" & encRisk == "encroached" & Grass == "undistrubed grass")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(135, length(.)))
#low con, encroached, disturbed grass: 136
IDLowEncDis <- RAT %>%
  filter((conRisk == "low" & encRisk == "encroached" & Grass == "distrubed grass")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(136, length(.)))
#low con, encroached, shrub: 137
IDLowEncShr <- RAT %>%
  filter((conRisk == "low" & encRisk == "encroached" & Grass == "shrub")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(137, length(.)))

#high con, encroached, undisturbed grass: 235
IDHigEncUnd <- RAT %>%
  filter((conRisk == "high" & encRisk == "encroached" & Grass == "undistrubed grass")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(235, length(.)))
#high con, encroached, disturbed grass: 236
IDHigEncDis <- RAT %>%
  filter((conRisk == "high" & encRisk == "encroached" & Grass == "distrubed grass")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(236, length(.)))
#high con, encroached, shrub: 237
IDHigEncShr <- RAT %>%
  filter((conRisk == "high" & encRisk == "encroached" & Grass == "shrub")) %>%
  pull(ID)  %>% 
  as.numeric() %>%
  cbind(rep(237, length(.)))


#create reclass matrix
rcl <- rbind(IDplowed, IDother, IDLowLowUnd, IDLowLowDis, IDLowLowShr, 
             IDHigLowUnd, IDHigLowDis, IDHigLowShr, IDHigHigUnd, 
             IDHigHigDis, IDHigHigShr, IDLowHigUnd, IDLowHigDis, IDLowHigShr,
             IDLowEncUnd, IDLowEncDis, IDLowEncShr, IDHigEncUnd, IDHigEncDis,
             IDHigEncShr)

#make sure all 144 categories are accounted for
setdiff(as.numeric(RAT$ID), rcl[,1])

#reclassigy
sdRiskCover_rcl <- classify(sdRiskCover, rcl = rcl, filename = "Output/Final/sdRiskCover_JV8.tif", overwrite = T)
plot(sdRiskCover_rcl)

#load again if starting here
sdRiskCover_rcl <- rast("Output/Final/sdRiskCover_JV8.tif") %>%
  as.factor()
levels(sdRiskCover_rcl)

#parse out categories from updated codes
vals <- levels(sdRiskCover_rcl)[[1]]$ID |>
  sprintf(fmt = "%03d")

RAT_final <- as.data.frame(do.call(rbind, strsplit(vals, ""))) %>%
  mutate(ID = vals,
         ConRisk = recode(V1,
                         "0" = "NA",
                         "1" = "low",
                         "2" = "high"),
         EncRisk = recode(V2,
                          "0" = "NA",
                          "1" = "low",
                          "2" = "high",
                          "3" = "encroached"),
         Cover = recode(V3,
                          "1" = "crop",
                          "2" = "other",
                          "5" = "undistured grass",
                          "6" = "disturbed grass",
                          "7" = "shrub")
  ) %>%
  select(ID, ConRisk, EncRisk, Cover) %>%
  mutate(ID = as.numeric(ID))

#calculate number of pixels within each category that occur within each JVXstate/province boundary
catFreq <- crosstab(c(sdRiskCover_rcl, jvRast), long = T)

#modify and join to RAT
acresJVXstate <- catFreq %>%
  filter(!(jv_state %in% c("Northern Great Plains_US-NE", "Oaks and Prairies_US-KS", "Playa Lakes_US-SD", "Playa Lakes_US-WY", #remove slivers of invalid jvXstates 
                         "Prairie Pothole_CA-AB", "Prairie Pothole_CA-MB", "Prairie Pothole_CA-SK", "Prairie Pothole_US-NE", 
                         "Rainwater Basin_US-SD", "Rio Grande_MX-GUA"))) %>%
  rename(ID = sum) %>%
  mutate(acreMil = n * prod(res(sdRiskCover_rcl)) / 4047 /1000000) %>% #calculate area in millions of acres
  left_join(RAT_final) %>% #join with RAT
  select(jv_state, ConRisk, EncRisk, Cover, ID, acreMil) %>% #select and reorder columns
  arrange(jv_state)

#Sum area within each JV
acresJV <- acresJVXstate %>%
  mutate(JV = str_extract(jv_state, "^[^_]+")) %>%
  group_by(JV, ConRisk, EncRisk, Cover, ID) %>%
  summarize(acreMil = sum(acreMil, na.rm = TRUE), .groups = "drop") %>%
  arrange(JV, ID)

#calcumate number of pixels within each catagory that occur within each USFWS region and Canada and Mexico
fwsRast <- rast("Data/USFWS_regions/fwsJVRast.tif")
catFreq_fws <- crosstab(c(sdRiskCover_rcl, fwsRast), long = T)

#modify and join to RAT
acresFWS <- catFreq_fws %>%
  filter(!(fws_jv %in% c("Canada_Prairie Pothole", "Mountain Prairie Region_Oaks and Prairies"))) %>%
  rename(ID = sum) %>%
  mutate(acreMil = n * prod(res(sdRiskCover_rcl)) / 4047 /1000000,
         ID = as.numeric(ID)) %>% #calculate area in millions of acres
  left_join(RAT_final) %>% #join with RAT
  select(fws_jv, ConRisk, EncRisk, Cover, ID, acreMil) %>% #select and reorder columns
  arrange(fws_jv)

#4. Export Raster Attribute table and area summary tables
#Export stepdown risk-cover rasters for all of JV8 and each JV
write.csv(RAT_final, "Output/Final/sdRiskCrop_IDs.csv", row.names = F)
write.csv(acresJV, "Output/Final/RiskCropAcres_jv.csv", row.names = F)
write.csv(acresJVXstate, "Output/Final/RiskCropAcres_jvXstate.csv", row.names = F)
write.csv(acresFWS, "Output/Final/RiskCropAcres_fws.csv", row.names = F)

#crop and mask to each JV and export
#get JV names and raster ids
jvlist <- unique(acresJV$JV)
jv_RAT <- levels(jvRast)[[1]]
jv_rasters <- lapply(jvlist, function(x) {
  #extract IDs for JV
  ids = jv_RAT %>%
    filter(str_detect(jv_state, x)) %>%
    pull(ID)
  # mask categorical raster to the JV
  jv_masked = mask(sdRiskCover_rcl, jvRast %in% ids, maskvalue = FALSE)
  # find cells that are not NA
  non_na_cells <- which(!is.na(values(jv_masked)))
  # Get coordinates (x/y) of those cells and extract min and max values
  coords = xyFromCell(jv_masked, non_na_cells)
  # crop to the non-NA extent
  jv_crop = crop(jv_masked, ext(min(coords[, "x"])-1000, max(coords[, "x"])+1000,
                                min(coords[, "y"])-1000, max(coords[, "y"])+1000))
  #remove spaces from JV name
  name = gsub(" ", "", x)
  writeRaster(jv_crop, filename = paste0("Output/Final/sdRiskCover_",name,".tif"))
  return(jv_crop)
})






