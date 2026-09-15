#This script is used for PHJV accomplishment tracking of grassland retention and restoration acres for the PHJV Implementation plan
#The objective is to spatially join acre tracking data to the Census Subdivision shapefile for the Prairie Provinces. 

library(dplyr)
library(tidyr)
library(sf)
library(stringr)
library(stringdist)

#Load PHJV acre tracking data and query out only grassland initiatives
data <- read.csv("Data/AcreTracking/PHJV_acres_2021_2025.csv") |>
  #remove rows where all three of these fields are blank
  filter(!(is.na(DirectAmount) & is.na(ExtensionAmount) & is.na(IndustryAmount))) 

#determine which Subprograms would qualify as grassland restoration or protection
unique(data$InitiativeSubProgramName)
grass <- c("Tame Pasture", "Native Pasture", "Hayland",
           "Winter Wheat", "Planted Cover", "Tame Grass-Idle",
           "Native Grass-Idle")

#query out grass categories
data.grass <- data |>
  filter(InitiativeSubProgramName %in% grass) |>
  select(InitiativeSubProgramName, DirectAmount, ExtensionAmount, PolicyAmount, ReportingType, SubInitiativeName, Province, FiscalYear, Agency, Landscape, Municipality)

#change to long format
#Uplands only
data.long <- data.grass |>
  pivot_longer(cols = c(DirectAmount, ExtensionAmount, PolicyAmount),
               names_to = "AcreType",
               values_to = "Acres") |>
  filter(!is.na(Acres)) |>
  mutate(Municipality = recode(Municipality, "Cornwallis" = "Elton")) #these were merged and renamed to Elton

#wetlands and uplands
dataFull.long <- data |>
  pivot_longer(cols = c(DirectAmount, ExtensionAmount, PolicyAmount),
               names_to = "AcreType",
               values_to = "Acres") |>
  filter(!is.na(Acres)) |>
  mutate(Municipality = recode(Municipality, "Cornwallis" = "Elton")) #these were merged and renamed to Elton

#|>
  #mutate(Municipality_std = str_replace(Municipality, "No\\.(\\d)", "No. \\1")) #modify municipality  names to match with county shapefile more easily

#1. create summary tables for total retention and restoration acres
acre.initiative.type <- data.long |>
  group_by(Province, SubInitiativeName, AcreType) |>
  summarize(sum(Acres)) |>
  arrange(Province, AcreType, SubInitiativeName)

acre.initiative <- data.long |>
  group_by(Province, SubInitiativeName) |>
  summarize(sum(Acres)) |>
  arrange(Province, SubInitiativeName)

write.csv(acre.initiative, "Output/PHJV_uplandAcresXprovince.csv", row.names = F)

acreFull.initiative <- dataFull.long |>
  group_by(Province, SubInitiativeName) |>
  summarize(sum(Acres)) |>
  arrange(Province, SubInitiativeName)

write.csv(acreFull.initiative, "Output/PHJV_AllAcresXprovince.csv", row.names = F)

acre.total <- data.long |>
  group_by(SubInitiativeName) |>
  summarize(sum(Acres))


#2. Summarize spatially (by county)

#create lookup table for provincial numeric codes
prov.lookup <- tribble(
  ~PRUID,   ~Province,
  "46",     "Manitoba",
  "47",     "Saskatchewan",
  "48",     "Alberta"
)

#load county shapefile
counties <- st_read("Data/CanadianCounties/lccs000b21a_e.shp") |>
  left_join(prov.lookup)
#load PHJV boundary and transcorm to CRS of counties
phjv <- st_read("Data/JVs/jv8.shp") |>
  filter(JV == "Prairie Habitat") |>
  select(JV) |>
  st_transform(st_crs(counties))

counties <- counties |> st_make_valid()
phjv <- phjv |> st_make_valid()

#query those counties that have at least 5% of their area within the PHJV
qualifying_ids <- counties |>
  mutate(total_area = st_area(geometry)) |>
  st_filter(phjv) |>
  st_intersection(phjv) |>
  mutate(overlap_pct = as.numeric(st_area(geometry) / total_area)) |>
  filter(overlap_pct >=0.05) |>
  pull(CCSUID)


counties.phjv <- counties |>
  filter(CCSUID %in% qualifying_ids, PRUID != "59") |>
  left_join(prov.lookup) 

plot(counties.phjv |> select(PRUID), reset = FALSE)
plot(st_geometry(phjv), add = TRUE, border = "red", lwd = 2)

#The county names don't match well between acre tracking data and shapefile, so modify names in data and shapefile so they match (this section written by claude.ai)
###NEW VERSION FOR CCS instead of CSD
# --- normalization function ---
normalize_name <- function(x) {
  x <- x |>
    str_replace_all(regex("fran.{1,4}ois", ignore_case = TRUE), "francois") |>
    str_replace_all("[\"'’‘]", "") |>
    str_replace_all(regex("^(municipal district of|county of|rural municipality of|town of|city of|village of)\\s+",
                          ignore_case = TRUE), "") |>
    str_replace_all(regex("special areas?", ignore_case = TRUE), "special area")
  
  is_special_area <- str_detect(x, regex("special area", ignore_case = TRUE))
  x[is_special_area]  <- str_replace_all(x[is_special_area], regex("no\\.?\\s*", ignore_case = TRUE), "")
  x[!is_special_area] <- str_replace_all(x[!is_special_area], regex("\\s*no\\.?\\s*\\d+\\b", ignore_case = TRUE), "")
  
  x |>
    str_replace_all(regex("\\s+county$", ignore_case = TRUE), "") |>
    str_replace_all("[-–—]", " ") |>
    str_replace_all("[.,]", "") |>
    str_squish() |>
    str_to_lower()
}

county_xwalk <- counties.phjv |>
  st_drop_geometry() |>
  distinct(CCSNAME, Province) |>
  mutate(match_key = normalize_name(CCSNAME))

#uplands only
data_xwalk <- data.long |>
  distinct(Municipality, Province) |>
  mutate(match_key = normalize_name(Municipality))

#uplands and wetlands
dataFull_xwalk <- dataFull.long |>
  distinct(Municipality, Province) |>
  mutate(match_key = normalize_name(Municipality))

# --- diagnostics ---
# see what's already matching
matched <- inner_join(data_xwalk, county_xwalk, by = c("match_key", "Province"))
matchedFull <- inner_join(dataFull_xwalk, county_xwalk, by = c("match_key", "Province"))
# what's left unmatched on each side
unmatched_data <- anti_join(data_xwalk, county_xwalk, by = c("match_key", "Province"))
unmatched_shp <- anti_join(county_xwalk, data_xwalk, by = c("match_key", "Province"))

#join match_key with data and shapefile
counties.join <- counties.phjv |>
  left_join(county_xwalk)

#upland only
data.join <- data.long |>
  left_join(data_xwalk)

#upland and wetland
dataFull.join <- dataFull.long |>
  left_join(dataFull_xwalk)

#Summarize acre data by Province, Municipality, and subinitiative
#Upland only
acre.summary <- data.join |>
  group_by(Province, SubInitiativeName, match_key) |>
  summarize(total.acres = sum(Acres)) |>
  filter(!match_key %in% c("alberta (provincial level)", "saskatchewan (provincial level)", "manitoba (provincial level)"))

#upland and wetland
acreFull.summary <- dataFull.join |>
  group_by(Province, SubInitiativeName, match_key) |>
  summarize(total.acres = sum(Acres)) |>
  filter(!match_key %in% c("alberta (provincial level)", "saskatchewan (provincial level)", "manitoba (provincial level)"))


#join acre data with county shapefile and switch from long to wide format so there are separate columns for restoration and retention acres by county
#start with restoration, then add retention
#Upland only
county.acres <- counties.join |>
  left_join(acre.summary |> filter(SubInitiativeName == "Restoration"), 
            by = c("Province", "match_key")) |>
  select(match_key, Province, total.acres) |>
  mutate(total.acres = coalesce(total.acres, 0)) |>
  rename(Restoration = total.acres) |>
  left_join(acre.summary |> filter(SubInitiativeName == "Retention"), 
            by = c("Province", "match_key")) |>
  select(-SubInitiativeName) |>
  mutate(total.acres = coalesce(total.acres, 0)) |>
  rename(Retention = total.acres)

#upland and wetland
countyFull.acres <- counties.join |>
  left_join(acreFull.summary |> filter(SubInitiativeName == "Restoration"), 
            by = c("Province", "match_key")) |>
  select(match_key, Province, total.acres) |>
  mutate(total.acres = coalesce(total.acres, 0)) |>
  rename(Restoration = total.acres) |>
  left_join(acreFull.summary |> filter(SubInitiativeName == "Retention"), 
            by = c("Province", "match_key")) |>
  select(-SubInitiativeName) |>
  mutate(total.acres = coalesce(total.acres, 0)) |>
  rename(Retention = total.acres)

#Double check that all restoration and retention acres were linked to a county in the shapefile
retention.ccs <- county.acres |>
  filter(Retention > 0) |>
  distinct(match_key, Province) #332 CSDs

retention.acres.ccs <- acre.summary |>
  filter(SubInitiativeName == "Retention") |>
  distinct(match_key, Province) #335 CSDs

#repeat for Restoration
restoration.ccs <- county.acres |>
  filter(Restoration > 0) |>
  distinct(match_key, Province) #324 matchkeyXProvince

restoration.acres.ccs <- acre.summary |>
  filter(SubInitiativeName == "Restoration") |>
  distinct(match_key, Province) #325 matchkeyXProvince

anti_join(restoration.acres.ccs, restoration.ccs, by = c("match_key", "Province"))

#West Interlake restoration is missing from the shapefile data because this was excluded when querying counties within PHJV, so no issue



#Inspect shapefile and export
plot(county.acres |> select(Restoration))
plot(countyFull.acres |> select(Retention), reset = FALSE)

#export as separate shapefiles for ease of mapping in ArcPro
#upland only
st_write(county.acres |> select(match_key, Province, Restoration), "Output/PHJV_AcreTracking/PHJV_Grass_RestXcounty.shp")
st_write(county.acres |> select(match_key, Province, Retention), "Output/PHJV_AcreTracking/PHJV_Grass_ReteXcounty.shp")

#upland and wetland
st_write(countyFull.acres |> select(match_key, Province, Restoration), "Output/PHJV_AcreTracking/PHJV_All_RestXcounty.shp")
st_write(countyFull.acres |> select(match_key, Province, Retention), "Output/PHJV_AcreTracking/PHJV_All_ReteXcounty.shp")














####Old, not needed

# ---------------------------------------------------------------
# Shared name-cleaning function (handles issues common to all provinces)
# ---------------------------------------------------------------
norm_name <- function(x) {
  x |>
    str_replace_all('"S\\b', "'s") |>              # curly-quote corruption: X"S -> X's
    str_replace_all('"', "'") |>                     # remaining "-quotes -> apostrophe
    str_replace_all("Fran\\+.ois", "Francois") |>     # MB mojibake fix
    gsub("(Mc)([a-z])", "\\1\\U\\2", x = _, perl = TRUE) |>  # Mccraney -> McCraney
    str_replace_all("-", " ") |>                      # hyphens -> space (before punct strip)
    str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>      # strip accents
    str_remove_all("[[:punct:]]") |>
    str_squish()
}

# =================================================================
# ALBERTA — small, irregular naming; hardcoded lookup
# =================================================================
ab_lookup <- tribble(
  ~Municipality,                                     ~CSDNAME,
  "Cypress County",                                  "Cypress County",
  "County of Newell",                                "Newell County",
  "Kneehill County",                                 "Kneehill County",
  "Lacombe County",                                   "Lacombe County",
  "Municipal District of Provost No. 52",            "Provost No. 52",
  "Municipal District of Spirit River No. 133",      "Spirit River No. 133",
  "Special Areas No. 2",                             "Special Area No. 2",
  "Special Areas 2",                                 "Special Area No. 2",
  "Sturgeon County",                                 "Sturgeon County",
  "Westlock County",                                 "Westlock County",
  "Vulcan County",                                   "Vulcan County",
  "Beaver County",                                   "Beaver County",
  "Lamont County",                                   "Lamont County",
  "Flagstaff County",                                "Flagstaff County",
  "Camrose County",                                  "Camrose County",
  "County of Stettler No. 6",                        "Stettler County No. 6",
  "Wheatland County",                                "Wheatland County",
  "Municipal District of Foothills No. 31",          "Foothills County",
  "Rocky View County",                               "Rocky View County",
  "Municipal District of Willow Creek No. 26",       "Willow Creek No. 26",
  "County of Paintearth No. 18",                     "Paintearth County No. 18",
  "Special Areas No. 3",                             "Special Area No. 3",
  "Special Areas No. 4",                             "Special Area No. 4",
  "County of Vermilion River",                       "Vermilion River County",
  "County of Warner No. 5",                          "Warner County No. 5",
  "County of Forty Mile No. 8",                      "Forty Mile County No. 8",
  "Red Deer County",                                 "Red Deer County",
  "Starland County",                                 "Starland County",
  "County of Minburn No. 27",                        "Minburn County No. 27",
  "Municipal District of Wainwright No. 61",         "Wainwright No. 61",
  "County of Wetaskiwin No. 10",                     "Wetaskiwin County No. 10",
  "Clear Hills County",                              "Clear Hills",
  "Clearwater County",                               "Clearwater County",
  "Mountain View County",                            "Mountain View County",
  "Smoky Lake County",                               "Smoky Lake County",
  "County of Two Hills No. 21",                      "Two Hills County No. 21",
  "Strathcona County",                               "Strathcona County",
  "Athabasca County",                                "Athabasca County",
  "Cardston County",                                 "Cardston County",
  "Municipal District of Bonnyville No. 87",         "Bonnyville No. 87",
  "Parkland County",                                 "Parkland County",
  "Thorhild County",                                 "Thorhild County",
  "Leduc County",                                    "Leduc County",
  "Municipal District of Pincher Creek No. 9",       "Pincher Creek No. 9",
  "County of St. Paul No. 19",                       "St. Paul County No. 19",
  "Municipal District of Taber",                     "Taber",
  "Ponoka County",                                   "Ponoka County"
) |>
  mutate(name_clean = norm_name(CSDNAME))  # not used for matching, just kept for consistency

data.long_ab <- data.long |>
  filter(Province == "Alberta", Municipality != "Alberta (provincial level)") |>
  mutate(Municipality_std = str_replace(Municipality, "No\\.(\\d)", "No. \\1")) |>
  left_join(ab_lookup |> select(Municipality_std = Municipality, CSDNAME), by = "Municipality_std") |>
  select(-Municipality_std)

# =================================================================
# SASKATCHEWAN — every municipality is an RM with "No. N" suffix
# =================================================================
sk_shp <- counties.phjv |>
  filter(PRUID == "47", str_detect(CSDNAME, "No\\.\\s*\\d+$")) |>
  mutate(
    base_name  = str_remove(CSDNAME, "\\s*No\\.\\s*\\d+$"),
    name_clean = norm_name(base_name)
  ) |>
  st_drop_geometry()

data.long_sk <- data.long |>
  filter(Province == "Saskatchewan", Municipality != "Saskatchewan (provincial level)") |>
  mutate(name_clean = norm_name(Municipality)) |>
  left_join(sk_shp |> select(CSDNAME, name_clean), by = "name_clean") |>
  select(-c(name_clean, Municipality_std))

# =================================================================
# MANITOBA — mostly direct match; disambiguate RM vs city/town duplicates
# =================================================================
mb_shp <- counties.phjv |>
  filter(PRUID == "46") |>
  mutate(name_clean = norm_name(CSDNAME)) |>
  group_by(name_clean) |>
  filter(n() == 1 | CSDTYPE == "RM") |>
  ungroup() |>
  st_drop_geometry()

data.long_mb <- data.long |>
  filter(Province == "Manitoba", Municipality != "Manitoba (provincial level)") |>
  mutate(name_clean = norm_name(Municipality)) |>
  left_join(mb_shp |> select(CSDNAME, name_clean), by = "name_clean") |>
  select(-c(name_clean, Municipality_std))

# =================================================================
# COMBINE
# =================================================================
data.long_matched <- bind_rows(data.long_ab, data.long_sk, data.long_mb)

# sanity checks across all three provinces
data.long_matched |> filter(is.na(CSDNAME)) |> distinct(Province, Municipality)
data.long_matched |> distinct(Province, Municipality, CSDNAME) |> count(Province, Municipality) |> filter(n > 1)



#create lookup table for provincial numeric codes
prov.lookup <- tribble(
  ~PRUID,   ~Province,
  "46",     "Manitoba",
  "47",     "Saskatchewan",
  "48",     "Alberta"
)










