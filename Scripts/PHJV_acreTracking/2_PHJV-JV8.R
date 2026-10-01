#This script manipulates the raw PHJV Acre tracking data to be uploaded to the JV8 Accomplishements tracking website
library(dplyr)
library(tidyr)
library(sf)
library(stringr)
library(stringdist)

#load and manipulate the data
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

#manipulate to JV8 format using the following definitions
#1. DirectAmount, Retention = Protection
#2. DirectAMount, Restoration = Restoration
#2. ExtensionAmount, Retention (<10 years) = persistence/retention
#3. ExtensionAmount, Restoration = restoration
#4. PolicyAmount, Retention = persistence/retention,
#5. PolicyAmount, Restoration = Restoration

data.jv8 <- data.grass |>
  pivot_longer(cols = c(DirectAmount, ExtensionAmount, PolicyAmount),
             names_to = "AcreType",
             values_to = "Acres") |>
  filter(!is.na(Acres)) |>
  mutate(Conservation_action = case_when(AcreType == "DirectAmount" & SubInitiativeName == "Retention" ~ "Protection",
                                         AcreType == "DirectAmount" & SubInitiativeName == "Restoration" ~ "Restoration",
                                         AcreType == "ExtensionAmount" & SubInitiativeName == "Retention" ~ "Persistence/Retention",
                                         AcreType == "ExtensionAmount" & SubInitiativeName == "Restoration" ~ "Restoration",
                                         AcreType == "PolicyAmount" & SubInitiativeName == "Retention" ~ "Persistence/Retention",
                                         AcreType == "PolicyAmount" & SubInitiativeName == "Restoration" ~ "Restoration"),
         FederalAmount = NA,
         NonFederalAmount = NA)

#update county names to match the CCS shapefile

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
  distinct(CCSNAME, Province) |>
  mutate(match_key = normalize_name(CCSNAME))

#uplands only
data_xwalk <- data.jv8 |>
  distinct(Municipality, Province) |>
  mutate(match_key = normalize_name(Municipality))

#join match_key with data and shapefile
counties.join <- counties.phjv |>
  left_join(county_xwalk)

#upland only data
data.join <- data.jv8 |>
  left_join(data_xwalk)


#simplify and export shapefiles and data
counties_export <- counties.join |>
  select(CCSNAME, Province, match_key)

data.export <- data.join |>
  select(FiscalYear, Province, Municipality, match_key, Acres, Conservation_action, FederalAmount, NonFederalAmount) |>
  filter(FiscalYear == 2025)

write.csv(data.export, "Output/PHJV_Acres_2025.csv", row.names = F)
st_write(counties_export, "Data/CanadianCounties/PHJV_counties_matchKey.shp")
