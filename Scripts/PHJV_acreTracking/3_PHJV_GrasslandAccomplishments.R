#This script is used for PHJV accomplishment tracking of grassland retention and restoration acres for the PHJV Implementation plan
#The objective is to (1) determine how many acres were retained or restored and (2) how many of those acres were in close proximity to
#Landbird priority areas.

library(dplyr)
library(tidyr)
library(sf)
library(stringr)

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
data.long <- data.grass |>
  pivot_longer(cols = c(DirectAmount, ExtensionAmount, PolicyAmount),
               names_to = "AcreType",
               values_to = "Acres") |>
  filter(!is.na(Acres)) |>
  mutate(Municipality_std = str_replace(Municipality, "No\\.(\\d)", "No. \\1")) #modify municipality  names to match with county shapefile more easily

#1. create summary tables for total retention and restoration acres
acre.initiative.type <- data.long |>
  group_by(Province, SubInitiativeName, AcreType) |>
  summarize(sum(Acres)) |>
  arrange(Province, AcreType, SubInitiativeName)

acre.initiative <- data.long |>
  group_by(Province, SubInitiativeName) |>
  summarize(sum(Acres)) |>
  arrange(Province, SubInitiativeName)

acre.total <- data.long |>
  group_by(SubInitiativeName) |>
  summarize(sum(Acres))


#2. Summarize spatially (by county) relative to Landbird priority areas
#load county shapefile
counties <- st_read("Data/CanadianCounties/lcsd000b21a_e.shp")
#load PHJV boundary and transcorm to CRS of counties
phjv <- st_read("Data/JVs/jv8.shp") |>
  filter(JV == "Prairie Habitat") |>
  select(JV) |>
  st_transform(st_crs(counties))

#query those counties that intersect with PHJV
counties.phjv <- st_filter(counties, phjv) |>
  filter(PRUID != "59")

# data.long |>
#   filter(Province == "Alberta") |>
#   pull(Municipality) |>
#   unique()
# 
# counties.phjv |>
#   filter(PRUID == "48") |>
#   pull(CSDNAME) |>
#   unique()

#The names don't match well, so modify names in data and shapefile so they match (this section written by claude.ai)

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
  )

data.long_sk <- data.long |>
  filter(Province == "Saskatchewan", Municipality != "Saskatchewan (provincial level)") |>
  mutate(name_clean = norm_name(Municipality)) |>
  left_join(sk_shp |> select(CSDNAME, name_clean), by = "name_clean") |>
  select(-name_clean)

# =================================================================
# MANITOBA — mostly direct match; disambiguate RM vs city/town duplicates
# =================================================================
mb_shp <- counties.phjv |>
  filter(PRUID == "46") |>
  mutate(name_clean = norm_name(CSDNAME)) |>
  group_by(name_clean) |>
  filter(n() == 1 | CSDTYPE == "RM") |>
  ungroup()

data.long_mb <- data.long |>
  filter(Province == "Manitoba", Municipality != "Manitoba (provincial level)") |>
  mutate(name_clean = norm_name(Municipality)) |>
  left_join(mb_shp |> select(CSDNAME, name_clean), by = "name_clean") |>
  select(-name_clean)

# =================================================================
# COMBINE
# =================================================================
data.long_matched <- bind_rows(data.long_ab, data.long_sk, data.long_mb)

# sanity checks across all three provinces
data.long_matched |> filter(is.na(CSDNAME)) |> distinct(Province, Municipality)
data.long_matched |> distinct(Province, Municipality, CSDNAME) |> count(Province, Municipality) |> filter(n > 1)

# =================================================================
# BUILD COMBINED SHAPEFILE AND JOIN
# =================================================================
counties_matched <- counties.phjv |>
  filter(PRUID %in% c("46", "47", "48"), CSDNAME %in% unique(data.long_matched$CSDNAME))

# confirm CSDNAME is unique within each province's matched subset before joining
counties_matched |> st_drop_geometry() |> count(PRUID, CSDNAME) |> filter(n > 1)

counties_joined <- counties_matched |>
  left_join(data.long_matched, by = "CSDNAME")









#ALBERTA
ab_lookup <- tribble(
  ~Municipality_std,                                ~CSDNAME,
  "Cypress County",                                  "Cypress County",
  "County of Newell",                                "Newell County",
  "Kneehill County",                                 "Kneehill County",
  "Lacombe County",                                  "Lacombe County",
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
)

data.long_ab <- data.long |>
  filter(Province == "Alberta", Municipality != "Alberta (provincial level)") |>
  left_join(ab_lookup, by = "Municipality_std")

data.long_ab |> filter(is.na(CSDNAME))  # should be empty

#SASKATCHEWAN
data.long |>
  filter(Province == "Saskatchewan") |>
  pull(Municipality) |>
  unique()

counties.phjv |>
  filter(PRUID == "47") |>
  pull(CSDNAME) |>
  unique()

fix_quotes <- function(x) {
  x <- str_replace_all(x, '"S\\b', "'s")
  x <- str_replace_all(x, '"', "'")
  x
}
fix_mc <- function(x) gsub("(Mc)([a-z])", "\\1\\U\\2", x, perl = TRUE)

norm_name <- function(x) {
  x |>
    fix_quotes() |>
    fix_mc() |>
    str_to_lower() |>
    str_remove_all("[[:punct:]]") |>
    str_squish()
}

# Candidate RM rows only — must have "No. <digits>" suffix
sk_rms <- counties.phjv |>
  filter(PRUID == "47", str_detect(CSDNAME, "No\\.\\s*\\d+$")) |>
  mutate(
    base_name  = str_remove(CSDNAME, "\\s*No\\.\\s*\\d+$"),
    name_clean = norm_name(base_name)
  )

data.long_sk <- data.long |>
  filter(Province == "Saskatchewan", Municipality != "Saskatchewan (provincial level)") |>
  mutate(name_clean = norm_name(Municipality)) |>
  left_join(
    sk_rms |> select(CSDNAME, name_clean),
    by = "name_clean"
  )

# sanity check
data.long_sk |> filter(is.na(CSDNAME))          # should be empty
data.long_sk |>
  distinct(Municipality, CSDNAME) |>
  count(Municipality) |>
  filter(n > 1)  # should be empty — check for dupes

#MANITOBA
data.long |>
  filter(Province == "Manitoba") |>
  pull(Municipality) |>
  unique()

counties.phjv |>
  filter(PRUID == "46") |>
  pull(CSDNAME) |>
  unique()

norm_name <- function(x) {
  x |>
    str_replace_all("Fran\\+.ois", "François") |>   # fix the mojibake before anything else
    str_replace_all("-", " ") |>                      # hyphen -> space (do this before punct strip)
    str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>      # é/ç -> e/c etc.
    str_remove_all("[[:punct:]]") |>
    str_squish()
}

mb_data <- data.long |>
  filter(Province == "Manitoba", Municipality != "Manitoba (provincial level)") |>
  mutate(name_clean = norm_name(Municipality))

mb_shp <- counties.phjv |>
  filter(PRUID == "46") |>
  mutate(name_clean = norm_name(CSDNAME)) |>
  # prefer RM over CY/T/IRI when a name is duplicated
  group_by(name_clean) |>
  filter(n() == 1 | CSDTYPE == "RM") |>
  ungroup()

# confirm the join is now 1:1
mb_shp |> count(name_clean) |> filter(n > 1)

mb_matched <- mb_data |>
  left_join(mb_shp |> select(CSDNAME, name_clean), by = "name_clean")

mb_matched |> filter(is.na(CSDNAME)) |> distinct(Municipality)
mb_matched |> distinct(Municipality, CSDNAME) |> count(Municipality) |> filter(n > 1)








mb_shp <- counties.phjv |>
  filter(PRUID == "46") |>
  mutate(name_clean = norm_name(CSDNAME))

mb_matched <- mb_data |>
  left_join(mb_shp |> select(CSDNAME, name_clean), by = "name_clean")

# check
mb_matched |> filter(is.na(CSDNAME)) |> distinct(Municipality)
mb_matched |> distinct(Municipality, CSDNAME) |> count(Municipality) |> filter(n > 1)

dupe_names <- mb_shp |>
  count(name_clean) |>
  filter(n > 1) |>
  pull(name_clean)

mb_shp |> filter(name_clean %in% dupe_names) |> arrange(name_clean)
