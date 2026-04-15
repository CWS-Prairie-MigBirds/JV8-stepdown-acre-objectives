#Stepdown acre objectives to JV x state/province
#Written by: Barry Robinson
#Date: April 15, 2026

#load libraries
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

#load excel file that assigns conservation actions to each category of grass for each JV (downloaded from JV8 Google Drive)
file <- "data/JV_GrassToConAction.xlsx"

# Get all sheet names and remove those not needed
sheets <- excel_sheets(file) |>
  setdiff(c("Orig", "SJV_Arizona"))

# Read all sheets into a named list and combine into a single table
data_list <- lapply(sheets, function(s) {
  read_excel(file, sheet = s) %>%
    rename("ConRisk" = "Agriculture Conversion Risk (Olimb and Robinson)",
           "EncRisk" = "Woody Encroachment Risk (RAP: tree and shrub)",
           "Cover" = "Landcover (PUDL)",
           "ConAct1" = "Primary Conservation Action",
           "Perc1" = "Percent of Acres with Primary Conservation Action",
           "ConAct2" = "Secondary Conservation Action",
           "Perc2" = "Percent of Acres with Secondary Conservation Action",
           "ConAct3" = "Tertiary Conservation Action",
           "Perc3" = "Comments") %>%
    select("ID", "ConAct1", "ConAct2", "ConAct3", "Perc1", "Perc2","Perc3") %>%
    mutate(Perc3 = as.numeric(Perc3))
})
names(data_list) <- sheets

jvActions <- bind_rows(data_list, .id = "JV")

#load table with acres of each grass category in each JV x state/province
acresJVXstate <- read.csv("Output/Final/RiskCropAcres_jvXstate.csv") %>%
  mutate(JVname = str_extract(jv_state, "^[^_]+"))

#parse out JV names and create lookup table for JV acronyms
lookup <- data.frame("JVname" = unique(acresJVXstate$JVname),
                     "JV" = c("NGPJV", "OPJV", "PLJV", "PHJV", "PPJV", "RWBJV", "RGJV", "SJV"))

acresJVXstate <- left_join(acresJVXstate, lookup)

#join acres and actions tables together by JV and grass ID and change to long format
stepdown <- left_join(acresJVXstate, jvActions, by = c("ID", "JV")) %>%
  pivot_longer(
    cols = c(ConAct1, ConAct2, ConAct3,
             Perc1,   Perc2,   Perc3),
    names_to = c(".value", "ConNum"),
    names_pattern = "(ConAct|Perc)(\\d)"
  ) %>%
  mutate(ConActAcres = acreMil * (Perc/100)) %>% #calculate acres for each conservation action in each jurasdiction (acreMil x Perc)
  filter(!is.na(ConAct) & !is.na(Perc)) #will be NAs if certain categories don't have conservation actions associated with them

#sum acres for each conservation action and jurasdiction
# JVConAcres <- stepdown %>%
#   group_by(jv_state, ConAct) %>%
#   summarize(totalAcres = sum(ConActAcres, na.rm = T)) %>%
#   group_by(ConAct) %>%
#   mutate(propAcres = totalAcres / sum(totalAcres, na.rm = TRUE)) %>%
#   ungroup()

JVConAcres <- stepdown %>%
  summarise(totalAcres = sum(ConActAcres, na.rm = TRUE),
            JV = first(JV),
            .by = c(jv_state, ConAct)) %>%
  mutate(propAcres = totalAcres / sum(totalAcres),
         .by = ConAct)

#make sure proportions sum to 1 for each conservation action
JVConAcres %>%
  summarize(check = sum(propAcres), .by = ConAct)


#Inspect groups to ensure this is doing what I want
groups <- stepdown %>% group_by(jv_state, ConAct) %>% group_keys()

#import JV8-wide acre objectives
jv8 <- read.csv("data/jv8AcreObj.csv")

stepdownFinal <- left_join(JVConAcres, jv8) %>%
  filter(!ConAct == "No Conservation Actions Recommended") %>%
  mutate(acreObj = round(propAcres * Acres)) %>%
  select(JV, jv_state, ConAct, totalAcres, propAcres, acreObj) %>%
  pivot_wider(names_from = ConAct, values_from = acreObj)

#summarize by JV
stepdownXjv <- stepdownFinal %>%
  summarize(acreObj = sum(acreObj), .by = c(JV, ConAct)) %>%
  pivot_wider(names_from = ConAct, values_from = acreObj) %>%
  select(JV, Protection, Restoration, Enhancement, "Persistence/Retention") %>%
  mutate(JV = factor(JV, levels = c("PHJV", "PPJV", "NGPJV", "RWBJV", "PLJV", "OPJV", "RGJV"))) %>%
  arrange(JV)

#make sure acres sum to JV8-wide acres objectives for each conservation action
stedownFinal %>%
  summarize(check = sum(acreObj), .by = ConAct)



#Notes about decisions made
# 1. Only PPJV consistently provided percentages for tertiary conservation action
# 2. RWBJV has <1% for protection in tertiary category so ignoring as negligable for now.
# 3. RGJV has protection for tertiary in 3 grass categories, but no percentage, so ignoring for now.
