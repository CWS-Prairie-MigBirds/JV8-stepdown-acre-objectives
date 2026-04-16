#Stepdown acre objectives to JV x state/province
#Written by: Barry Robinson
#Date: April 15, 2026

#load libraries
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

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
                     "JV" = c("NGPJV", "OPJV", "PLJV", "PHJV", "PPJV", "RWBJV", "RGJV", "SJV"),
                     "Region" = c("Northern Great Plains", "Southern Great Plains", "Southern Great Plains", "Northern Great Plains", "Northern Great Plains", "Northern Great Plains", "Southern Great Plains", "Southern Great Plains"))

acresJVXstate <- left_join(acresJVXstate, lookup)

#join acres and actions tables together by JV and grass ID and change to long format
ActionToAcres <- left_join(acresJVXstate, jvActions, by = c("ID", "JV")) %>%
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

JVConAcres <- ActionToAcres %>%
  filter(!ConAct == "No Conservation Actions Recommended") %>%
  summarise(totalAcres = sum(ConActAcres, na.rm = TRUE),
            JV = first(JV),
            .by = c(jv_state, ConAct)) %>%
  mutate(propAcres = totalAcres / sum(totalAcres),
         .by = ConAct)

#make sure proportions sum to 1 for each conservation action
JVConAcres %>%
  summarize(check = sum(propAcres), .by = ConAct)


#Inspect groups to ensure this is doing what I want
groups <- ActionToAcres %>% 
  filter(!ConAct == "No Conservation Actions Recommended") %>%
  group_by(jv_state, ConAct) %>% group_keys()

#import JV8-wide acre objectives
jv8 <- read.csv("data/jv8AcreObj.csv")

#multiply JV8-wide acres objectives by proportions in each jv x state
stepdown_long <- left_join(JVConAcres, jv8) %>%
  mutate(acreObj = round(propAcres * Acres)) %>%
  left_join(lookup)

#summarize by region in wide format
acresXregion <- stepdown_long %>%
  summarize(acreObj = sum(acreObj), .by = c(Region, ConAct)) %>%
  pivot_wider(names_from = ConAct, values_from = acreObj)

#summarize by JV
acresXjv <- stepdown_long %>%
  summarize(acreObj = sum(acreObj), .by = c(JV, ConAct)) %>%
  pivot_wider(names_from = ConAct, values_from = acreObj) %>%
  select(JV, Protection, Restoration, Enhancement, "Persistence/Retention") %>%
  mutate(JV = factor(JV, levels = c("PHJV", "PPJV", "NGPJV", "RWBJV", "PLJV", "OPJV", "RGJV"))) %>%
  arrange(JV)


#summarize by JV x State/province in wide format
acresXjvState <- stepdown_long %>%
  select(jv_state, ConAct, acreObj) %>%
  pivot_wider(names_from = ConAct, values_from = acreObj) %>%
  mutate(JVname = str_extract(jv_state, "^[^_]+")) %>%
  left_join(lookup) %>%
  select(JV, jv_state, Protection, Restoration, Enhancement, "Persistence/Retention") %>%
  mutate(JV = factor(JV, levels = c("PHJV", "PPJV", "NGPJV", "RWBJV", "PLJV", "OPJV", "RGJV"))) %>%
  arrange(JV)

#make sure acres sum to JV8-wide acres objectives for each conservation action
stepdown_long %>%
  summarize(check = sum(acreObj), .by = ConAct)

#export results in wide format
write.csv(acresXregion, "Output/Final/StepdownObj_region.csv", row.names = F)
write.csv(acresXjv, "Output/Final/StepdownObj_jv.csv", row.names = F)
write.csv(acresXjvState, "Output/Final/StepdownObj_jvState.csv", row.names = F)


#Notes about decisions made
# 1. Only PPJV consistently provided percentages for tertiary conservation action
# 2. RWBJV has <1% for protection in tertiary category so ignoring as negligible for now.
# 3. RGJV has protection for tertiary in 3 grass categories, but no percentage, so ignoring for now.
