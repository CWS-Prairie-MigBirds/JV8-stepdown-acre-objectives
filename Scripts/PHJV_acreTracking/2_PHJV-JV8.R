#This script manipulates the raw PHJV Acre tracking data to be uploaded to the JV8 Accomplishements tracking website
library(dplyr)
library(tidyr)

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

write.csv(data.jv8, "Output/PHJV_Acres_2021-2025.csv", row.names = F)
