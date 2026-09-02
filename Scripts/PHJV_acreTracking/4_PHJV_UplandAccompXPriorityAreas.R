#This script using the shapefile from Script 3 with retention and restoration acres by county to estimate how many of those acres were in close proximity to
#Landbird priority areas.

library(dplyr)
library(sf)

#1. Load spatial data
#Import accomplishment acres X county
county.acres <- st_read("Output/PHJV_AcreTracking/PHJV_Grass_acresXcounty.shp")

#Import upland bird priority areas
pa <- st_read("Data/AcreTracking/Upland_smooth.shp") |>
  st_transform(st_crs(county.acres))

#query counties that overlap at least 50% of their area with Upland bird priority areas
overlapingTest <- county.acres |>
  mutate(total_area = st_area(geometry)) |>
  st_filter(pa) |>
  st_intersection(pa) |>
  mutate(overlap_pct = as.numeric(st_area(geometry) / total_area)) |>
  filter(overlap_pct >=0.30) |>
  mutate(CSDUnique = paste(CSDNAME, CSDTYPE, Provinc, sep = "_")) |>
  pull(CSDUnique)

pa.counties <- county.acres |>
  mutate(CSDUnique = paste(CSDNAME, CSDTYPE, Provinc, sep = "_")) |>
  filter(CSDUnique %in% overlapingTest)

#plot and inspect to fine-tune overlap threshold
plot(pa.counties |> select(Restrtn), reset = FALSE)
plot(st_geometry(pa), add = TRUE, border = "red", lwd = 2)

#export shapefile
st_write(pa.counties, "Output/PHJV_AcreTracking/PHJV_acresXpriorityArea.shp")

#calculate acres by activity and province
pa.acres.summary <- pa.counties |>
  group_by(Provinc) |>
  summarize(Restoration = sum(Restrtn),
            Retention = sum(Retentn)) |>
  st_drop_geometry()
