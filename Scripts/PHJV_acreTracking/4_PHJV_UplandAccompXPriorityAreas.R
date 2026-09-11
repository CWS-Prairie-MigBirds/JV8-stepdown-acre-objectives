#This script using the shapefile from Script 3 with retention and restoration acres by county to estimate how many of those acres were in close proximity to
#Landbird priority areas.

library(dplyr)
library(sf)
library(terra)

#1. Load spatial data
#Import accomplishment acres X county and merge
county.rest <- st_read("Output/PHJV_AcreTracking/PHJV_Grass_RestXcounty.shp") |>
  rename("match_key" = "mtch_ky",
         "Province" = "Provinc",
         "Restoration" = "Restrtn")
county.rete <- st_read("Output/PHJV_AcreTracking/PHJV_Grass_ReteXcounty.shp")

county.acres <- left_join(county.rest, st_drop_geometry(county.rete))

#Import upland bird priority areas
pa <- st_read("Data/AcreTracking/PriorityAreas/Upland_smooth.shp") |>
  st_transform(st_crs(county.acres))

#2.UPLAND BIRD PRIORITY AREAS 
#Query counties that have at least 30% of their area overlapping with Upland bird priority areas
overlapingTest <- county.acres |>
  mutate(total_area = st_area(geometry)) |>
  st_filter(pa) |>
  st_intersection(pa) |>
  mutate(overlap_pct = as.numeric(st_area(geometry) / total_area)) |>
  filter(overlap_pct >=0.30) |>
  mutate(CSDUnique = paste(match_key, Province, sep = "_")) |>
  pull(CSDUnique)

pa.counties <- county.acres |>
  mutate(CSDUnique = paste(match_key, Province, sep = "_")) |>
  filter(CSDUnique %in% overlapingTest)

#plot and inspect to fine-tune overlap threshold
plot(pa.counties |> select(Restoration), reset = FALSE)
plot(st_geometry(pa), add = TRUE, border = "red", lwd = 2)

#export shapefile
st_write(pa.counties |> select(match_key, Province, Restoration), "Output/PHJV_AcreTracking/PHJV_RestXpriorityArea.shp")
st_write(pa.counties |> select(match_key, Province, Retention), "Output/PHJV_AcreTracking/PHJV_ReteXpriorityArea.shp")

#calculate acres by activity and province
pa.acres.summary <- pa.counties |>
  group_by(Province) |>
  summarize(Restoration = sum(Restoration),
            Retention = sum(Retention)) |>
  st_drop_geometry()

#3. GRASSLAND SAR HIGH PRIORITY GRASSLANDS
#Import high priority grasslands raster and reporject to match county.areas
t.grass <- rast("Data/AcreTracking/PriorityAreas/HabitatObj_Final_wBAIS.tif")
names(t.grass) <- "count"

#Query out counties where reproject county.acres to match t.grass
county.grass <- st_transform(county.acres, crs(t.grass))

# Get raster cells intersecting each county, including the fraction of each cell covered by each county
grass_cells <- extract(t.grass, county.grass, exact = TRUE, cells = TRUE)

# Calculate grass area within each county
grass_area <- grass_cells |>
  filter(!is.na(count)) |>
  group_by(ID) |>
  summarise(
    grass_area = sum(fraction * prod(res(t.grass)))
  )

#add grass area and country area to county.acres and calculate % overlap
county.grass <- county.grass |>
  mutate(ID = row_number()) |>
  left_join(grass_area, by = "ID") |>
  mutate(
    grass_area = coalesce(grass_area, 0),
    total_area = as.numeric(st_area(geometry)),
    overlap_pct = grass_area / total_area
  ) |>
  select(-ID)

#ensure max percent cover is 1
max(county.grass$overlap_pct)

#plot counties with >10% cover of target grass
plot(county.grass |>  filter(overlap_pct >=0.1) |> select(overlap_pct))

#calculate acres of restoration and retention within counties that have >=10% target grass cover
plot(county.grass |>  filter(overlap_pct >=0.1) |> select(Restrtn))
sum(county.grass |> 
      filter(overlap_pct >=0.1) |>
      pull(Restrtn)
    )

plot(county.grass |>  filter(overlap_pct >=0.1) |> select(Retentn))
sum(county.grass |> 
      filter(overlap_pct >=0.1) |>
      pull(Retentn)
)

#query out counties with > X % of their area overlapping with target grasslands
county.grass <- county.acres |>
  
plot(select(county.grass, overlap_pct))
     