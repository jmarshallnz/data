library(tidyverse)
library(sf)
library(rmapshaper)

sa2 <- read_sf("~/data/data/nz_map/statsnz-statistical-area-2-higher-geographies-2025-SHP/statistical-area-2-higher-geographies-2025.shp")

ggplot(sa2) +
  geom_sf()

# OK, filter this out
sa2
map <- sa2 %>% filter(SA22025__1 != "Chatham Islands") |>
  filter(LAND_AREA_ > 0)

ggplot(map) +
  geom_sf()

map_simple <- ms_simplify(map, keep = 0.03,
                          keep_shapes = FALSE)

ggplot(map_simple) +
  geom_sf()

# tidy up the names
map_simple |>
  select(sa2 = SA22025_V1,
         sa2_name = SA22025__1, # with macron
         sa3 = SA32025_V1,
         sa3_name = SA32025__1, # with macron
         territorial_authority = TA2025_V_1,
         regional_council = REGC2025_1) |>
  st_write("~/data/data/nz_map/sa2_boundary.sqlite", delete_layer=TRUE)

# test (doesn't work with GDAL 403 error on linux)
read_sf("https://www.massey.ac.nz/~jcmarsha/data/nz_map/sa2_boundary.sqlite")
