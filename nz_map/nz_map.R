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


# Stats NZ stuff
stat1 <- read_sf("nz_map/statsnz-2023-census-totals-by-topic-for-individuals-by-statistical-a-SHP/2023-census-totals-by-topic-for-individuals-by-statistical-a.shp")

count_names <- read_csv("nz_map/statsnz-2023-census-totals-by-topic-for-individuals-by-statistical-a-SHP/2023_census_totals_by_topic_for_individuals_by_sa2_part_2_lookup_table.csv")

# STuff that might be interesting
cols <- count_names |> filter(Year == 2023) |> filter(Variable1 %in% c("Total personal income",
                                                                       "Status in employment",
                                                                       "Years at usual residence",
                                                                       "Individual home ownership",
                                                                       "Highest qualification",
                                                                       "Hours worked in employment per week",
                                                                       "Work and labour force status")) |>
  select(Column_name, Variable1, Variable1_category)

test <- stat1 |> select(SA22023_V1, all_of(cols$Column_name)) |> st_set_geometry(NULL) |>
  pivot_longer(-SA22023_V1, names_to='Column_name', values_to='count')

sa2_dat <- test |> left_join(cols)

# home ownership rates: (VAR_2_13 + VAR_2_14)/VAR_2_18
# mean years at residence: VAR_2_101 weighted by VAR_2_100
# proportion moved in last year: VAR2_92/VAR_2_100
# tertiary qualification: sum(VAR_2_245:VAR_2_248)/VAR_2_252
# median income: VAR_2_404 weighted by VAR_2_414 I guess?!? (weighted median doesn't make much sense, but OK for now)
# hours worked average: VAR_2_536 weighted by VAR_2_546
# labour force status: (VAR_2_429 + VAR_2_430)/VAR_2_435

map_data <- stat1 |>
  filter(SA22023__1 != "Chatham Islands") |>
  filter(LAND_AREA_ > 0) |>
  st_set_geometry(NULL)

  mutate(home_ownership = (VAR_2_13 + VAR_2_14)/VAR_2_18,
         home_for_last = VAR_2_101,
         moved_in_last_year = VAR_2_92/VAR_2_100,
         tertiary_qual = (VAR_2_245+VAR_2_246+VAR_2_247+VAR_2_248)/VAR_2_252,
         median_income = VAR_2_404,
         mean_hours_worked = VAR_2_536,
         in_employment = (VAR_2_429 + VAR_2_430)/VAR_2_435) |>
  st_set_geometry(NULL) |>
  select(-starts_with("VAR"))

# read in our simplified version with SA3 level?

concordance <- read_sf("nz_map/sa2_boundary.sqlite")
concordance |> anti_join(map_data, join_by(sa2 == SA22023_V1))
map_data |> anti_join(concordance, join_by(SA22023_V1 == sa2)) # nice

library(naniar)
# let's go down to SA3 level
testing <- concordance |> left_join(map_data, join_by(sa2 == SA22023_V1)) |>
  st_set_geometry(NULL) |>
  replace_with_na_all(condition = ~.x == -999) |>
  group_by(sa3, sa3_name, territorial_authority, regional_council) |>
  summarise(home_ownership = sum(VAR_2_13 + VAR_2_14, na.rm=TRUE)/sum(VAR_2_18, na.rm=TRUE),
            home_for_last = sum(VAR_2_101*VAR_2_100, na.rm=TRUE)/sum(VAR_2_100, na.rm=TRUE), # weighted avg
            moved_in_last_year = sum(VAR_2_92, na.rm=TRUE)/sum(VAR_2_100, na.rm=TRUE),
            tertiary_qual = sum(VAR_2_245+VAR_2_246+VAR_2_247+VAR_2_248, na.rm=TRUE)/sum(VAR_2_252, na.rm=TRUE),
            median_income = sum(VAR_2_404*VAR_2_414, na.rm=TRUE)/sum(VAR_2_414, na.rm=TRUE), # weighted avg of median :|
            mean_hours_worked = sum(VAR_2_536*VAR_2_546, na.rm=TRUE)/sum(VAR_2_546, na.rm=TRUE), # weighted avg
            in_employment = sum(VAR_2_429 + VAR_2_430, na.rm=TRUE)/sum(VAR_2_435, na.rm=TRUE))

finalish <- concordance |> group_by(sa3, sa3_name, territorial_authority, regional_council) |>
  summarise() |>
  left_join(testing) |>
  relocate(GEOMETRY, .after=last_col()) |>
  st_as_sf()

finalish |>
  filter(!is.na(home_wondership) |>
  st_write("~/data/data/nz_map/sa3_data.sqlite", delete_layer=TRUE)

ggplot(finalish) +
  aes(fill=home_ownership) +
  geom_sf()
  
ggplot(testing) +
  aes(x=home_for_last) +
  geom_histogram()
ggplot(testing) +
  aes(x=in_employment) +
  geom_histogram()

