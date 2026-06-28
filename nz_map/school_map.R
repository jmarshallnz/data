# Long/lat of NZ schools

# https://www.educationcounts.govt.nz/directories/list-of-nz-schools

read_csv("~/Downloads/directory (1).csv", skip=15) |>
  select(school = `School Name`, long=Longitude, lat=Latitude) |> filter(!is.na(school), !is.na(long)) |>
  write_csv("~/data/data/nz_map/schools.csv")

schools_sf <- st_as_sf(schools, coords=c('long', 'lat'), remove=FALSE) |>
  st_set_crs('WGS84') |>
  st_transform(st_crs(sa3_nz))

coords <- st_coordinates(schools_sf)
schools_sf <- schools_sf |> st_drop_geometry() |>
  mutate(x = coords[,1], y = coords[,2]) |> select(-geometry)
