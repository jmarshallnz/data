library(tidyverse)
library(lubridate)
library(readxl)
library(TSP)

ni1 <- read_excel("~/data/data/LAWA/riverwqmonitoringdata_northisland_2004-2021-_1of2.xlsx", guess_max = Inf, na = c("", "NA"), sheet=2)
ni2 <- read_excel("~/data/data/LAWA/riverwqmonitoringdata_northisland_2004-2021_2of2.xlsx", guess_max = Inf, na = c("", "NA"), sheet=2)
si <- read_excel("~/data/data/LAWA/riverwqmonitoringdata_southisland_2004-2021.xlsx", guess_max = Inf, na = c("", "NA"), sheet="RiverWQMonitoringDataSI") %>%
  rename(`REC Landcover` = RECLandcover,
         `Landuse_council WFS` = Landuse_councilWFS,
         `Altitude_council WFS` = Altitude_councilWFS)

setdiff(names(ni1), names(ni2))
setdiff(names(ni1), names(si))

if (0) {
  # could potentially append old stuff here...
  
  ni <- read_csv("~/data/data/LAWA/RiverWQMonitoringData_NorthIsland_2004-18.csv",
                 col_types = cols(RawValue = col_character(),
                               Symbol = col_character()))
  si <- read_csv("~/data/data/LAWA/RiverWQMonitoringData_SouthIsland_2004-18.csv",
                 col_types = cols(RawValue = col_character(),
                                  Symbol = col_character()))
}
all <- bind_rows(ni1, ni2, si) %>% mutate(Date = as_date(Date)) |>
  rename(LawaSiteID = `LAWA ID`)

# NOTE: Some sites have unique SiteIDs. I think this can happen when there's a lawa site and a regional council site that are the same.
#       Often the site names differ, but lat/long don't. Sometimes, you can get different names with same site id too...
all %>% group_by(LawaSiteID) %>% summarise(n = n_distinct(SiteID)) %>%
  filter(n != 1)

# Hmm, we have a new measure TURBNFU. Let's see how that works out...
wider <- all %>%
  select(Agency, Region, SiteID, Catchment, LandCover = `REC Landcover`, Altitude = `Altitude_council WFS`,
         Lat = Latitude, Long = Longitude, Date, Indicator, Value = `Censored Value`) %>%
  pivot_wider(names_from = Indicator, values_from = Value, values_fn = first)

# seems pretty much correlated with TURB, so just replace when needed
wider %>% filter(!is.na(TURBFNU)) %>%
  ggplot(aes(x=TURB, y=TURBFNU)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col='red') +
  scale_x_log10() +
  scale_y_log10()

final <- wider %>% mutate(TURB = coalesce(TURBFNU, TURB)) %>%
  select(-TURBFNU)

final %>% names()

# write out the big dataset
write_csv(final, "~/data/data/LAWA/lawa_2021.csv.gz")
# NH4 = ammonia and ammonium
# TURB = Turbidity
# BDISC = Black Disc
# DRP = Dissolved reactive phosphorus (in the water)
# ECOLI = E. coli
# TN = Total Nitrogen
# TP = Total Phosphorus
# PH = pH
# TON = Total Oxygenated Nitrogen nitrite nitrogen+ nitrate nitrogen

sites <- final %>% select(Latitude = Lat, Longitude = Long, SiteID) %>% group_by(SiteID, Latitude, Longitude) %>% summarise(Count = n()) %>%
  tibble::rowid_to_column("LocationID")

# Cluster the SiteID spatially by taking a travelling salesman tour through all sites
set.seed(7)
etsp <- ETSP(data.frame(x=sites$Longitude, y=sites$Latitude), labels = sites$LocationID)
tour <- solve_TSP(etsp)
plot(etsp, tour)
plot(etsp, tour, xlim=c(173,175), ylim=c(-42,-39))
text(etsp, labels=sites$LocationID, col=rainbow(10))
foo <- cut_tour(tour, 826, exclude_cut = TRUE)
plot(etsp)
lines(etsp[foo,])

plot(etsp)
plot(etsp[foo[1:106],], col='red')
lines(etsp[foo[1:106],])

# What we want is 100 stretches through 1056 observations. So we should start at
i <- 1:100
num_per_student <- 155
student_rows <- tibble(student = 1:100, start = round((length(foo)-num_per_student)/(100-1) * (student-1) + 1),
                       end = start + num_per_student - 1) %>%
  mutate(end = pmin(end, length(foo)))

grab_sites <- function(student, start, end) {
  rows = foo[start:end]
  sites[rows,]
}
foo <- student_rows %>% pmap_dfr(grab_sites, .id = "StudentID")

# Right, now dump this data out as spreadsheet
datasets <- foo %>% ungroup() %>% group_split(StudentID)

dump_data <- function(subset, out_path = ".") {
  out_data <- subset %>% select(SiteID) %>% left_join(final, by="SiteID") %>%
    select(SiteID, everything()) %>%
    select(-Agency, -Region, -Catchment)
  student = subset %>% pull(StudentID) %>% first() %>% as.numeric()
  write_csv(out_data, file.path(out_path, sprintf("lawa%02i.csv", student %% 100)))
}

out_path <- here::here("LAWA/LAWA")
fs::dir_create(out_path)
out <- map(datasets, dump_data, out_path = out_path)

out %>% bind_rows(.id="Student") %>%
  group_by(Student) %>%
  count(LandCover) %>%
  pivot_wider(names_from=LandCover, values_from=n) %>%
  filter(is.na(Urban))
