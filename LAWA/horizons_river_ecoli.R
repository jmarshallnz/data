# These data come from: https://www.lawa.org.nz/download-data

# we then unzip, extracting the CSV
library(tidyverse)
library(lubridate)
library(readxl)

ni1 <- read_excel("LAWA/riverwqmonitoringdata_northisland_2006-2020_1of2.xlsx", guess_max = 60000, na = c("", "NA"))
ni2 <- read_excel("LAWA/riverwqmonitoringdata_northisland_2006-2020_2of2.xlsx", guess_max = 250000, na = c("", "NA"))

ni <- bind_rows(ni1, ni2)

# could bind to 2004 data as well possibly?
if (0) {
  ni_2004 <- read_csv("~/data/data/LAWA/RiverWQMonitoringData_NorthIsland_2004-18.csv",
                      col_types = cols(RawValue = col_character(),
                                       Symbol = col_character())) %>% mutate(Date = dmy(Date))
}

hrc <- ni %>% rename(Site = SiteID) %>% extract(LawaSiteID, into=c("Council", "SiteID"), regex="(.*)-([0-9]+)") %>%
  filter(Council == "hrc", Indicator == "ECOLI", Value > 0, Agency == "Horizons Regional Council") %>%
  mutate(Council = str_to_upper(Council)) %>%
  arrange(SiteID, DateSampled) %>%
  group_by(SiteID) %>% mutate(Site = first(Site[order(nchar(Site))])) %>%
  select(Council, SiteID, Site, Indicator, Date = DateSampled, RawValue, Symbol, Value, Region)

dir.create("LAWA/hrc")
write_csv(hrc, "LAWA/hrc/horizons_river_ecoli.csv")

