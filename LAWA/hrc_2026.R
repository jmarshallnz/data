library(tidyverse)
library(readxl)

foo <- read_excel("~/data/data/LAWA/2026/LAWA River Water Quality Monitoring Data_North Island 2of3_9Feb2026.xlsx", sheet=2,
                  na="NA")

mw <- foo |> filter(Region == "manawatū-whanganui",
                    Agency == "Horizons Regional Council")

test <- mw |>
  mutate(Year = year(SampleDateTime), Month = month(SampleDateTime)) |>
  group_by(LawaSiteID, Indicator, Year, Month) |>
  summarise(Value = mean(Value))

test |> ungroup() |>
  complete(LawaSiteID, Indicator, Year, Month) |>
  group_by(Indicator) |>
  summarise(missingness = sum(is.na(Value)), n=n()) # OK, so quite a bit of missingness for some items

wide <- test |>
  pivot_wider(names_from=Indicator, values_from=Value) |>
  mutate(Date = ymd(sprintf('%04i-%02i-01', Year, Month)))

ggplot(wide) +
  aes(x=Date, y=E.coli) +
  geom_point() +
  scale_y_log10()

ggplot(wide) +
  aes(col=LawaSiteID, x=`Total nitrogen`, y=`Total phosphorus`) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  guides(col='none')

write_csv(wide, 
          '~/data/data/LAWA/hrc_2026.csv.gz')
library(naniar)
vis_miss(wide)

  group_by(Indicator) |>
  summarise(min_mpm = min(n), max_mpm = max(n)) # INTERESTING!
