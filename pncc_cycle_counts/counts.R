library(tidyverse)
library(readxl)

# These are days only
counts <- read_excel("~/data/data/pncc_cycle_counts/export (2).xlsx", skip=2) %>%
  set_names("Times", "Pedestrian1", "Pedestrian2", "Cyclist1", "Cyclist2") %>%
  pivot_longer(-Times, names_to = c("Mode", "Direction"), names_pattern = "(.*)([12])", values_to = "Count")

# These are 15 minutes, convert to days
counts2 <- read_excel("~/data/data/pncc_cycle_counts/14513_2020-04-30-12-05-52.xlsx") %>%
  set_names("Times", "Skip", "Pedestrian1", "Pedestrian2", "Cyclist1", "Cyclist2") %>%
  pivot_longer(-c(Times,Skip), names_to = c("Mode", "Direction"), names_pattern = "(.*)([12])", values_to = "Count") %>%
  select(-Skip)

counts3 <- read_excel("~/data/data/pncc_cycle_counts/He Ara Kotahi - 1 March to 31 July - 15 minute blocks.xlsx", skip=2) %>%
  set_names("Times", "Skip", "Pedestrian1", "Pedestrian2", "Cyclist1", "Cyclist2") %>%
  pivot_longer(-c(Times,Skip), names_to = c("Mode", "Direction"), names_pattern = "(.*)([12])", values_to = "Count") %>%
  select(-Skip)

daily_counts1 <- bind_rows(counts2, counts3) %>%
  unique() %>%
  filter(!is.na(Count)) %>%
  mutate(Date = as.Date(Times)) %>%
  filter(Date < "2020-07-31") %>%
  group_by(Date, Mode, Direction) %>%
  summarise(Count = sum(Count))

final_daily_counts <- bind_rows(counts %>% mutate(Date = as.Date(Times)), daily_counts1)

ggplot(final_daily_counts %>% filter(Date >= "2020-06-01")) +
  geom_line(aes(x=Date, y=Count)) +
  geom_smooth(aes(x=Date, y=Count)) +
  facet_grid(vars(Mode), vars(Direction))

one_year <- final_daily_counts %>% filter(Date >= "2020-06-01")

final_one_year <- one_year %>%
  select(Date, Mode, Direction, Count) %>%
  mutate(Direction = if_else(Direction == 1, "To Massey", "To City"))

write_csv(final_one_year, "~/data/data/pncc_cycle_counts/he_ara_kotahi_one_year.csv")
