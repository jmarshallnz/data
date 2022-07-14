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

final_one_year <- read_csv("~/data/data/pncc_cycle_counts/he_ara_kotahi_one_year.csv")

# create a table of average daily counts for each mode.
# hint: the wday() function in the lubridate package will be useful.
#       pivoting wider will give a more useful table.
data <- final_one_year %>%
  mutate(Day = wday(Date, label=TRUE),
         Month = month(Date, label=TRUE))

# create a table of total counts for each mode (cyclist, pedestrian) by month.
# hint: the month() function in the lubridate package will be useful.
data %>% group_by(Month, Mode) %>%
  summarise(n=mean(Count)) %>%
  pivot_wider(names_from=Mode, values_from=n)

# Generally there are more movements towards Massey than towards the City, particularly
# for pedestrians who perhaps cross back towards town using the Fitzherbert bridge.

# However, in the last year there was a running event that started at Massey and
# crossed towards the city. This should show up in the data where the To City count
# is higher than the To Massey count.

# **Using the data**, find which day this was.
data %>% pivot_wider(names_from = Direction,
                     values_from = Count) %>%
  filter(Mode == "Pedestrian",
         `To Massey` < `To City`)


  mutate(Difference = `To Massey` - `To City`) %>%
  group_by(Day, Mode) %>%
  summarise(diff = mean(Difference)) %>%
  pivot_wider(names_from=Mode, values_from=diff)

data %>% pivot_wider(names_from = Direction,
                     values_from = Count) %>%
  mutate(Difference = `To Massey` - `To City`) %>%
  group_by(Month, Mode) %>%
  summarise(mean(Difference))

  ggplot(mapping = aes(x=Date, y=Difference)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(vars(Mode))

ggplot(one_year %>% mutate(Day = wday(Date, label=TRUE))) +
  geom_boxplot(aes(x=Day, y=Count, col=Direction)) +
  facet_grid(vars(Mode))

ggplot(one_year %>% mutate(Month = month(Date, label=TRUE))) +
  geom_boxplot(aes(x=Month, y=Count, col=Direction)) +
  facet_grid(vars(Mode))

ggplot(one_year %>% mutate(Day = wday(Date, label=TRUE))) +
  geom_line(aes(x=Date, y=Count, col=Direction)) +
  geom_smooth(aes(x=Date, y=Count, col=Direction),
              span=0.5) +
  facet_grid(vars(Mode), vars(Day))


bind_rows(counts, counts2, counts3) %>%
  filter(!is.na(Count)) %>%
  unique() %>%
  mutate(Date = as.Date(Times)) %>%
  mutate(Hour = hour(Times)) %>%
  filter(Date == "2020-07-31", Hour == 0)
  group_by(Date, Hour) %>%
  summarise(n=n()) %>%
  filter(n == 12)


  mutate(Day = wday(Times, label = TRUE))

ggplot(counts, mapping=aes(x=Times, y=Count, col=Direction)) +
  geom_line() +
  geom_smooth() +
  facet_grid(vars(Mode), vars(Day))

names(counts)
