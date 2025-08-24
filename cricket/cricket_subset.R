library(cricketr)
library(tidyverse)
library(lubridate)

if (0) {
  teams <- read_csv("https://www.massey.ac.nz/~jcmarsha/161122/data/ODIs.csv")
  teams |> count(Team) |> write_csv(here::here("cricket/teams.csv"))
}

# read in the data for each team
teams <- read_csv(here::here("cricket/teams.csv"))
all   <- teams |> mutate(dat = map(Team, \(team) getTeamData(matchType = "ODI", teamName = team))) |>
  mutate(dat = map(dat, \(x) select(x, any_of(names(x)[names(x) != ""])))) |>
  rename(Lookup = Team) |>
  unnest(dat) |> extract(Score, into=c("Runs", "Wickets"), regex="([0-9]+)([/0-9])*", convert=TRUE, remove=FALSE) |>
  mutate(Opposition = str_sub(Opposition, 3)) |>
  replace_na(list(Wickets = 10)) |>
  mutate(`Start Date` = dmy(`Start Date`)) |>
  mutate(Year = year(`Start Date`)) |>
  select(Team, Score=Runs, Wickets, Overs, Opposition, Result, Ground, Year) |>
  filter(Opposition %in% teams$Team) |>
  write_csv(here::here("cricket/ODIs_all_majors.csv"))

# filter down to highly played grounds
read_csv(here::here("cricket/ODIs_all_majors.csv")) |>
  add_count(Ground) |>
  filter(n > 50) |>
  select(-n) |>
  write_csv(here::here("cricket/ODIs_major_grounds.csv"))
