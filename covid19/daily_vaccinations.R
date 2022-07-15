## Data is taken from the covid19nz github:
## https://github.com/jmarshallnz/covid19nz
## Run the export_data.R script
library(tidyverse)

vacc <- read_csv("covid19/daily_vaccinations.csv")

# Some questions we could ask about:

# The day with the most vaccinations occurred on Super Saturday (October 16th 2021).
# Our data starts on October 22nd, a week later.

# Which day had the most vaccinations (of any dose) administered since then?

vacc |> group_by(Date) |>
  summarise(Number = sum(Number)) |>
  arrange(desc(Number)) # hmm, 19th of February!

# How many of each dose was done on that day? Produce
# a table of doses by DHB for this date, including
# a row and column for totals (hint: see janitor::adorn_totals)
vacc |> filter(Date == "2022-02-19") |>
  select(DHB, Dose, Number) |>
  pivot_wider(names_from=Dose, values_from=Number) |>
  janitor::adorn_totals(where = c('row', 'col'))

# One DHB managed to vaccinate more people than their reported population.
# On which day was this achieved, and in which DHB?
vacc |> filter(Total > Population)

# Choose FOUR (4) DHBs, and produce a plot of the proportion
# of people vaccinated with dose 1 or dose 2 through time for
# the four DHBs.

# You should have a total of 8 lines/curves (4 DHBs, 2 doses).
# Consider carefully your use of titles, axis labels, panels,
# and legends.
vacc |> filter(Dose != 3) |>
  filter(DHB %in% c("Auckland", "MidCentral", "Northland", "Southern")) |>
  ggplot() +
  geom_line(mapping=aes(x=Date, y=Total/Population, col=factor(Dose))) +
  facet_wrap(vars(DHB))

# You should notice a 'step' where the proportion vaccinated drops. This
# was when Ministry of Health revised the number of people vaccinated down (by removing
# those that may have died or moved districts from the data). On which date was this?
vacc |> group_by(Date) |> summarise(Number = sum(Number)) |>
  arrange(Number) # 17th March 2022
