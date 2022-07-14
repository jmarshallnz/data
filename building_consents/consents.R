library(tidyverse)
library(lubridate)

all <- read_csv("~/data/data/building_consents/New dwellings consented per 1000 residents by region (Annually).csv")

all %>% count(Series_title_1)

all %>% filter(Series_title_1 == "New Zealand") %>%
  mutate(Period = as.character(Period)) %>%
  separate(Period, into=c("Year", "Month")) %>%
  mutate(Date = ymd(sprintf("%s-%s-01", Year, Month))) %>%
  ggplot() +
  geom_line(aes(x=Date, y=Data_value)) +
  labs(y = "New dwelling consents per 1000 residents")
