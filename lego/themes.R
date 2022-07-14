library(tidyverse)

# Theme processing. Lego themes are nested. This code unnests so that only the 'high level' themes are there.
themes <- read_csv("data-raw/rebrickable/themes.csv.gz")

main_themes <- themes %>% rename(join_id = parent_id) %>% 
  left_join(themes %>% select(join_id = id, parent_id), by='join_id') %>%
  mutate(parent_id = if_else(is.na(parent_id), join_id, parent_id)) %>%
  select(-join_id) %>% rename(join_id = parent_id) %>%
  left_join(themes %>% select(join_id = id, parent_name = name, parent_id)) %>%
  mutate(theme = if_else(is.na(parent_name), name, parent_name)) %>%
  select(theme_id = id, theme)

write_csv(main_themes, "data-raw/themes.csv")
