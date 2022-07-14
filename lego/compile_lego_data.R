library(tidyverse)

themes <- read_csv("data-raw/themes.csv")

# Set processing
sets <- read_csv("data-raw/sets.csv")

# Nice :)
sets %>% left_join(themes) %>%
  ggplot() +
  geom_bar(aes(x=year, fill=theme)) +
  guides(fill='none')

# Now try and get the parts in there?
inventories <- read_csv("data-raw/rebrickable/inventories.csv.gz")
inventory_parts <- read_csv("data-raw/rebrickable/inventory_parts.csv.gz")
inventory_minifigs <- read_csv("data-raw/rebrickable/inventory_minifigs.csv.gz")

# minifigs are 'sets' as well. So are subsets of sets really.
# so for each set, to get all the parts, you first need to use the parts, then
# then also the minifigs, then the minifig parts from there?

minifigs <- read_csv("data-raw/rebrickable/minifigs.csv.gz")

minifig_parts <- minifigs %>% left_join(inventories, by=c("fig_num" = "set_num")) %>%
  group_by(fig_num) %>% filter(version == max(version)) %>%
  left_join(inventory_parts, by=c("id" = "inventory_id")) %>%
  select(-id, -version)

# check the number of parts match: Yay!
minifig_parts %>%
  group_by(fig_num, num_parts) %>%
  summarise(n=sum(quantity)) %>%
  filter(n != num_parts)

# OK, do the same for the main 'set' parts
set_parts <- sets %>% left_join(inventories, by=c("set_num" = "set_num")) %>%
  group_by(set_num) %>% filter(version == max(version)) %>%
  left_join(inventory_parts, by=c("id" = "inventory_id")) %>%
  select(-id, -version) %>%
  filter(!is.na(part_num))

set_parts %>% group_by(set_num, num_parts) %>%
  summarise(n = sum(quantity[!is_spare])) %>%
  filter(n > num_parts) # no sets have too many parts!

set_parts %>% group_by(set_num, num_parts) %>%
  summarise(n = sum(quantity[!is_spare])) %>%
  filter(n < num_parts) # lots have too few. Mostly they'll be minifigs ofcourse!

# find the set -> minifig map
set_minifigs <- sets %>% left_join(inventories, by=c("set_num" = "set_num")) %>%
  group_by(set_num) %>% filter(version == max(version)) %>%
  left_join(inventory_minifigs, by=c("id" = "inventory_id")) %>%
  filter(!is.na(fig_num)) %>%
  select(-id, -version)

set_minifig_parts <- set_minifigs %>%
  rename(minifig_quantity = quantity) %>%
  left_join(minifig_parts %>% select(fig_num, part_num, color_id, quantity, is_spare)) %>%
  mutate(quantity = quantity * minifig_quantity) %>%
  select(-minifig_quantity, -fig_num) %>%
  filter(!is.na(part_num))

# tie the set_parts and set_minifig_parts together
sets_all_parts <- bind_rows(set_parts, set_minifig_parts)

sets_all_parts %>% group_by(set_num, num_parts) %>%
  summarise(n = sum(quantity[!is_spare])) %>%
  filter(n != num_parts) # YAY!

parts <- read_csv("data-raw/parts.csv")

colors <- read_csv("data-raw/colors.csv")

color_map <- colors %>%
  select(color, rgb) %>%
  deframe()

all_part_info <- sets_all_parts %>% left_join(colors) %>%
  left_join(parts, by="part_num") %>%
  select(set = set_num,
         part = part_num,
         color,
         quantity,
         is_spare, weight,
         x,y,z)

# do some plots :)
set_info <- sets %>%
  left_join(themes) %>%
  rename(set = set_num) %>%
  select(-theme_id) %>%
  left_join(all_part_info)

set_info %>%
  group_by(year, set) %>% summarise(weight = sum(weight, na.rm=TRUE)) %>%
  ggplot() + 
  geom_boxplot(aes(x=as_factor(year), y=weight))

set_info %>%
  group_by(year, set) %>% summarise(parts = sum(quantity)) %>%
  ggplot() + 
  geom_boxplot(aes(x=as_factor(year), y=parts))

set_info %>% uncount(quantity)

library(vegan)
my_shannon <- function(value, count) {
  diversity(table(rep(value, times=count)))
}
set_info %>%
  group_by(year, set) %>% summarise(col_div = my_shannon(color, quantity)) %>%
  ggplot() + 
  geom_boxplot(aes(x=as_factor(year), y=col_div))

set_info %>%
  group_by(year, set, color) %>% summarise(quantity = sum(quantity*weight, na.rm=TRUE)) %>%
  mutate(col_div = diversity(quantity)) %>%
  ggplot() + 
  geom_boxplot(aes(x=as_factor(year), y=col_div))

# nice, now order by colours around a colour wheel?
set_info %>%
  group_by(year, color) %>% summarise(quantity = sum(quantity, na.rm=TRUE)) %>%
  left_join(colors) %>%
  ggplot() + 
  geom_col(aes(x=year, y=quantity, fill=fct_reorder(color, sort_order)), position='fill',
           width=1) +
  guides(fill = 'none') +
  scale_fill_manual(values = color_map)

# nice, now order by colours around a colour wheel?
top_themes <- set_info %>% select(set, theme) %>%
  unique() %>% count(theme) %>% slice_max(n, n=12)
set_info %>%
#  semi_join(top_themes) %>%
  group_by(year, color) %>% summarise(quantity = sum(quantity, na.rm=TRUE)) %>%
  left_join(colors) %>%
  ggplot() + 
  geom_col(aes(x=year, y=quantity, fill=fct_reorder(color, sort_order)), position='fill',
           width = 1) +
  guides(fill = 'none') +
  scale_fill_manual(values = color_map) +
  facet_wrap(vars(theme), ncol=3)

