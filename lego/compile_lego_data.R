library(tidyverse)

themes <- read_csv("lego/data-raw/themes.csv")

# Set processing
sets <- read_csv("lego/data-raw/sets.csv")

# Nice :)
sets %>% left_join(themes) %>%
  ggplot() +
  geom_bar(aes(x=year, fill=theme)) +
  guides(fill='none')

# Now try and get the parts in there?
inventories <- read_csv("lego/data-raw/rebrickable/inventories.csv.gz")
inventory_parts <- read_csv("lego/data-raw/rebrickable/inventory_parts.csv.gz")
inventory_minifigs <- read_csv("lego/data-raw/rebrickable/inventory_minifigs.csv.gz")

# minifigs are 'sets' as well. So are subsets of sets really.
# so for each set, to get all the parts, you first need to use the parts, then
# then also the minifigs, then the minifig parts from there?

minifigs <- read_csv("lego/data-raw/rebrickable/minifigs.csv.gz")

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

parts <- read_csv("lego/data-raw/parts.csv")

colors <- read_csv("lego/data-raw/colors.csv")

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
  group_by(year, set, price, theme, num_parts) %>% summarise(weight = sum(weight, na.rm=TRUE)) %>%
  ggplot() + 
  geom_point(aes(x=weight, y=price, col=log10(num_parts))) +
  scale_x_log10() +
  scale_y_log10()

set_info %>%
  group_by(year, set, price) %>% summarise(parts = sum(quantity)) %>%
  ggplot() + 
  geom_boxplot(aes(x=as_factor(year), y=parts)) +
  scale_y_log10()

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
set_info %>% filter(theme %in% c("Technic", "Town", "Creator", "Friends", "Architecture", "LEGO Ideas and CUUSOO", "Ninjago", "Modular Buildings", "Star Wars")) %>%
#  semi_join(top_themes) %>%
  mutate(theme = if_else(theme == "LEGO Ideas and CUUSOO", "Ideas", theme)) %>%
  filter(year > 2011) %>%
  left_join(colors %>% select(color, sort_order)) %>%
  filter(!is.na(quantity)) %>%
  select(set, name, year, num_parts, price, theme, part, colour_id=sort_order, quantity) ->
  lego2011

lego2011 |> select(set, name, year, num_parts, price, theme) |>
  unique() |>
  write_csv("lego/lego_sets.csv")

lego2011 |> select(set, part, colour_id, quantity) |>
  write_csv("lego/lego_parts.csv")

write_csv(lego2011, "lego/lego.csv.gz")

colours <- colors %>% select(colour_id=sort_order, rgb, colour=color)
write_csv(colours, "lego/lego_colours.csv")

lego <- read_csv("lego/lego.csv.gz")

colours <- read_csv("lego/lego_colours.csv")

colour_map <- colours %>% deframe()

lego %>%
  group_by(colour_id, theme) %>%
  summarise(quantity = sum(quantity)) %>%
  ggplot() + 
  geom_col(aes(y=theme, x=quantity, fill=as_factor(colour_id)), position='fill') +
  guides(fill = 'none') +
  scale_fill_manual(values = colour_map)

lego %>% select(set, name, num_parts, price, theme) %>%
  unique() %>%
  filter(price < num_parts) %>%
  ggplot(mapping=aes(x=theme, y=price/num_parts)) +
  geom_boxplot() #+\
#  scale_y_log10()
#  geom_smooth(aes(col=theme))
#  facet_wrap(vars(theme))
             
# hints:
# 1. You need to use facet_wrap() to get each theme in a separate panel.
# 2. Each panel has a different scale - see the `scales` argument of `facet_wrap()`.
# 3. You can turn off axis labels and tick marks by setting them to `element_blank()` using `theme()`.
# 4. Check the different `theme's` to match the look.
# 5. Don't worry about trying to match the font size!

#  facet_wrap(vars(theme), ncol=3, scales='free_y') #+
#  labs(y=NULL, x=NULL) +
#  theme_bw() +
#  scale_x_continuous(expand=c(0,0)) +
#  scale_y_discrete(expand=c(0,0)) +
#  theme(axis.text = element_blank(),
#        axis.ticks = element_blank())
