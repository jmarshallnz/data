library(tidyverse)
library(farver)
library(TSP)

colors <- read_csv("data-raw/rebrickable/colors.csv.gz") %>%
  rename(color_id = id,
         color = name) %>%
  mutate(rgb = paste0('#', rgb))

# cluster the hsl data into, say 10 groups
hsl <- decode_colour(colors$rgb, to='hsl')
d <- compare_colour(hsl, from_space = 'hsl', method = 'cie2000')

hc <- hclust(as.dist(t(d)))

# playing with clustering
if (0) {
  # for plotting
  color_map <- colors %>%
    select(color, rgb) %>%
    deframe()
  
  for (k in 3:25) {
    cuts <- cutree(hc, k=k)
    colors$which = cuts
    g <- ggplot(colors) +
      geom_col(aes(x=as_factor(which), y=1, fill=color)) +
      #  guides(fill='none')
      scale_fill_manual(values = color_map, guide='none')
    print(g)
  }
}

# 13 seems to be a pretty good setup visually
cuts <- cutree(hc, k=13)
colors$which = cuts

# reorder the cuts as needed...
new_order <- data.frame(which = c(10,8,9,1,2,4,3,11,13,12,6,5,7)) %>%
  mutate(order = 1:n())

colors <- colors %>% left_join(new_order) %>%
  mutate(which = order) %>% select(-order)

if (0) {
  # check
  color_map <- colors %>%
    select(color, rgb) %>%
    deframe()
  
  ggplot(colors) +
    geom_col(aes(x=which,y=1, fill=color)) +
    scale_fill_manual(values = color_map, guide='none')
}

order_colours <- function(cols) {
  rgb <- decode_colour(cols, to='rgb')
  dist <- compare_colour(rgb, from_space='rgb', method='cie2000')
  
  # We want to solve the minimum path through all colours problem
  # create a travelling salesman problem by adding a dummy point
  # distance zero from the rest
  d <- rbind(0,cbind(0, t(dist)))
  
  # and solve
  etsp <- TSP(as.dist(d))
  tour <- solve_TSP(etsp,method='concorde') # exact - not really needed. Note order is arbitrary, so could return reversed order
  my_order <- cut_tour(tour, 1) - 1
  
  # return the order each should be in
  order(my_order)
}

# Fuck yeah!
final_col <- colors %>%
  group_by(which) %>%
  mutate(order_in_which = order_colours(rgb)) %>%
  mutate(order = which*100 + order_in_which) %>%
  ungroup() %>%
  select(color_id, color, rgb, sort_group = which, sort_order = order)

write_csv(final_col, "~/data/data/lego/colors.csv")

final_col <- read_csv("~/data/data/lego/colors.csv")
color_map <- final_col %>%
  select(color, rgb) %>%
  deframe()

# some reordering can go in here I guess?
# we need to swap the order of groups 2 through 6,8,12
swap_these <- final_col %>% filter(sort_group %in% c(2:6,8,12))
swapped <- swap_these %>% group_by(sort_group) %>%
  arrange(sort_order) %>%
  mutate(sort_order = sort_group*100 + rev(sort_order %% 100))
unswapped <- final_col %>% filter(!sort_group %in% c(2:6,8,12))
final_col <- bind_rows(swapped, unswapped)
write_csv(final_col, "data-raw/colors.csv")

ggplot(final_col) +
  geom_col(aes(x=sort_group,y=1, fill=fct_reorder(color, sort_order))) +
  scale_fill_manual(values = color_map, guide='none')
