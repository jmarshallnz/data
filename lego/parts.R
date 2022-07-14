library(tidyverse)
library(httr)

# find some more info on the parts
parts <- read_csv("data-raw/rebrickable/parts.csv.gz")

# link to bricklink info where we can
bl_part_info <- read.table("data-raw/bricklink/bricklink_parts.txt", sep="\t", quote = "", header=TRUE, fill=TRUE) %>%
  select(part = Number, Weight = Weight..in.Grams., Dimensions)

# some 30k parts aren't in the bricklink list. Prob lots of these are sticker sheets etc
parts %>% anti_join(bl_part_info, by=c("part_num" = "part"))

# what about the other way around
bl_part_info %>% anti_join(parts, by=c("part" = "part_num")) %>%
  select(part) %>% unique() %>% nrow() # and some 57k bricklink parts aren't in rebrickable

# try and create a mapping from rebrickable to bl for those that don't have info?
unknown_parts <- parts %>% anti_join(bl_part_info, by=c("part_num" = "part")) %>%
  select(part_num) %>% unique()

# maybe hit the API for this?

query_parts <- function(part_numbers) {
  message("querying parts ", part_numbers[1], " to ", part_numbers[length(part_numbers)])
  url <- parse_url("https://rebrickable.com/api/v3/lego/parts/")
  
  url$query = list(part_nums = paste0(part_numbers, collapse=","),
                   inc_part_details = 1,
                   inc_color_details = 0)
  
  get_from_url <- function(url) {
    # sleep for a bit to be nice :)
    Sys.sleep(1)
    r <- GET(url, add_headers(Authorization="key 0525a12615335f1cb04a8f109166d240"))
    # check the response content
    # check we have successful response
    if (status_code(r) == 200) {
      # success!
      data = jsonlite::fromJSON(r %>% content(as="text", encoding="UTF-8"))$results
      # check for a `next` URL
      next_url <- content(r)$`next`
      if (!is.null(next_url)) {
        # recurse in I guess?
        message("recursing to next URL")
        next_data <- get_from_url(next_url)
        data = bind_rows(data, next_data)
      }
    } else {
      # hmmm
      warning("Status code is", status_code(r), "\n")
      data = data.frame()
    }
    data
  }
  
  get_from_url(build_url(url))
}

part_details <- unknown_parts %>% mutate(batch = row_number() %/% 100) %>%
  group_by(batch) %>% summarise(query_parts(part_num))

bricklink_ids <- part_details %>% pull(external_ids) %>%
  mutate(BrickLink = map(BrickLink, head, n=1),
         BrickLink = as.character(BrickLink),
         BrickLink = na_if(BrickLink, "NULL")) %>% pull(BrickLink)

part_map <- part_details %>% ungroup() %>% select(part_num, year_from, year_to) %>%
  mutate(bricklink = bricklink_ids)

part_map %>% anti_join(bl_part_info %>% rename(bricklink=part)) # down to 3.8k now :)

matched <- parts %>% semi_join(bl_part_info, by=c("part_num"="part"))
unmatched <- parts %>% anti_join(bl_part_info,  by=c("part_num"="part"))
unmatched_info <- unmatched %>% left_join(part_map) %>%
  left_join(bl_part_info %>% rename(bricklink=part)) %>%
  select(-year_from, -year_to, -bricklink)
matched_info <- matched %>% left_join(bl_part_info, by=c("part_num"="part"))
all_info <- bind_rows(matched_info, unmatched_info)

part_categories <- read_csv("data-raw/rebrickable/part_categories.csv.gz")

full_parts_info <- all_info %>% mutate(Weight = as.numeric(Weight)) %>%
  separate(Dimensions, into=c("x", "y", "z"), sep=" x ") %>%
  mutate(across(x:z, as.numeric)) %>%
  rename(weight = Weight) %>%
  left_join(part_categories %>% rename(part_category = name, part_cat_id = id), by=c("part_cat_id")) %>%
  select(-part_cat_id)

write_csv(full_parts_info, "data-raw/parts.csv")
