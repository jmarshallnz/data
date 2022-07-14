library(tidyverse)
library(httr)
library(jsonlite)
library(data.table)

# TODO: Go over the Brickset API to get pricing information for each set (where available!)
brickset_url <- "https://brickset.com/api/v3.asmx"
brickset_apikey <- "3-gIl4-KeJP-9QbNq"

# grab all the set details from brickset by year seems sensible enough...

# read set info from rebrickable
sets <- read_csv("data-raw/rebrickable/sets.csv.gz")

set_years <- sets %>% count(year)

from_brickset_api <- function(years, pagenum = 1, fetched_so_far = NULL) {
  url <- parse_url(brickset_url)
  url$path <- file.path(url$path, "getSets")
  year_query <- years
  cat("running from_brickset_api on years=", years, "\n")
  if (length(years) == 1)
    year_query <- unbox(years)
  params <- toJSON(list(year = year_query,
                        pageSize = unbox(500),
                        pageNumber = unbox(pagenum),
                        extendedData = unbox(1)))
  url$query = list(apiKey=brickset_apikey,
                   userHash='',
                   params=params)
  
  print(url)
  print(build_url(url))
  r <- GET(build_url(url))

  print(r$status_code)
  stop_for_status(r)

  foo <- jsonlite::fromJSON(r %>% content(as="text", encoding="UTF-8"))

  # some loop here to re-call the function while increasing the number of pages?
  if (foo$status == "error") {
    stop(foo$message)
  }
  sets_found <- bind_rows(fetched_so_far, foo$sets)
  num_fetched <- nrow(sets_found)
  if (foo$matches > num_fetched) {
    # re-call in a loop until we achieve our goal? Can we just re-call this?
    # Hmm, how do we know when to stop???
    sets_found <- from_brickset_api(years, pagenum+1, sets_found)
  }
  sets_found
}

# OK, now we run our price loop and we're done :)
set_info <- set_years %>%
  mutate(test = map(year, from_brickset_api))

library(data.table)
brickset_info <- set_info %>% as.data.table() %>%
  as.data.frame() %>% select(-starts_with('extendedData'))

brickset_info <- read_csv("data-raw/brickset/brickset.csv", col_types = cols(.default = col_character()))
# join them up :)
sets %>% left_join(brickset_info %>%
                     mutate(set_num = paste(number, numberVariant, sep="-")) %>%
                     select(set_num, price = LEGOCom.US.retailPrice )) %>%
  mutate(price = as.numeric(price)) %>%
  write_csv("data-raw/sets.csv")
