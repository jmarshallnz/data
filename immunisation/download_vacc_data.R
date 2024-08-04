library(tidyverse)
library(rvest)
library(lubridate)
library(readxl)

ht <- xml2::read_html("https://www.health.govt.nz/our-work/preventative-health-wellness/immunisation/immunisation-coverage/national-and-dhb-immunisation-data")

urls1 <- ht %>% html_nodes(css = "a[href$='.xls']") %>% html_attr("href") %>% str_subset("3[-_ ]*month")
urls2 <- ht %>% html_nodes(css = "a[href$='.xlsx']") %>% html_attr("href") %>% str_subset("3[-_ ]*month")
urls3 <- ht %>% html_nodes(css = "a[href$='.xlsx']") %>% html_attr("href") # %>% str_subset()

excel_sheets <- ht %>% html_nodes(css = "a[href$='.xls']")

three_month <- excel_sheets%>% html_text() %>% str_detect("January .*? March") |
  excel_sheets%>% html_text() %>% str_detect("January .*? April") |
  excel_sheets %>% html_text() %>% str_detect("October .*? December") |
  excel_sheets %>% html_text() %>% str_detect("October .*? January") |
  excel_sheets %>% html_text() %>% str_detect("July .*? September") |
  excel_sheets %>% html_text() %>% str_detect("July .*? October") |
  excel_sheets %>% html_text() %>% str_detect("April .*? June") |
  excel_sheets %>% html_text() %>% str_detect("April .*? July")
  
urls1 <- excel_sheets[three_month] %>% html_attr("href")

excel_sheets <- ht %>% html_nodes(css = "a[href$='.xlsx']")

three_month <- excel_sheets%>% html_text() %>% str_detect("January .*? March") |
  excel_sheets %>% html_text() %>% str_detect("October .*? December") |
  excel_sheets %>% html_text() %>% str_detect("July .*? September") |
  excel_sheets %>% html_text() %>% str_detect("April .*? June")

urls2 <- excel_sheets[three_month] %>% html_attr("href")

urls <- c(urls1, urls2)

# download the excel sheets and extract the data
base_url <- "https://www.health.govt.nz/"
base_url <- "https://www.tewhatuora.govt.nz"

cat("Processing...\n", file="log.txt", append=FALSE)
download_excel <- function(url, base_url) {
  # First download the file...
  source <- file.path(base_url, url)
  out <- tempfile(fileext = fs::path_ext(url))
  if (download.file(source, out) == 0) {
    # This is a bit nasty assuming the sheets will have the same layout, but that's what happens when you go and put
    # headings in 2 rows...
    eth <- c("Total", "NZE", "Maori", "Pacific", "Asian", "Other")
    cat <- c("Eligible", "Immunised", "Percent")
    names <- expand_grid(Eth=eth, Cat=cat) %>% unite(EthCat, Eth, Cat) %>% pull(EthCat)
    # METHOD 1:
    method1 <- function(file) {
      read_sheet <- function(sheet, file) {
        # Find the date we want (fucking lack of consistency, what is this bullshit)
        rows <- read_excel(file, range="A1:A31", col_names=FALSE, sheet=sheet) %>% as_vector()
        date_row <- which(str_detect(rows, "The report"))
        date <- read_excel(file, range=paste0("A", date_row), col_names=FALSE, sheet=sheet) %>% as_vector()
        date <- str_match(date, "[0-9]+ and[ ]+([0-9].*[0-9]+)")[2]
        # Find the block we want (fucking lack of consistency, what is this bullshit)
        block_start <- which(str_detect(rows, "Auckland"))
        block_end   <- which(str_detect(rows, "National"))
        read_excel(file, sheet=sheet, range = paste0("A", block_start, ":S", block_end, collapse=""), na = c("n/s", "-"), col_names = FALSE, col_types=c("text", rep("numeric", 18))) %>%
          set_names("DHB", names) %>%
          pivot_longer(-DHB, names_to=c("Ethnicity", "Item"), names_sep="_", values_to = "value") %>%
          mutate(Date = dmy(date), Age = sheet)
      }
      sheets <- excel_sheets(file)
      map_dfr(sheets, read_sheet, file=file)
    }
    # METHOD 2:
    method2 <- function(file) {
      # need to figure out the date
      date_cell <- read_excel(file, range="A1", col_names=FALSE, sheet=1) %>% as_vector()
      date <- str_match(date_cell, " to (.*) \\(report")[2]
      if (is.na(date)) {
        date <- str_match(date_cell, "[0-9]-(.*) \\(report")[2]
      }
      read_excel(file, skip = 3) %>%
        rename(Age = `Milestone age for CI`,
               DHB = `DHB of residence`) %>%
        rename_with(.fn = ~ names, .cols = -c(Age, DHB)) %>%
        fill(Age, DHB) %>%
        mutate(across(all_of(names), as.numeric)) %>%
        pivot_longer(-c(DHB,Age), names_to=c("Ethnicity", "Item"), names_sep="_", values_to = "value") %>%
        mutate(Date = dmy(date))
    }
    
    method2b <- function(file, skip=2) {
      # need to figure out the date
      date_cell <- read_excel(file, range="A1", col_names=FALSE, sheet=1) %>% as_vector()
      date <- str_match(date_cell, "[0-9]-(.*) \\(report")[2]
      read_excel(file, skip = skip, sheet="Ethnicity") %>%
        rename(Age = `Milestone age`,
               DHB = `DHB of residence`) %>%
        rename_with(.fn = ~ names, .cols = -c(Age, DHB)) %>%
        fill(Age, DHB) %>%
        mutate(across(all_of(names), as.numeric)) %>%
        pivot_longer(-c(DHB,Age), names_to=c("Ethnicity", "Item"), names_sep="_", values_to = "value") %>%
        mutate(Date = dmy(date))
    }
    # METHOD 3:
    method3 <- function(file) {
      # need to figure out the date
      date_cell <- read_excel(file, range="A1", col_names=FALSE, sheet="Ethnicity") %>% as_vector()
      date <- str_match(date_cell, "- (.*)\\)")[2]
      dhb_cells <- read_excel(file, range="B1:B7", col_names=FALSE, sheet="Ethnicity") |>
        set_names('field') |> rowid_to_column('row') |>
        filter(str_detect(field, "DHB")) |> slice(1) |> as.list()
      read_excel(file, skip = 5, sheet="Ethnicity") |>
        rename(Age = `Milestone age`,
               DHB = dhb_cells$field) |>
        rename_with(.fn = ~ names, .cols = -c(Age, DHB)) |>
        fill(Age, DHB) |>
        mutate(across(all_of(names), as.numeric)) |>
        pivot_longer(-c(DHB,Age), names_to=c("Ethnicity", "Item"), names_sep="_", values_to = "value") |>
        mutate(Date = dmy(date))
    }
    method3b <- function(file) {
      # need to figure out the date
      date_cell <- read_excel(file, range="A1", col_names=FALSE, sheet="Ethnicity") %>% as_vector()
      date <- str_match(date_cell, "- ?(3.*)\\)")[2]
      dhb_cells <- read_excel(file, range="B1:B20", col_names=FALSE, sheet="Ethnicity") |>
        set_names('field') |> rowid_to_column('row') |>
        filter(str_detect(field, 'residence')) |> slice(1) |> as.list()
      region_cells <- read_excel(file, range="C1:C20", col_names=FALSE, sheet="Ethnicity") |>
        set_names('field') |> rowid_to_column('row') |>
        filter(!is.na(field)) |> slice(1) |> as.list()
      too_many_rows <- read_excel(file, range = paste0("A", dhb_cells$row, ":U40000"), sheet="Ethnicity") |>
        rename(Age = `Milestone age`,
               DHB = dhb_cells$field,
               Region = region_cells$field) |>
        rename_with(.fn = ~ names, .cols = -c(Age, DHB, Region))
      # filter out where all the rows are NA
      empty <- apply(too_many_rows, 1, \(x) sum(is.na(x))) == ncol(too_many_rows)
      last_row <- max(which(!empty))
      my_parse_number <- function(col) {
        if (is.numeric(col)) {
          col
        } else {
          readr::parse_number(col)
        }
      }
      too_many_rows |> slice(1:last_row) |>
        fill(Age, DHB) |>
        mutate(across(all_of(names), my_parse_number)) |>
        pivot_longer(-c(DHB,Age,Region), names_to=c("Ethnicity", "Item"), names_sep="_", values_to = "value") |>
        mutate(Date = dmy(date))
    }
    # figure out WTF method we should use
    top_cell <- read_excel(out, range="A1", col_names=FALSE, sheet=1) %>% as_vector()
    if (str_detect(top_cell, "11 October 2021")) {
      # Special cases suck
      method2b(file=out)
    } else if (str_detect(top_cell, "12 July 2021")) {
      method2b(file=out, skip=3)
    } else if (str_detect(top_cell, "^Childhood")) {
      # Newer layout
      method2(file=out)
    } else if (str_detect(top_cell, "22\\-23")) {
      # 22/23
      method3b(file=out)
    } else if (str_detect(top_cell, "23\\-24")) {
      # TODO: Changed to group EU/Other together
      data.frame()
    } else if (str_detect(top_cell, "Quarterly childhood")) {
      # Even newer
      method3(file=out)
    } else {
      method1(file=out)
    }
  } else {
    cat("Shitty data for source:", source, "\n", file="log.txt", append=TRUE)
    data.frame()
  }
}

# try and grab them all
foo <- map_dfr(urls, download_excel, base_url=base_url)

if (0) {
  bad <- urls[[28]]
  download_excel(bad, base_url=base_url)
}

if (0) {
  download_excel(url = urls[28], base_url=base_url)
}
# tidy stuff up
age_cats <- c("6 months", "8 months", "12 months", "18 months", "24 months", "54 months", "5 years")

final <- foo %>% mutate(Age = str_extract(tolower(Age), "[0-9]+ (months|years)")) |>
  filter(!is.na(Age)) |>
  mutate(DHB = fct_collapse(DHB, `Capital & Coast` = c("Capital & Coast", "Capital and Coast"),
                            MidCentral = c("Midcentral", "MidCentral"),
                            `Hawkes Bay` = c("Hawkes Bay", "Hawke's Bay"),
                            Southern = c("Otago", "Southland", "Southern"),
                            National = c("National", "National Total", "National total"),
                            Tairawhiti = c("Tairawhiti", "Tairāwhiti"),
                            Waitemata = c("Waitematā", "Waitemata")),
         Age = factor(Age, levels=age_cats))

# checking stuff - have too many southern's but the for_real_final below fixes stuff up
final %>% count(Age)
final %>% count(DHB) %>% as.data.frame()

final %>% filter(DHB == "Southern", Age == "6 months", Date == "2009-07-01") %>%
  as.data.frame()

# Too many in Southern and South Canterbury?
final %>% group_by(DHB, Date, Age) %>% summarise(count = n()) %>% filter(count > 18)
final %>% filter(DHB == "South Canterbury", Date == "2010-10-01", Age == "5 years") %>% as.data.frame()

for_real_final <- final %>% group_by(DHB, Date, Age, Ethnicity, Item) %>% summarise(value = sum(value, na.rm=FALSE)) %>%
  filter(Item != "Percent") %>% pivot_wider(names_from = Item, values_from = value) %>%
  mutate(Immunised = if_else(is.na(Eligible), NA_real_, Immunised)) %>% ungroup()

for_real_final |> filter(!DHB %in% c("Central", "Nothern", "Te Manawa Taki", "Te Waipounamu")) |>
  write_csv("immunisation/vaccinations_all.csv")

for_real_final %>% filter(DHB == "National") %>%
  mutate(Year = year(Date),
         YearGroup = case_when(Year %in% 2012:2013 ~ "2012_2013",
                               Year %in% 2018:2019 ~ "2017_2018",
                               TRUE ~ NA_character_)) %>%
  filter(!is.na(YearGroup)) %>% group_by(YearGroup, Ethnicity, Age) %>% summarise(value = sum(Immunised)) %>%

ggplot(aes(x=YearGroup, y=value, fill=Ethnicity)) + geom_col() +
  facet_wrap(~Age) + scale_y_continuous() +
  theme_bw() +
  ggtitle("National vaccination rates by ethnicity")
