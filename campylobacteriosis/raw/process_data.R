library(tidyverse)
library(readxl)

input_path <- "~/data/data/campylobacteriosis/raw/"

# read in the 2015 data
foo = read.table(file.path(input_path, "2015/raw.txt"), header=FALSE, sep="\t", quote="\"", stringsAsFactors=FALSE)
foo = matrix(foo[,1], nrow=13, ncol=20, byrow=TRUE)
colnames(foo) = foo[1,]
foo = foo[-1,]
foo = apply(foo, 2, as.numeric)
rownames(foo) = month.name
d2015 = as.data.frame(t(foo))
d2015$DHB = rownames(d2015)
d2015$Year = 2015
d2015 = d2015 %>% pivot_longer(cols=January:December, names_to="Month", values_to="Count")

# read in the 2016/2017 data
d2016 <- read.table(file.path(input_path, "2016/2016_cases.txt"), header=TRUE, check.names = FALSE)
d2016 <- d2016 %>% pivot_longer(-Month, names_to="DHB", values_to="Count") %>% mutate(Year = 2016)

d2017 <- read.table(file.path(input_path, "2017/2017_cases.txt"), header=TRUE, check.names = FALSE)
d2017 <- d2017 %>% pivot_longer(-Month, names_to="DHB", values_to="Count") %>% mutate(Year = 2017)

# read in pre-2015 data
files = list.files(file.path(input_path, "2014"), "*.csv")

read_dhb <- function(file, path) {
  foo = read.csv(file.path(path, file), fileEncoding = "UCS2", stringsAsFactors = FALSE)
  dhb = sub("Campylobacteriosis cases by month since 1997 for (.*) District Health Board in New Zealand", "\\1", foo[1,1])
  foo = foo[,2:14]
  names(foo)[1] = "Year"
  foo$DHB = dhb
  foo
}

l = lapply(files, read_dhb, file.path(input_path, "2014"))
d = do.call(rbind, l)
d2014 <- d %>% pivot_longer(January:December, names_to="Month", values_to="Count")

pre2017 <- bind_rows(d2014, d2015, d2016, d2017)

# now read in the rest
xls_files <- list.files(input_path, "*.xlsx?", recursive=TRUE)

read_xlsx <- function(file, input_path) {
  cat("reading", file, "\n")
  # find the fucking row
  row <- read_excel(file.path(input_path, file), range="A1:A10")[[1]] %>%
    grepl(pattern="Campy", .) %>% which()
  dhb_names <- read_excel(file.path(input_path, file), range="C3:V3", col_names=FALSE) %>%
    as.character()
  dat <- read_excel(file.path(input_path, file), range=paste0("C", row, ":V", row+1)) %>%
    set_names(nm = dhb_names) %>%
    pivot_longer(everything(), names_to="DHB", values_to="Count") %>%
    mutate(File = file) %>% mutate(Count = as.numeric(Count))
  dat
}
post2018 <- map_dfr(xls_files, read_xlsx, input_path=input_path)

# read in the earlier 2018 stuff from PDFs
pre_processed <- read_csv(file.path(input_path, "2018/processed_pdfs.csv"))
DHBs <- post2018 %>% filter(File == "2018/201805MayDHB.xlsx") %>% pull(DHB)
pre_processed$DHBs <- ""
pre_processed$DHBs[seq_along(DHBs)] <- DHBs
write_csv(pre_processed, file.path(input_path, "2018/processed_pdfs.csv"))
# HACK IN EXCEL

pdf2018 <- read_csv(file.path(input_path, "2018/finalised_from_pdfs.csv")) %>%
  rename(DHB = DHBs, Count = Counts)
post2018 <- bind_rows(pdf2018, post2018)

post2018 %>% count(DHB) # yay!
d2018 <- post2018 %>% extract(File, into=c("Year", "Month"), regex="([0-9]{4})([0-9]{2})", convert=TRUE) %>%
  mutate(Month = month.name[Month])

all <- bind_rows(d2014, d2015, d2016, d2017, d2018)

all %>% count(Year) %>% filter(n != 240)
all %>% count(Month) %>% filter(n != 480)
all %>% count(DHB) %>% filter(n != 288)

write_csv(all, "~/data/data/campylobacteriosis/processed/counts_by_dhb_1997_2020.csv")

# right, merge in the population file
popn = read_csv("~/data/data/campylobacteriosis/population/Subnational population estimates (DHB, DHB constituency), by age and sex, at 30 June 1996-2020 (2020 boundaries)/TABLECODE7509_Data_2bd8e82b-4359-456d-a2d5-423f5404bcb6.csv")
popn = popn %>% filter(Age == "Total people, age", Sex == "Total people, sex") %>%
  filter(Area != "Total New Zealand by DHB area/DHB constituency",
         Area != "Area outside district health board") %>%
  select(DHB = Area, Year = `Year at 30 June`, Population = Value)

# do some interpolation??
popn

mod <- lm(log(Population) ~ Year*DHB, data=popn)
plot(mod)

my_spline <- function(Year, Population) {
  x = sort(Year)
  xout = seq(min(Year)+1, max(Year)+1, by=1/12)[-1]
  expand.grid(Month = month.name, Year=x[-1]) %>% mutate(Time = xout) %>%
    mutate(Population = spline(x, Population, xout=xout)[[2]])
}

population <- popn %>% group_by(DHB) %>% summarise(my_spline(Year, Population))
write_csv(population, "~/data/data/campylobacteriosis/processed/population_by_dhb_1997_2020.csv")

# join them up
library(lubridate)
all %>% left_join(population) %>%
  select(DHB, Year, Month, Count, Population) %>%
  mutate(Population = round(Population)) %>%
  mutate(Date = ymd(paste(Year, Month, "1"))) %>%
  mutate(Date = add_with_rollback(Date, months(1) - 86400)) %>%
  select(DHB, Date, Cases=Count, Population) %>%
  write_csv("~/data/data/campylobacteriosis/campy_1997_2020.csv")

library(ggplot2)

campy <- read_csv("~/data/data/campylobacteriosis/campy_1997_2020.csv")

ggplot(campy) +
  geom_line(aes(x=Date, y=Population, col=DHB))
