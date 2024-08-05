library(tidyverse)

year <- 2023

# yay, this looks like everything, saved the values with zero in it. Let's do some checking though
all = read_csv('school_roll/Machine-Readable-Roll-by-Year-Level-and-Ethnicity-2014-2023.zip') %>%
  filter(`Year: As at 1 July` == year)

#all = read_excel('Roll-by-FYL-and-Ethnic-Group-2007-2017_fullsheet.xlsx', sheet="all_data") %>%


# grab the schools
schools = all %>% select(starts_with("School"), starts_with("Region")) %>% unique()
schools = schools %>% rename_with(.fn = ~ substring(., 9), .cols = everything()) %>%
  select(-`Education Region`, -`MOE Local office`) %>% 
  rename(School = Name, SchoolGender = Gender) %>%
  rename(`Māori Affiliation` = "Māori Medium Peak Body Affiliation") %>%
  rename_with( ~ stringr::str_replace_all(., ' ', ''), .cols = everything()) %>%
  arrange(School) %>%
  mutate(RegionalCouncil = stringr::str_replace(RegionalCouncil, " Region", ""),
         TerritorialAuthority = stringr::str_replace(TerritorialAuthority, " City", ""),
         TerritorialAuthority = stringr::str_replace(TerritorialAuthority, " District", ""),
         TerritorialAuthority = stringr::str_replace(TerritorialAuthority, "Auckland- ", "Auckland: "))

# add equity index
schools_dir <- read_csv("school_roll/directory.csv", skip=15) |>
  select(ID = `School Number`, School2 = `School Name`, EQI = `Equity Index (EQI)`) |>
  mutate(EQI = parse_number(EQI))

schools <- schools |> left_join(schools_dir, by="ID") |>
  select(-School) |>
  select(School = School2, everything()) |> filter(!is.na(School))

# and now the student rolls
roll = all %>% select(starts_with('Student'), `School: ID`, `School: Name`) %>%
  rename_with( ~ substring(., 10), starts_with("Student: ")) %>%
  rename(ID = `School: ID`,
         EthnicGroup = `Ethnic Group`, Students = `Students (∑ Values)`,
         Level = `Year level`) %>%
  select(-`Year level (Grouped)`) %>%
  select(ID, EthnicGroup, Level, Students) %>%
  left_join(schools |> select(ID, School)) %>%
  select(-ID) %>%
  arrange(School, Level) %>%
  mutate(EthnicGroup = case_when(EthnicGroup == "European\\Pā\u0081kehā\u0081" ~ "European",
                                 EthnicGroup == "European\\Pākehā" ~ "European",
                                 EthnicGroup == "Mā\u0081ori" ~ "Māori",
                                 EthnicGroup == "Pacfic" ~ "Pacific",
                                 TRUE ~ EthnicGroup)) %>%
  select(School, everything())

schools <- schools |> select(-ID)
roll_nomacrons <- roll %>% mutate(EthnicGroup = str_replace(EthnicGroup, "Māori", "Maori"))

out_dir <- file.path("school_roll", year)
fs::dir_create(out_dir)

write_csv(roll, file.path(out_dir, "roll.csv"))
write_csv(roll_nomacrons, file.path(out_dir, "roll_nomacrons.csv"))
write_csv(schools, file.path(out_dir, "schools.csv"))

# Tidy up the school column
str_replace('Ä\u0081', 'ā')
str_replace('Å\u008d', 'ō')
str_replace('â€™', "'")

