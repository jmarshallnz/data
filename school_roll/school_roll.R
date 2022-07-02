library(tidyverse)

year <- 2021

# yay, this looks like everything, saved the values with zero in it. Let's do some checking though
all = read_csv('school_roll/Machine-Readable-Roll-by-Year-Level-and-Ethnicity-2012-2021.zip') %>%
  filter(`Year: As at 1 July` == year)

#all = read_excel('Roll-by-FYL-and-Ethnic-Group-2007-2017_fullsheet.xlsx', sheet="all_data") %>%


# grab the schools
schools = all %>% select(starts_with("School"), starts_with("Region")) %>% unique
schools = schools %>% rename_with(.fn = ~ substring(., 9), .cols = everything()) %>%
  select(-ID, -`Education Region`, -`MOE Local office`) %>% 
  rename(School = Name, SchoolGender = Gender) %>%
  rename_with( ~ stringr::str_replace_all(., ' ', ''), .cols = everything()) %>%
  arrange(School) %>%
  mutate(RegionalCouncil = stringr::str_replace(RegionalCouncil, " Region", ""),
         TerritorialAuthority = stringr::str_replace(TerritorialAuthority, " City", ""),
         TerritorialAuthority = stringr::str_replace(TerritorialAuthority, " District", ""),
         TerritorialAuthority = stringr::str_replace(TerritorialAuthority, "Auckland- ", "Auckland: "),
         KuraType = stringr::str_replace(KuraType, " \\(Section 15[5-6]\\)", ""),
         Decile = readr::parse_number(Decile))

# and now the student rolls
roll = all %>% select(starts_with('Student'), `School: ID`, `School: Name`) %>%
  rename_with( ~ substring(., 10), starts_with("Student: ")) %>%
  rename(SchoolID = `School: ID`, School = `School: Name`,
         EthnicGroup = `Ethnic Group`, Students = `Students (Σ Values)`,
         Level = `Year level`) %>%
  select(-`Year level group`) %>%
  select(School, Gender, EthnicGroup, Level, Students) %>%
  arrange(School, Level) %>%
  mutate(EthnicGroup = case_when(EthnicGroup == "European\\Pā\u0081kehā\u0081" ~ "European",
                                 EthnicGroup == "Mā\u0081ori" ~ "Māori",
                                 EthnicGroup == "Pacfic" ~ "Pacific",
                                 TRUE ~ EthnicGroup))

roll_nomacrons <- roll %>% mutate(EthnicGroup = str_replace(EthnicGroup, "Māori", "Maori"))

out_dir <- file.path("school_roll", year)
fs::dir_create(out_dir)

write_csv(roll, file.path(out_dir, "roll.csv"))
write_csv(roll_nomacrons, file.path(out_dir, "roll_nomacrons.csv"))
write_csv(schools, file.path(out_dir, "schools.csv"))
