library(tidyverse)
library(readxl)
library(here)

# Data from https://www.hbs.edu/behavioral-finance-and-financial-stability/data/Pages/global.aspx

crisis_raw <- read_excel(here("data", "20160923_global_crisis_data.xlsx"),
                         na = c("", "n/a")) %>%
  janitor::clean_names() %>%
  filter(!is.na(case)) %>%
  mutate(across(matches("(_crisis$|_crises$)"), ~as.numeric(.)))

crisis_idx <- crisis_raw %>%
  # select crisis variables (should be five, define missing variable later)
  select(c(3:5), matches("(_crisis$|_crises$)")) %>%
  # data already contains country code variable, just rename
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c")) %>%
  # adapt naming
  rename_with(~str_replace(., "_crises", "_crisis")) %>%
  mutate(crisis_idx = rowSums(.[3:6], na.rm = TRUE)) %>%
  select(-c(country))

write.csv(crisis_idx, here::here("output", "crisis_idx.csv"))
