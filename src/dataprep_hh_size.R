library(tidyverse)
library(readxl)
library(here)

# Data from https://www.un.org/development/desa/pd/data/household-size-and-composition

hh_size_raw <- read_excel(here("data", "undesa_pd_2019_houseshold_size_and_composition_dataset.xlsx"),
                          sheet = "UN HH Size and Composition 2019", skip = 4, na = c("", "..")) %>%
  janitor::clean_names()

hh_size <- hh_size_raw %>%
  mutate(iso3c = countrycode::countrycode(iso_code, "iso3n", "iso3c"),
         # extract only year
         year = lubridate::year(reference_date_dd_mm_yyyy)) %>%
  select(iso3c, year, c(5:9)) %>%
  # remove "x" from var. names
  rename_with(~str_remove_all(., "^x")) %>%
  # add count for duplicates
  add_count(iso3c, year) %>%
  # filter out duplicates (multiple data sources) with missing values
  filter(n==1 | (n>1 & !if_any(c(3:7), ~is.na(.)))) %>%
  group_by(iso3c, year) %>%
  # calculate mean for duplicates from multiple data sources
  mutate(across(matches("member"), ~ifelse(n > 1, mean(.), .))) %>%
  # remove duplicates
  distinct() %>%
  ungroup() %>%
  select(-"n")

saveRDS(hh_size, here("output", "hh_size.RDS"))
