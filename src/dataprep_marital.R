library(tidyverse)
library(readxl)
library(here)

# Data from: https://population.un.org/MarriageData/Index.html#/home

marital_stat_raw <- read_excel(here("data", "UNPD_WMD_2019_MARITAL_STATUS.xlsx"), skip = 2, sheet = 4) %>%
  janitor::clean_names()

# Change/add country codes for Kosovo, Neth. Antilles, Channel islands

marital_stat <- marital_stat_raw %>%
  group_by(country_or_area, year_end, sex) %>%
  summarise(marit_rate = mean(data_value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(iso3c = countrycode::countrycode(country_or_area, "country.name", "iso3c")) %>%
  pivot_wider(names_from = sex, names_prefix = "marit_rate", names_sep = "_", values_from = marit_rate) %>%
  janitor::clean_names()

# Change/add country codes for Kosovo, Neth. Antilles, Channel islands

saveRDS(marital_stat, here::here("output/marital_stat.RDS"))
