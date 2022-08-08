library(tidyverse)
library(readxl)
library(here)

#Data from http://data.un.org/Data.aspx?q=divorce+&d=GenderStat&f=inID%3a23

div_rate_raw <- read_csv(here("data", "UNdata_Export_20210222_101217772.csv")) %>%
  janitor::clean_names()

div_rate <- div_rate_raw %>%
  # separate subgroup variable, warning because "yr" gets cut off
  separate("subgroup", into = c("region", "sex", "age"), sep = " ") %>%
  filter(region == "Total") %>%
  group_by(country_or_area, year, sex) %>%
  summarise(div_rate = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(iso3c = countrycode::countrycode(country_or_area, "country.name", "iso3c")) %>%
  pivot_wider(names_from = sex, names_prefix = "div_rate", names_sep = "_", values_from = div_rate) %>%
  janitor::clean_names()

# Add iso code for Czechoslovakia (former), German Democratic Republic (former),
# Netherlands Antilles, Northern Ireland, Scotland, Serbia and Montenegro

saveRDS(div_rate, here::here("output/div_rate.RDS"))
