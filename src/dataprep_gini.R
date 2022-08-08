pacman::p_load("tidyverse","countrycode")

swiid_summary <- read_csv(here::here("data/swiid9_0_summary.csv"))

# create clean merging data

# recode countries

swiid_summary$iso3c <- countrycode(swiid_summary$country, "country.name","iso3c")

# select variables

gini <- select(swiid_summary, iso3c, year, gini_disp:gini_mkt_se)

write_csv(gini, file = "output/gini.csv")

# we can supplement this with the OWD gini

gini_owd <- read_csv(here::here("data/economic-inequality-gini-index.csv")) %>%
  janitor::clean_names() %>%
  rename(iso3c = code, country = entity) %>%
  arrange(iso3c, year)

write_csv(gini_owd, file = "output/gini_owd.csv")
