library(tidyverse)
library(countrycode)

# Read in data ------------------------------------------------------------

dir(here::here("output")) %>%
  str_subset("raw", negate = TRUE) %>%
  walk(safely(
    ~ .x %>%
      str_extract("^\\w+") %>%
      assign(
        rio::import(paste0(here::here("output"), "/", .x), setclass = "tibble") %>%
          drop_na(iso3c),
        envir = .GlobalEnv
      )
  ))

# From the V-Dem project
regime <- read_csv(here::here("data/vdem_regime__INTERPOLATED_consolidated.csv"))


 # alcohol <- read_csv(here::here("output/alcohol_pc.csv"), col_types = "ccinc") %>%
 #   drop_na(iso3c)
 #
 # gini <- read_csv(here::here("output/gini.csv"), col_types = "icinnnn") %>%
 #   drop_na(iso3c)
 #
 # qog <- read_csv(here::here("output/qog.csv"), col_types = list(year = "i")) %>%
 #   drop_na(iso3c)
 #
 # suicide_ctry_yr <- readRDS(here::here("output/suicide_ctry_yr.RDS")) %>%
 #   drop_na(iso3c)

# Find and deal with problems ---------------------------------------------

alcohol_pc %>%
  distinct() %>%
  count(iso3c, year, sort = TRUE)

alcohol_ctry_yr <- alcohol_pc %>%
  distinct() %>%
  pivot_wider(names_from = beveragetypes, names_prefix = "alc_",
              values_from = litres_pc) %>%
  janitor::clean_names()

gini %>%
  count(iso3c, year, sort = TRUE)

gini %>%
  add_count(iso3c, year) %>%
  filter(n > 1) %>% View()

# Keep values for Russia in years with data for both Russia and Soviet Union
gini <- gini %>%
  group_by(iso3c, year) %>%
  summarise(across(starts_with("gini"), max)) %>%
  ungroup()

gini_owd %>%
  count(iso3c, year, sort = TRUE)

gini_owd <- gini_owd %>%
  rename(gini_owd_worldbk = 4) %>%
  select(-country)

wdi %>%
  count(iso3c, year, sort = TRUE)

suicide_ctry_yr %>%
  count(iso3c, year, sort = TRUE)

suicide_owd %>%
  count(iso3c, year, sort = TRUE)

crisis_idx <- select(crisis_idx, -c(country))

hh_size %>% count(iso3c, year, sort = TRUE)

mad_gdp %>%
  count(iso3c, year, sort = TRUE)

marital_stat <- marital_stat %>%
  group_by(iso3c, year_end) %>%
  slice(1) %>% # Fix Laos
  ungroup()

marital_stat <- marital_stat %>%
  select(-country_or_area) %>%
  rename(year = year_end)

div_rate %>% count(iso3c, year, sort = TRUE)

div_rate <- div_rate %>%
  filter(!str_detect(country_or_area, "Soviet"))

div_rate <- div_rate %>%
  select(-country_or_area)

regime <- regime %>%
  subset(year > 1988) %>%
  mutate(iso3c = iso3) %>%
  select(iso3c, year, regime)


# Join datasets -----------------------------------------------------------

joined_data_full <- suicide_ctry_yr %>%
  full_join(alcohol_ctry_yr, by = c("iso3c", "year")) %>%
  full_join(gini, by = c("iso3c", "year")) %>%
  full_join(gini_owd, by = c("iso3c", "year")) %>%
  full_join(wdi, by = c("iso3c", "year")) %>%
  full_join(mad_gdp, by = c("iso3c", "year")) %>%
  full_join(crisis_idx, by = c("iso3c", "year")) %>%
  full_join(hh_size, by = c("iso3c", "year")) %>%
  full_join(marital_stat, by = c("iso3c", "year")) %>%
  full_join(div_rate, by = c("iso3c", "year")) %>%
  full_join(socx, by = c("iso3c", "year")) %>%
  select(-country)

joined_data_full <- joined_data_full %>%
  left_join(regime, by = c("iso3c", "year")) %>%
  left_join(religion, by = c("iso3c", "year")) %>%
  left_join(war, by = c("iso3c", "year"))

joined_data_no_na <- joined_data_full %>%
  drop_na() # Throws away lots of obs

# Get year range for each country in suicide dataset
year_ranges <- suicide_ctry_yr %>%
  arrange(iso3c) %>%
  group_by(iso3c) %>%
  summarise(firstyear = range(year)) %>% # Misleading name for convenience
  ungroup()

last_years <- year_ranges %>%
  filter(row_number() %% 2 == 0) %>%
  rename(lastyear = firstyear)

# "Ideal" number of observations for each country based on available range
keep_obs <- year_ranges %>%
  filter(row_number() %% 2 == 1) %>%
  inner_join(last_years) %>%
  mutate(firstyear = firstyear - 1, # For lagged independent variables
         year = map2(firstyear, lastyear, ~.x:.y)) %>%
  select(iso3c, year) %>%
  unnest(year)

# Still not very realistic anyway because some countries miss many years in the suicide data
joined_data_ideal <- joined_data_full %>%
  right_join(keep_obs, by = c("iso3c", "year")) %>%
  # some counries' names are missing, fill them in
  mutate(country = countrycode(iso3c, "iso3c","country.name"))

# TODO:
# - divorce data
# - imputation

saveRDS(joined_data_ideal, here::here("output/joined_raw.Rds"))


# Do same as above with owd suicide data

# Get year range for each country in suicide dataset
year_ranges <- suicide_owd %>%
  arrange(iso3c) %>%
  group_by(iso3c) %>%
  summarise(firstyear = range(year)) %>% # Misleading name for convenience
  ungroup()

last_years <- year_ranges %>%
  filter(row_number() %% 2 == 0) %>%
  rename(lastyear = firstyear)

# "Ideal" number of observations for each country based on available range
keep_obs <- year_ranges %>%
  filter(row_number() %% 2 == 1) %>%
  inner_join(last_years) %>%
  mutate(firstyear = firstyear - 1, # For lagged independent variables
         year = map2(firstyear, lastyear, ~.x:.y)) %>%
  select(iso3c, year) %>%
  unnest(year)

# Still not very realistic anyway because some countries miss many years in the suicide data
joined_data_ideal_owd <- joined_data_full %>%
  # Remove old suicide cols # KEEP for comparison
  # select(-starts_with("suicide")) %>%
  # Remove remaining NA observations from suicide cols # KEEP so that we don't break the time series
  # filter(if_any(-c(1:2), ~!is.na(.))) %>%
  full_join(suicide_owd, by = c("iso3c", "year")) %>%
  right_join(keep_obs, by = c("iso3c", "year")) %>%
  mutate(country = countrycode(iso3c, "iso3c","country.name"))


saveRDS(joined_data_ideal_owd, here::here("output/joined_raw_owd.Rds"))


#--- Nate's ----
#saveRDS(owd_suicide, here::here("output/joined_1990_raw.Rds"))
