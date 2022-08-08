library(dplyr)
library(countrycode)

# Supplement suicide data with Ritchie, Roser and Esteban Ortiz-Ospina (2021)

owd_suicide <- read_csv(here::here("data/suicide-death-rates.csv")) %>%
  rename(country = Entity, iso3c = Code, year = Year, suicide_rate_total_owd = 4) %>%
  # There are some subtleties to be addressed some day like N Ireland.. for now all iso3c NAs are dropped
  subset(., !is.na(iso3c) & iso3c != "OWID_ABK")

owd_suicide_mf <- read_csv(here::here("data/male-vs-female-suicide.csv")) %>%
  rename(country = Entity, iso3c = Code, year = Year, region = Continent,
         suicide_rate_male_owd = 5, suicide_rate_female_owd = 6) %>%
  subset(., !is.na(iso3c) & iso3c != "OWID_ABK")


suicide_owd <- owd_suicide %>%
  select(-country) %>%
  full_join(owd_suicide_mf, by = c("iso3c", "year")) %>%
  mutate(country = ifelse(iso3c == "VIR", "Virgin Islands",
                          ifelse(iso3c == "FSM", "Micronesia", country))) %>%
  select(-region) %>%
  subset(!is.na(suicide_rate_total_owd)) %>%
  arrange(iso3c, year)


saveRDS(suicide_owd, here::here("output/suicide_owd.RDS"))
