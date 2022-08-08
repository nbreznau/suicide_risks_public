pacman::p_load("tidyverse",
               "countrycode")

socx <- read_csv(here::here("data", "socx_public_oecd.csv"))

socx <- socx %>%
  mutate(iso3c = countrycode(cow_code, "cown", "iso3c"),
         socx = value) %>%
  select(iso3c, year, socx)

write.csv(socx, here::here("output", "socx.csv"), row.names = F)
