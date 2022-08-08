library(tidyverse, countrycode)

war <- read_csv(here::here("data/war_any_entities_cow.csv"))

war <- war %>%
  subset(year > 1988) %>%
  mutate(iso3c = countrycode(cow_code, "cown", "iso3c"),
         atwar = value,
         iso3c = ifelse(cow_code == 345, "SRB", ifelse(cow_code == 6, "PRI", iso3c))) %>%
  select(iso3c, year, atwar)

saveRDS(war, here::here("output/war.RDS"))
