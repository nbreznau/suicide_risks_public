library(tidyverse)
library(WDI)

wdi <- WDI(indicator = c(
  gdp_cap = "NY.GDP.PCAP.KD",
  gdp_cap_growth = "NY.GDP.PCAP.KD.ZG",
  gdp_growth = "NY.GDP.MKTP.KD.ZG",
  unemp_nat = "SL.UEM.TOTL.NE.ZS",
  unemp_ilo = "SL.UEM.TOTL.ZS",
  fertility = "SP.DYN.TFRT.IN",
  fem_labour = "SL.TLF.TOTL.FE.ZS",
  pop = "SP.POP.TOTL",
  pop65 = "SP.POP.65UP.TO.ZS"
)) %>%
  mutate(iso3c = countrycode::countrycode(iso2c, "iso2c", "iso3c")) %>%
  drop_na(iso3c) %>%
  relocate(iso3c) %>%
  select(-country, -iso2c)

saveRDS(wdi, here::here("output/wdi.RDS"))
