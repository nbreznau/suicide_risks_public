library(tidyverse)
library(data.table)
library(zoo)

# we also use the combined version of 'Our World in Data' GDP to interpolate missing
# https://ourworldindata.org/grapher/gross-domestic-product
owd_gdp <- read_csv(here::here("data/gross-domestic-product.csv"))
pop <- read_csv(here::here("data/mpd2018i.csv"))

# for some reason Somalia is missing after 1990 in the World Bank data
# here we substitute UN data. it is the poorest country in the world so it
# is a rather uncontroversial data substitute

somalia <- as.data.frame(matrix(nrow = 29, ncol = 3))

somalia[,1] <- "SOM"
somalia[1:29,2] <- seq.int(1989, 2017, 1)
somalia[29:1,3] <- c(107,103,105,123,135,120,92,91,106,228,224,222,221,
195,154,128,142,231,223,234,187,164,150,153,136,80,84,138,137)

colnames(somalia) <- c("iso3c","year","gdp_madk")

mad_gdp <- readxl::read_xlsx(here::here("data/mpd2020.xlsx"), sheet = "Full data")

mad_gdp <- subset(mad_gdp, year > 1988)
pop <- pop %>%
  subset(year > 1988) %>%
  mutate(popi = pop) %>%
  select(countrycode, year, popi)

mad_gdp <- full_join(mad_gdp, pop, by = c("countrycode","year"))

#make all possible years for all countries
mad_gdpDT <-data.table(mad_gdp)
mad_gdpDT <- mad_gdpDT[, .(year = 1989:2018), by = .(countrycode)][]

mad_gdp <- left_join(as.data.frame(mad_gdpDT), mad_gdp, by = c("countrycode", "year"))

mad_gdp <- mad_gdp %>%
  mutate(iso3c = countrycode,
         gdp_madk = gdppc/1000,
         pop = ifelse(is.na(pop), popi, pop))

# interpolate missing pop, although none seem to be missing
mad_gdp <- mad_gdp %>%
  group_by(iso3c) %>%
         mutate(popi = na.approx(pop, na.rm = F)) %>%
  select(iso3c, year, gdp_madk, pop, popi)

# combine data
colnames(owd_gdp) <- c("country", "iso3c", "year", "gdp")

owd_gdp <- select(owd_gdp, -country)
owd_gdp <- subset(owd_gdp, year > 1989)

mad_gdp <- full_join(owd_gdp, mad_gdp, by =  c("iso3c","year"))

somalia <- somalia %>%
  mutate(gdp = NA,
         pop = NA,
         popi = NA) %>%
  select(iso3c, year, gdp, gdp_madk, pop, popi)

mad_gdp <- rbind(mad_gdp,somalia)

# fix population estimates
wbpop <- read_csv(here::here("data/wb.transposed.pop.csv"))
wbpop <- melt(wbpop, id.vars=1)
colnames(wbpop) <- c("year","iso3c","pop2")


mad_gdp <- mad_gdp %>%
  left_join(wbpop, by = c("iso3c", "year")) %>%
  mutate(pop = ifelse(is.na(pop), pop2, pop)) %>%
  select(-c(popi, pop2))


# calculate per capita gdp from OWD data
mad_gdp <- mad_gdp %>%
  mutate(gdp_owdk = (gdp/(pop*1000))/1000) # population already in thousands

# interpolate from OWD
m1 <- lm(gdp_madk ~ gdp_owdk, data = mad_gdp)

mad_gdp$gdp_madki <- predict(m1, newdata = mad_gdp)

mad_gdp <- mad_gdp %>%
  mutate(gdp_madki = ifelse(!is.na(gdp_madk), gdp_madk, gdp_madki)) %>%
  select(iso3c, year, gdp_madk, gdp_madki) %>%
  subset(iso3c == "SOM" & !is.na(gdp_madki) | iso3c != "SOM") # two somalias created for some reason, drop empty one

saveRDS(mad_gdp, here::here("output/mad_gdp.RDS"))
