library(tidyverse)

# Get raw data ------------------------------------------------------------

mortality_urls <- c(
  "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd07.zip",
  "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd08.zip",
  "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd09.zip",
  "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part1.zip",
  "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part2.zip",
  "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part3.zip",
  "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part4.zip",
  "https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part5.zip"
)

# Names of files to be downloaded
mortality_zips <- paste0(here::here("data"), "/", "Morticd", c(7:9, seq(10.1, 10.5, .1)), ".zip")

# Download zip files
walk2(mortality_urls, mortality_zips, curl::curl_download)

# Unzip downloaded files
walk(mortality_zips, unzip, exdir = paste0(here::here("data"), "/"))

# Download WHO Mortality country code list
mort_countries <- curl::curl_download(
  "https://www.who.int/healthinfo/statistics/country_codes.zip?ua=1",
  here::here("data/Mort_ccodes.zip")
) %>%
  unzip(exdir = tempfile()) %>%
  rio::import(format = ",")

# Read in data ------------------------------------------------------------

# Paths of the extracted files
raw_csvs <- paste0(here::here("data"), "/", dir(here::here("data"), "^Mort.*\\d$"))

suicide_codes <- list(ICD_10_1 = c("UE63", paste0("X", c(60:84, 600:849)), "Y870"),
                      ICD_10_2 = c("UE63", paste0("X", c(60:84, 600:849)), "Y870"),
                      ICD_10_3 = c("UE63", paste0("X", c(60:84, 600:849)), "Y870"),
                      ICD_10_4 = c("UE63", paste0("X", c(60:84, 600:849)), "Y870"),
                      ICD_10_5 = c("UE63", paste0("X", c(60:84, 600:849)), "Y870"),
                      ICD_7 = c("A148", "B049"),
                      ICD_8 = c("A147", "B049"),
                      ICD_9 = c("B54", "C102"))

# Read in raw data and filter for suicide
suicide_raw <- map2_df(.x = suicide_codes, .y = raw_csvs,
                       ~ rio::import(.y, format = ",",
                                     # Avoid error when column is empty in one file:
                                     colClasses = c(SubDiv = "character"),
                                     setclass = "tbl_df") %>%
                         filter(Cause %in% .x) %>%
                         mutate(List = as.character(List)), # ICD_10_3$List has type integer,
                       .id = "ICD") %>%
  left_join(mort_countries, by = c("Country" = "country")) %>% # Get country names
  mutate(iso3c = countrycode::countrycode(name, "country.name", "iso3c")) # Add ISO-3 codes

# Check if every country-year appears only in one data file
suicide_raw %>%
  distinct(Country, Year, ICD) %>%
  count(Country, Year, sort = TRUE)

saveRDS(suicide_raw, here::here("output/suicide_raw.RDS"))

file.remove(dir(here::here("data"), pattern = "^Mort", full.names = TRUE))

# Clean data --------------------------------------------------------------

suicide_raw <- readRDS(here::here("output/suicide_raw.RDS"))

suicide_ctry_yr <- suicide_raw %>%
  rename(suicide_total = Deaths1, suicide_95_up = Deaths25, suicide_age_na = Deaths26) %>%
  # Rename Deaths2:Deaths6 to age_0 to age_4 based on digit(s) at the end
  rename_with(~ paste0("suicide_", as.integer(str_extract(.x, "\\d+$")) - 2),
              Deaths2:Deaths6) %>%
  # Rename Deaths7:Deaths24 to corresponding age bins, same method
  rename_with(~ paste0("suicide_", (as.integer(str_extract(.x, "\\d+$")) - 6) * 5,
                       "_", (as.integer(str_extract(.x, "\\d+$")) - 6) * 5 + 4),
              Deaths7:Deaths24) %>%
  rename(suicide_infant_0 = IM_Deaths1,
         suicide_infant_1_6 = IM_Deaths2,
         suicide_infant_7_27 = IM_Deaths3,
         suicide_infant_28_364 = IM_Deaths4,
         year = Year, sex = Sex) %>%
  group_by(iso3c, year, sex) %>%
  # Calculate number of deaths per age group by country, year, and sex
  summarise(across(starts_with("suicide_"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sex = case_when(sex == 1 ~ "m",
                         sex == 2 ~ "f",
                         TRUE ~ NA_character_)) %>%
  drop_na(sex) %>% # Very few anyway
  # Pivot wider to make each row a country-year
  pivot_wider(names_from = sex, values_from = starts_with("suicide"))

saveRDS(suicide_ctry_yr, here::here("output/suicide_ctry_yr.RDS"))
