#-------------------------------------
# Tidying of alcohol consumption data
#-------------------------------------

# Packages ----------------------------------------------------------------

pacman::p_load("here", "tidyverse", "countrycode")


# Data tidying function ---------------------------------------------------

tidy_func <- function(df_raw, filename){

  # Read raw data
  df_raw <- read.csv(file = here("data", filename), skip = 1, header = TRUE)

  df_raw %>%
    # Rename columns
    rename_with(~ str_remove(., "^X\\.|\\.") %>%
                  tolower()) %>%
    # Pivot year columns to long fomat
    pivot_longer(cols = -c(1:3), names_to = "year", values_to = "litres_pc") %>%
    # Add country code variable
    mutate(iso3c = countrycode(sourcevar = country, "country.name", "iso3c")) %>%
    # Select variables
    select(-datasource)
}


# Tidy files, finalise data set -------------------------------------------


alc1960 <- tidy_func(alc1960_raw, "SA_0000001400_1960.csv")

alc1980 <- tidy_func(alc1980_raw, "SA_0000001400_1980.csv")

alc2000 <- tidy_func(alc2000_raw, "SA_0000001400_2000.csv")

alc2010 <- tidy_func(alc2010_raw, "SA_0000001400_2010.csv")


alcohol_pc <-
  rbind(alc1960, alc1980, alc2000, alc2010) %>%
  mutate(year = as.numeric(year),
         beveragetypes = str_trim(beveragetypes)) %>%
  arrange(country, beveragetypes, desc(year))

write.csv(alcohol_pc, here("output", "alcohol_pc.csv"), row.names = FALSE)


