library(tidyverse, countrycode)

# From the Correlates of War project and World Religion Project
religion <- read_csv(here::here("data/WRP_national.csv"))

# From the World Religion Projections project
install.packages("jsonlite", repos="https://cran.rstudio.com/")
library("jsonlite")

json_file <- 'https://datahub.io/sagargg/world-religion-projections/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    assign(paste0("data_",i), read.csv(url(path_to_file)))
    #data <- read.csv(url(path_to_file))
    print(data)
  }
}

religion <- religion %>%
  subset(year > 1989) %>%
  mutate(iso3c = name) %>%
  select(iso3c, year, chrstprotpct:othrgenpct)

# make general codes
religion <- religion %>%
  mutate(christpct = chrstprotpct + chrstorthpct + chrstcatpct,
         islampct = islmsunpct + islmshipct + islmibdpct + islmnatpct + islmalwpct + islmahmpct,
         hindupct = hindgenpct,
         buddhapct = budmahpct + budthrpct + budothrpct + budgenpct,
         jewishpct = judconspct + judorthpct + judrefpct + judothrpct + judconspct,
         otherpct = 1 - (christpct + islampct + hindupct + buddhapct + jewishpct + nonreligpct)) %>%
  select(iso3c, year, christpct:otherpct, nonreligpct)

saveRDS(religion, here::here("output/religion.Rds"))
