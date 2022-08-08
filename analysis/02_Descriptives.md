---
title: "Tech 1. Data Prep & Descriptives"
author: "Nate Breznau"
date: "3/4/2021"
output: 
  html_document:
    keep_md: true

---



Data are loaded from the `/src` folder.

**TO DO** Make a list of all sources here (or somewhere)

### Data

Note that we have the raw WHO data (suicide_total_m, suicide_total_f), and
we have the Max Roser 'Our World in Data' data which has WHO plus several supplementary sources.

Note that we use "df_raw" here but we are using "df_imp" the imputed data.

Clean data


```r
df_raw <- readRDS(here::here("data/df_imp_raw.Rds"))

# some cleaning, WHO variables, plus total suicide rate from OWD
df_raw <- df_raw %>%
  mutate(suicide_total_owd = suicide_rate_total_owd*10,
         suicide_rate_female_owd = suicide_rate_female_owd*10,
         suicide_rate_male_owd = suicide_rate_male_owd*10) %>%
  filter(suicide_rate_male_owd != "NaN")
```

#### Descriptives


```r
df_desc <- df_raw %>%
  filter(year > 2009) %>%
  group_by(iso3c) %>%
  summarise(suicide_pcm_owd = mean(suicide_total_owd, na.rm = T),
            suicide_m_pcm_owd = mean(suicide_rate_male_owd, na.rm = T),
            suicide_f_pcm_owd = mean(suicide_rate_female_owd, na.rm = T))

# calculate change score
df_desc90 <- df_raw %>%
  filter(year < 1995) %>%
  group_by(iso3c) %>%
  summarise(suicide_pcm_owd90 = mean(suicide_total_owd, na.rm = T),
            suicide_m_pcm_owd90 = mean(suicide_rate_male_owd, na.rm = T),
            suicide_f_pcm_owd90 = mean(suicide_rate_female_owd, na.rm = T)) %>%
  select(iso3c, suicide_pcm_owd90, suicide_m_pcm_owd90, suicide_f_pcm_owd90)

df_desc <- df_desc %>%
  left_join(df_desc90, by = c("iso3c")) %>%
  mutate(change_total = round(((suicide_pcm_owd90/suicide_pcm_owd)*100)-100),0)

kable_styling(kable(df_desc, digits = 1))
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> iso3c </th>
   <th style="text-align:right;"> suicide_pcm_owd </th>
   <th style="text-align:right;"> suicide_m_pcm_owd </th>
   <th style="text-align:right;"> suicide_f_pcm_owd </th>
   <th style="text-align:right;"> suicide_pcm_owd90 </th>
   <th style="text-align:right;"> suicide_m_pcm_owd90 </th>
   <th style="text-align:right;"> suicide_f_pcm_owd90 </th>
   <th style="text-align:right;"> change_total </th>
   <th style="text-align:right;"> 0 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> AFG </td>
   <td style="text-align:right;"> 94.7 </td>
   <td style="text-align:right;"> 144.2 </td>
   <td style="text-align:right;"> 47.3 </td>
   <td style="text-align:right;"> 103.7 </td>
   <td style="text-align:right;"> 149.4 </td>
   <td style="text-align:right;"> 54.4 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AGO </td>
   <td style="text-align:right;"> 125.4 </td>
   <td style="text-align:right;"> 216.1 </td>
   <td style="text-align:right;"> 48.5 </td>
   <td style="text-align:right;"> 163.0 </td>
   <td style="text-align:right;"> 256.3 </td>
   <td style="text-align:right;"> 69.6 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ALB </td>
   <td style="text-align:right;"> 52.0 </td>
   <td style="text-align:right;"> 74.2 </td>
   <td style="text-align:right;"> 30.8 </td>
   <td style="text-align:right;"> 40.2 </td>
   <td style="text-align:right;"> 56.9 </td>
   <td style="text-align:right;"> 23.9 </td>
   <td style="text-align:right;"> -23 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AND </td>
   <td style="text-align:right;"> 82.0 </td>
   <td style="text-align:right;"> 131.5 </td>
   <td style="text-align:right;"> 30.6 </td>
   <td style="text-align:right;"> 108.4 </td>
   <td style="text-align:right;"> 168.6 </td>
   <td style="text-align:right;"> 39.2 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARE </td>
   <td style="text-align:right;"> 51.4 </td>
   <td style="text-align:right;"> 64.2 </td>
   <td style="text-align:right;"> 18.6 </td>
   <td style="text-align:right;"> 44.9 </td>
   <td style="text-align:right;"> 55.1 </td>
   <td style="text-align:right;"> 23.4 </td>
   <td style="text-align:right;"> -13 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARG </td>
   <td style="text-align:right;"> 108.1 </td>
   <td style="text-align:right;"> 182.2 </td>
   <td style="text-align:right;"> 40.9 </td>
   <td style="text-align:right;"> 106.8 </td>
   <td style="text-align:right;"> 175.9 </td>
   <td style="text-align:right;"> 46.9 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARM </td>
   <td style="text-align:right;"> 85.6 </td>
   <td style="text-align:right;"> 146.0 </td>
   <td style="text-align:right;"> 34.5 </td>
   <td style="text-align:right;"> 38.9 </td>
   <td style="text-align:right;"> 61.0 </td>
   <td style="text-align:right;"> 19.6 </td>
   <td style="text-align:right;"> -55 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ASM </td>
   <td style="text-align:right;"> 66.8 </td>
   <td style="text-align:right;"> 98.9 </td>
   <td style="text-align:right;"> 35.5 </td>
   <td style="text-align:right;"> 67.8 </td>
   <td style="text-align:right;"> 103.1 </td>
   <td style="text-align:right;"> 31.6 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ATG </td>
   <td style="text-align:right;"> 26.7 </td>
   <td style="text-align:right;"> 46.6 </td>
   <td style="text-align:right;"> 8.9 </td>
   <td style="text-align:right;"> 25.9 </td>
   <td style="text-align:right;"> 44.4 </td>
   <td style="text-align:right;"> 10.2 </td>
   <td style="text-align:right;"> -3 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AUS </td>
   <td style="text-align:right;"> 108.5 </td>
   <td style="text-align:right;"> 167.9 </td>
   <td style="text-align:right;"> 51.2 </td>
   <td style="text-align:right;"> 126.1 </td>
   <td style="text-align:right;"> 203.6 </td>
   <td style="text-align:right;"> 51.7 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AUT </td>
   <td style="text-align:right;"> 119.4 </td>
   <td style="text-align:right;"> 191.7 </td>
   <td style="text-align:right;"> 54.2 </td>
   <td style="text-align:right;"> 194.3 </td>
   <td style="text-align:right;"> 308.5 </td>
   <td style="text-align:right;"> 97.8 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AZE </td>
   <td style="text-align:right;"> 43.8 </td>
   <td style="text-align:right;"> 74.0 </td>
   <td style="text-align:right;"> 17.3 </td>
   <td style="text-align:right;"> 30.9 </td>
   <td style="text-align:right;"> 50.8 </td>
   <td style="text-align:right;"> 14.0 </td>
   <td style="text-align:right;"> -30 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BDI </td>
   <td style="text-align:right;"> 130.3 </td>
   <td style="text-align:right;"> 204.6 </td>
   <td style="text-align:right;"> 60.5 </td>
   <td style="text-align:right;"> 202.0 </td>
   <td style="text-align:right;"> 295.6 </td>
   <td style="text-align:right;"> 127.6 </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BEL </td>
   <td style="text-align:right;"> 153.6 </td>
   <td style="text-align:right;"> 227.8 </td>
   <td style="text-align:right;"> 83.6 </td>
   <td style="text-align:right;"> 181.8 </td>
   <td style="text-align:right;"> 273.1 </td>
   <td style="text-align:right;"> 100.9 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BEN </td>
   <td style="text-align:right;"> 115.1 </td>
   <td style="text-align:right;"> 177.7 </td>
   <td style="text-align:right;"> 60.6 </td>
   <td style="text-align:right;"> 108.5 </td>
   <td style="text-align:right;"> 162.2 </td>
   <td style="text-align:right;"> 60.7 </td>
   <td style="text-align:right;"> -6 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BFA </td>
   <td style="text-align:right;"> 130.9 </td>
   <td style="text-align:right;"> 216.6 </td>
   <td style="text-align:right;"> 57.7 </td>
   <td style="text-align:right;"> 134.5 </td>
   <td style="text-align:right;"> 200.0 </td>
   <td style="text-align:right;"> 76.5 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BGD </td>
   <td style="text-align:right;"> 65.6 </td>
   <td style="text-align:right;"> 63.7 </td>
   <td style="text-align:right;"> 66.5 </td>
   <td style="text-align:right;"> 100.8 </td>
   <td style="text-align:right;"> 103.9 </td>
   <td style="text-align:right;"> 95.6 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BGR </td>
   <td style="text-align:right;"> 93.5 </td>
   <td style="text-align:right;"> 155.4 </td>
   <td style="text-align:right;"> 37.9 </td>
   <td style="text-align:right;"> 156.8 </td>
   <td style="text-align:right;"> 244.4 </td>
   <td style="text-align:right;"> 79.7 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BHR </td>
   <td style="text-align:right;"> 45.5 </td>
   <td style="text-align:right;"> 60.3 </td>
   <td style="text-align:right;"> 19.7 </td>
   <td style="text-align:right;"> 60.1 </td>
   <td style="text-align:right;"> 84.4 </td>
   <td style="text-align:right;"> 24.6 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BHS </td>
   <td style="text-align:right;"> 31.4 </td>
   <td style="text-align:right;"> 55.0 </td>
   <td style="text-align:right;"> 10.3 </td>
   <td style="text-align:right;"> 33.0 </td>
   <td style="text-align:right;"> 56.3 </td>
   <td style="text-align:right;"> 12.9 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BIH </td>
   <td style="text-align:right;"> 65.9 </td>
   <td style="text-align:right;"> 110.2 </td>
   <td style="text-align:right;"> 25.4 </td>
   <td style="text-align:right;"> 111.7 </td>
   <td style="text-align:right;"> 183.9 </td>
   <td style="text-align:right;"> 44.0 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BLR </td>
   <td style="text-align:right;"> 229.8 </td>
   <td style="text-align:right;"> 421.3 </td>
   <td style="text-align:right;"> 69.7 </td>
   <td style="text-align:right;"> 242.1 </td>
   <td style="text-align:right;"> 429.4 </td>
   <td style="text-align:right;"> 80.7 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BLZ </td>
   <td style="text-align:right;"> 75.8 </td>
   <td style="text-align:right;"> 132.4 </td>
   <td style="text-align:right;"> 19.6 </td>
   <td style="text-align:right;"> 66.7 </td>
   <td style="text-align:right;"> 105.7 </td>
   <td style="text-align:right;"> 27.2 </td>
   <td style="text-align:right;"> -12 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BMU </td>
   <td style="text-align:right;"> 35.3 </td>
   <td style="text-align:right;"> 65.7 </td>
   <td style="text-align:right;"> 9.1 </td>
   <td style="text-align:right;"> 55.1 </td>
   <td style="text-align:right;"> 88.1 </td>
   <td style="text-align:right;"> 27.2 </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BOL </td>
   <td style="text-align:right;"> 66.4 </td>
   <td style="text-align:right;"> 89.8 </td>
   <td style="text-align:right;"> 44.1 </td>
   <td style="text-align:right;"> 82.2 </td>
   <td style="text-align:right;"> 106.4 </td>
   <td style="text-align:right;"> 59.8 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BRA </td>
   <td style="text-align:right;"> 60.3 </td>
   <td style="text-align:right;"> 100.3 </td>
   <td style="text-align:right;"> 23.4 </td>
   <td style="text-align:right;"> 69.9 </td>
   <td style="text-align:right;"> 115.5 </td>
   <td style="text-align:right;"> 28.0 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BRB </td>
   <td style="text-align:right;"> 39.8 </td>
   <td style="text-align:right;"> 69.6 </td>
   <td style="text-align:right;"> 14.0 </td>
   <td style="text-align:right;"> 49.9 </td>
   <td style="text-align:right;"> 83.9 </td>
   <td style="text-align:right;"> 21.6 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BRN </td>
   <td style="text-align:right;"> 60.1 </td>
   <td style="text-align:right;"> 89.7 </td>
   <td style="text-align:right;"> 30.1 </td>
   <td style="text-align:right;"> 42.4 </td>
   <td style="text-align:right;"> 62.4 </td>
   <td style="text-align:right;"> 20.2 </td>
   <td style="text-align:right;"> -29 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BTN </td>
   <td style="text-align:right;"> 59.2 </td>
   <td style="text-align:right;"> 84.0 </td>
   <td style="text-align:right;"> 31.5 </td>
   <td style="text-align:right;"> 83.2 </td>
   <td style="text-align:right;"> 102.4 </td>
   <td style="text-align:right;"> 63.1 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BWA </td>
   <td style="text-align:right;"> 104.5 </td>
   <td style="text-align:right;"> 160.4 </td>
   <td style="text-align:right;"> 57.3 </td>
   <td style="text-align:right;"> 156.0 </td>
   <td style="text-align:right;"> 288.1 </td>
   <td style="text-align:right;"> 44.4 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CAF </td>
   <td style="text-align:right;"> 172.4 </td>
   <td style="text-align:right;"> 282.9 </td>
   <td style="text-align:right;"> 71.4 </td>
   <td style="text-align:right;"> 172.2 </td>
   <td style="text-align:right;"> 276.5 </td>
   <td style="text-align:right;"> 78.7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CAN </td>
   <td style="text-align:right;"> 109.7 </td>
   <td style="text-align:right;"> 165.3 </td>
   <td style="text-align:right;"> 56.5 </td>
   <td style="text-align:right;"> 133.1 </td>
   <td style="text-align:right;"> 213.4 </td>
   <td style="text-align:right;"> 56.3 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CHE </td>
   <td style="text-align:right;"> 104.3 </td>
   <td style="text-align:right;"> 156.7 </td>
   <td style="text-align:right;"> 55.6 </td>
   <td style="text-align:right;"> 204.8 </td>
   <td style="text-align:right;"> 306.7 </td>
   <td style="text-align:right;"> 109.9 </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CHL </td>
   <td style="text-align:right;"> 104.2 </td>
   <td style="text-align:right;"> 174.9 </td>
   <td style="text-align:right;"> 38.4 </td>
   <td style="text-align:right;"> 202.7 </td>
   <td style="text-align:right;"> 361.2 </td>
   <td style="text-align:right;"> 58.3 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CHN </td>
   <td style="text-align:right;"> 77.8 </td>
   <td style="text-align:right;"> 91.8 </td>
   <td style="text-align:right;"> 64.5 </td>
   <td style="text-align:right;"> 210.9 </td>
   <td style="text-align:right;"> 198.7 </td>
   <td style="text-align:right;"> 225.3 </td>
   <td style="text-align:right;"> 171 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CIV </td>
   <td style="text-align:right;"> 144.1 </td>
   <td style="text-align:right;"> 225.3 </td>
   <td style="text-align:right;"> 57.7 </td>
   <td style="text-align:right;"> 142.3 </td>
   <td style="text-align:right;"> 219.9 </td>
   <td style="text-align:right;"> 56.4 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CMR </td>
   <td style="text-align:right;"> 147.9 </td>
   <td style="text-align:right;"> 217.5 </td>
   <td style="text-align:right;"> 82.9 </td>
   <td style="text-align:right;"> 137.9 </td>
   <td style="text-align:right;"> 189.7 </td>
   <td style="text-align:right;"> 89.5 </td>
   <td style="text-align:right;"> -7 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> COD </td>
   <td style="text-align:right;"> 115.4 </td>
   <td style="text-align:right;"> 197.9 </td>
   <td style="text-align:right;"> 46.0 </td>
   <td style="text-align:right;"> 122.3 </td>
   <td style="text-align:right;"> 197.0 </td>
   <td style="text-align:right;"> 54.3 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> COG </td>
   <td style="text-align:right;"> 140.0 </td>
   <td style="text-align:right;"> 206.4 </td>
   <td style="text-align:right;"> 80.3 </td>
   <td style="text-align:right;"> 170.3 </td>
   <td style="text-align:right;"> 264.5 </td>
   <td style="text-align:right;"> 90.8 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> COL </td>
   <td style="text-align:right;"> 55.6 </td>
   <td style="text-align:right;"> 94.5 </td>
   <td style="text-align:right;"> 19.5 </td>
   <td style="text-align:right;"> 51.8 </td>
   <td style="text-align:right;"> 87.2 </td>
   <td style="text-align:right;"> 18.6 </td>
   <td style="text-align:right;"> -7 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> COM </td>
   <td style="text-align:right;"> 83.7 </td>
   <td style="text-align:right;"> 134.6 </td>
   <td style="text-align:right;"> 39.2 </td>
   <td style="text-align:right;"> 106.4 </td>
   <td style="text-align:right;"> 159.1 </td>
   <td style="text-align:right;"> 56.5 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CPV </td>
   <td style="text-align:right;"> 170.7 </td>
   <td style="text-align:right;"> 285.8 </td>
   <td style="text-align:right;"> 61.0 </td>
   <td style="text-align:right;"> 132.7 </td>
   <td style="text-align:right;"> 243.5 </td>
   <td style="text-align:right;"> 50.5 </td>
   <td style="text-align:right;"> -22 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CRI </td>
   <td style="text-align:right;"> 73.7 </td>
   <td style="text-align:right;"> 131.2 </td>
   <td style="text-align:right;"> 20.6 </td>
   <td style="text-align:right;"> 64.5 </td>
   <td style="text-align:right;"> 113.0 </td>
   <td style="text-align:right;"> 17.5 </td>
   <td style="text-align:right;"> -12 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CUB </td>
   <td style="text-align:right;"> 110.0 </td>
   <td style="text-align:right;"> 175.1 </td>
   <td style="text-align:right;"> 48.3 </td>
   <td style="text-align:right;"> 199.5 </td>
   <td style="text-align:right;"> 247.2 </td>
   <td style="text-align:right;"> 154.4 </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CYP </td>
   <td style="text-align:right;"> 47.0 </td>
   <td style="text-align:right;"> 77.4 </td>
   <td style="text-align:right;"> 17.7 </td>
   <td style="text-align:right;"> 45.8 </td>
   <td style="text-align:right;"> 67.6 </td>
   <td style="text-align:right;"> 24.5 </td>
   <td style="text-align:right;"> -3 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CZE </td>
   <td style="text-align:right;"> 117.2 </td>
   <td style="text-align:right;"> 200.1 </td>
   <td style="text-align:right;"> 39.3 </td>
   <td style="text-align:right;"> 176.5 </td>
   <td style="text-align:right;"> 281.4 </td>
   <td style="text-align:right;"> 86.4 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DEU </td>
   <td style="text-align:right;"> 97.5 </td>
   <td style="text-align:right;"> 152.3 </td>
   <td style="text-align:right;"> 46.3 </td>
   <td style="text-align:right;"> 136.0 </td>
   <td style="text-align:right;"> 210.8 </td>
   <td style="text-align:right;"> 71.3 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DJI </td>
   <td style="text-align:right;"> 85.6 </td>
   <td style="text-align:right;"> 125.9 </td>
   <td style="text-align:right;"> 41.4 </td>
   <td style="text-align:right;"> 96.3 </td>
   <td style="text-align:right;"> 134.7 </td>
   <td style="text-align:right;"> 56.4 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DMA </td>
   <td style="text-align:right;"> 44.6 </td>
   <td style="text-align:right;"> 75.3 </td>
   <td style="text-align:right;"> 13.9 </td>
   <td style="text-align:right;"> 43.7 </td>
   <td style="text-align:right;"> 72.8 </td>
   <td style="text-align:right;"> 18.0 </td>
   <td style="text-align:right;"> -2 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DNK </td>
   <td style="text-align:right;"> 89.6 </td>
   <td style="text-align:right;"> 136.7 </td>
   <td style="text-align:right;"> 44.7 </td>
   <td style="text-align:right;"> 217.5 </td>
   <td style="text-align:right;"> 299.9 </td>
   <td style="text-align:right;"> 139.7 </td>
   <td style="text-align:right;"> 143 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DOM </td>
   <td style="text-align:right;"> 82.6 </td>
   <td style="text-align:right;"> 136.9 </td>
   <td style="text-align:right;"> 29.1 </td>
   <td style="text-align:right;"> 53.4 </td>
   <td style="text-align:right;"> 85.8 </td>
   <td style="text-align:right;"> 23.0 </td>
   <td style="text-align:right;"> -35 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DZA </td>
   <td style="text-align:right;"> 43.3 </td>
   <td style="text-align:right;"> 62.5 </td>
   <td style="text-align:right;"> 23.8 </td>
   <td style="text-align:right;"> 49.0 </td>
   <td style="text-align:right;"> 67.7 </td>
   <td style="text-align:right;"> 30.3 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ECU </td>
   <td style="text-align:right;"> 98.4 </td>
   <td style="text-align:right;"> 150.4 </td>
   <td style="text-align:right;"> 48.7 </td>
   <td style="text-align:right;"> 61.8 </td>
   <td style="text-align:right;"> 86.6 </td>
   <td style="text-align:right;"> 37.9 </td>
   <td style="text-align:right;"> -37 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EGY </td>
   <td style="text-align:right;"> 54.2 </td>
   <td style="text-align:right;"> 80.2 </td>
   <td style="text-align:right;"> 27.1 </td>
   <td style="text-align:right;"> 49.6 </td>
   <td style="text-align:right;"> 67.4 </td>
   <td style="text-align:right;"> 31.2 </td>
   <td style="text-align:right;"> -9 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ERI </td>
   <td style="text-align:right;"> 166.2 </td>
   <td style="text-align:right;"> 291.2 </td>
   <td style="text-align:right;"> 76.5 </td>
   <td style="text-align:right;"> 212.0 </td>
   <td style="text-align:right;"> 360.9 </td>
   <td style="text-align:right;"> 99.7 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ESP </td>
   <td style="text-align:right;"> 57.6 </td>
   <td style="text-align:right;"> 91.1 </td>
   <td style="text-align:right;"> 26.7 </td>
   <td style="text-align:right;"> 72.6 </td>
   <td style="text-align:right;"> 116.1 </td>
   <td style="text-align:right;"> 34.5 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EST </td>
   <td style="text-align:right;"> 140.1 </td>
   <td style="text-align:right;"> 257.1 </td>
   <td style="text-align:right;"> 39.5 </td>
   <td style="text-align:right;"> 318.1 </td>
   <td style="text-align:right;"> 549.0 </td>
   <td style="text-align:right;"> 122.1 </td>
   <td style="text-align:right;"> 127 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ETH </td>
   <td style="text-align:right;"> 103.8 </td>
   <td style="text-align:right;"> 150.8 </td>
   <td style="text-align:right;"> 53.5 </td>
   <td style="text-align:right;"> 192.4 </td>
   <td style="text-align:right;"> 259.2 </td>
   <td style="text-align:right;"> 128.0 </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FIN </td>
   <td style="text-align:right;"> 140.2 </td>
   <td style="text-align:right;"> 215.7 </td>
   <td style="text-align:right;"> 66.0 </td>
   <td style="text-align:right;"> 254.7 </td>
   <td style="text-align:right;"> 414.0 </td>
   <td style="text-align:right;"> 103.9 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FJI </td>
   <td style="text-align:right;"> 99.9 </td>
   <td style="text-align:right;"> 138.9 </td>
   <td style="text-align:right;"> 60.8 </td>
   <td style="text-align:right;"> 100.7 </td>
   <td style="text-align:right;"> 133.3 </td>
   <td style="text-align:right;"> 67.9 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FRA </td>
   <td style="text-align:right;"> 136.6 </td>
   <td style="text-align:right;"> 218.3 </td>
   <td style="text-align:right;"> 63.2 </td>
   <td style="text-align:right;"> 208.0 </td>
   <td style="text-align:right;"> 327.6 </td>
   <td style="text-align:right;"> 105.7 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GAB </td>
   <td style="text-align:right;"> 127.8 </td>
   <td style="text-align:right;"> 222.5 </td>
   <td style="text-align:right;"> 45.1 </td>
   <td style="text-align:right;"> 131.0 </td>
   <td style="text-align:right;"> 219.3 </td>
   <td style="text-align:right;"> 55.9 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GBR </td>
   <td style="text-align:right;"> 72.6 </td>
   <td style="text-align:right;"> 113.2 </td>
   <td style="text-align:right;"> 33.0 </td>
   <td style="text-align:right;"> 91.6 </td>
   <td style="text-align:right;"> 140.8 </td>
   <td style="text-align:right;"> 45.2 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GEO </td>
   <td style="text-align:right;"> 68.9 </td>
   <td style="text-align:right;"> 124.7 </td>
   <td style="text-align:right;"> 21.4 </td>
   <td style="text-align:right;"> 49.4 </td>
   <td style="text-align:right;"> 82.8 </td>
   <td style="text-align:right;"> 22.0 </td>
   <td style="text-align:right;"> -28 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GHA </td>
   <td style="text-align:right;"> 99.1 </td>
   <td style="text-align:right;"> 193.9 </td>
   <td style="text-align:right;"> 20.6 </td>
   <td style="text-align:right;"> 80.0 </td>
   <td style="text-align:right;"> 141.0 </td>
   <td style="text-align:right;"> 22.7 </td>
   <td style="text-align:right;"> -19 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GIN </td>
   <td style="text-align:right;"> 111.9 </td>
   <td style="text-align:right;"> 160.4 </td>
   <td style="text-align:right;"> 67.0 </td>
   <td style="text-align:right;"> 83.5 </td>
   <td style="text-align:right;"> 106.3 </td>
   <td style="text-align:right;"> 62.1 </td>
   <td style="text-align:right;"> -25 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GMB </td>
   <td style="text-align:right;"> 96.9 </td>
   <td style="text-align:right;"> 140.1 </td>
   <td style="text-align:right;"> 57.6 </td>
   <td style="text-align:right;"> 85.7 </td>
   <td style="text-align:right;"> 108.3 </td>
   <td style="text-align:right;"> 63.2 </td>
   <td style="text-align:right;"> -12 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GNB </td>
   <td style="text-align:right;"> 139.6 </td>
   <td style="text-align:right;"> 218.5 </td>
   <td style="text-align:right;"> 72.4 </td>
   <td style="text-align:right;"> 138.6 </td>
   <td style="text-align:right;"> 203.7 </td>
   <td style="text-align:right;"> 79.8 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GNQ </td>
   <td style="text-align:right;"> 91.2 </td>
   <td style="text-align:right;"> 147.8 </td>
   <td style="text-align:right;"> 46.6 </td>
   <td style="text-align:right;"> 172.7 </td>
   <td style="text-align:right;"> 294.3 </td>
   <td style="text-align:right;"> 76.8 </td>
   <td style="text-align:right;"> 89 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GRC </td>
   <td style="text-align:right;"> 35.5 </td>
   <td style="text-align:right;"> 59.2 </td>
   <td style="text-align:right;"> 12.8 </td>
   <td style="text-align:right;"> 34.5 </td>
   <td style="text-align:right;"> 55.6 </td>
   <td style="text-align:right;"> 14.8 </td>
   <td style="text-align:right;"> -3 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GRD </td>
   <td style="text-align:right;"> 48.4 </td>
   <td style="text-align:right;"> 78.8 </td>
   <td style="text-align:right;"> 17.6 </td>
   <td style="text-align:right;"> 62.8 </td>
   <td style="text-align:right;"> 101.8 </td>
   <td style="text-align:right;"> 29.6 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GRL </td>
   <td style="text-align:right;"> 574.8 </td>
   <td style="text-align:right;"> 837.2 </td>
   <td style="text-align:right;"> 293.5 </td>
   <td style="text-align:right;"> 936.9 </td>
   <td style="text-align:right;"> 1362.0 </td>
   <td style="text-align:right;"> 469.6 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GTM </td>
   <td style="text-align:right;"> 57.9 </td>
   <td style="text-align:right;"> 97.9 </td>
   <td style="text-align:right;"> 23.7 </td>
   <td style="text-align:right;"> 125.1 </td>
   <td style="text-align:right;"> 223.6 </td>
   <td style="text-align:right;"> 32.7 </td>
   <td style="text-align:right;"> 116 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GUM </td>
   <td style="text-align:right;"> 201.0 </td>
   <td style="text-align:right;"> 329.2 </td>
   <td style="text-align:right;"> 66.2 </td>
   <td style="text-align:right;"> 174.5 </td>
   <td style="text-align:right;"> 264.8 </td>
   <td style="text-align:right;"> 73.8 </td>
   <td style="text-align:right;"> -13 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GUY </td>
   <td style="text-align:right;"> 281.3 </td>
   <td style="text-align:right;"> 465.6 </td>
   <td style="text-align:right;"> 107.6 </td>
   <td style="text-align:right;"> 214.4 </td>
   <td style="text-align:right;"> 320.1 </td>
   <td style="text-align:right;"> 114.3 </td>
   <td style="text-align:right;"> -24 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HND </td>
   <td style="text-align:right;"> 44.6 </td>
   <td style="text-align:right;"> 73.7 </td>
   <td style="text-align:right;"> 17.9 </td>
   <td style="text-align:right;"> 56.3 </td>
   <td style="text-align:right;"> 96.5 </td>
   <td style="text-align:right;"> 18.4 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HRV </td>
   <td style="text-align:right;"> 120.1 </td>
   <td style="text-align:right;"> 199.7 </td>
   <td style="text-align:right;"> 51.1 </td>
   <td style="text-align:right;"> 201.2 </td>
   <td style="text-align:right;"> 322.2 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HTI </td>
   <td style="text-align:right;"> 87.8 </td>
   <td style="text-align:right;"> 136.0 </td>
   <td style="text-align:right;"> 44.9 </td>
   <td style="text-align:right;"> 108.5 </td>
   <td style="text-align:right;"> 153.4 </td>
   <td style="text-align:right;"> 66.1 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HUN </td>
   <td style="text-align:right;"> 161.4 </td>
   <td style="text-align:right;"> 272.5 </td>
   <td style="text-align:right;"> 67.0 </td>
   <td style="text-align:right;"> 321.8 </td>
   <td style="text-align:right;"> 518.7 </td>
   <td style="text-align:right;"> 152.5 </td>
   <td style="text-align:right;"> 99 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IDN </td>
   <td style="text-align:right;"> 32.6 </td>
   <td style="text-align:right;"> 49.2 </td>
   <td style="text-align:right;"> 16.2 </td>
   <td style="text-align:right;"> 39.8 </td>
   <td style="text-align:right;"> 56.0 </td>
   <td style="text-align:right;"> 24.1 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IND </td>
   <td style="text-align:right;"> 164.6 </td>
   <td style="text-align:right;"> 191.4 </td>
   <td style="text-align:right;"> 138.8 </td>
   <td style="text-align:right;"> 200.1 </td>
   <td style="text-align:right;"> 202.4 </td>
   <td style="text-align:right;"> 197.9 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IRL </td>
   <td style="text-align:right;"> 95.5 </td>
   <td style="text-align:right;"> 155.8 </td>
   <td style="text-align:right;"> 36.6 </td>
   <td style="text-align:right;"> 106.6 </td>
   <td style="text-align:right;"> 168.9 </td>
   <td style="text-align:right;"> 45.3 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IRN </td>
   <td style="text-align:right;"> 58.3 </td>
   <td style="text-align:right;"> 80.6 </td>
   <td style="text-align:right;"> 35.5 </td>
   <td style="text-align:right;"> 71.6 </td>
   <td style="text-align:right;"> 83.4 </td>
   <td style="text-align:right;"> 59.3 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IRQ </td>
   <td style="text-align:right;"> 48.2 </td>
   <td style="text-align:right;"> 65.4 </td>
   <td style="text-align:right;"> 29.9 </td>
   <td style="text-align:right;"> 87.2 </td>
   <td style="text-align:right;"> 100.1 </td>
   <td style="text-align:right;"> 73.8 </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ISL </td>
   <td style="text-align:right;"> 97.4 </td>
   <td style="text-align:right;"> 163.6 </td>
   <td style="text-align:right;"> 33.1 </td>
   <td style="text-align:right;"> 135.1 </td>
   <td style="text-align:right;"> 202.0 </td>
   <td style="text-align:right;"> 70.8 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ISR </td>
   <td style="text-align:right;"> 61.2 </td>
   <td style="text-align:right;"> 101.0 </td>
   <td style="text-align:right;"> 23.6 </td>
   <td style="text-align:right;"> 84.2 </td>
   <td style="text-align:right;"> 128.2 </td>
   <td style="text-align:right;"> 43.4 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ITA </td>
   <td style="text-align:right;"> 51.9 </td>
   <td style="text-align:right;"> 84.7 </td>
   <td style="text-align:right;"> 21.9 </td>
   <td style="text-align:right;"> 71.7 </td>
   <td style="text-align:right;"> 112.3 </td>
   <td style="text-align:right;"> 36.6 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> JAM </td>
   <td style="text-align:right;"> 34.2 </td>
   <td style="text-align:right;"> 58.4 </td>
   <td style="text-align:right;"> 11.0 </td>
   <td style="text-align:right;"> 16.9 </td>
   <td style="text-align:right;"> 28.3 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> -51 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> JOR </td>
   <td style="text-align:right;"> 33.1 </td>
   <td style="text-align:right;"> 49.6 </td>
   <td style="text-align:right;"> 14.1 </td>
   <td style="text-align:right;"> 58.4 </td>
   <td style="text-align:right;"> 75.1 </td>
   <td style="text-align:right;"> 39.7 </td>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> JPN </td>
   <td style="text-align:right;"> 167.5 </td>
   <td style="text-align:right;"> 242.9 </td>
   <td style="text-align:right;"> 94.1 </td>
   <td style="text-align:right;"> 151.6 </td>
   <td style="text-align:right;"> 207.6 </td>
   <td style="text-align:right;"> 99.0 </td>
   <td style="text-align:right;"> -10 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KAZ </td>
   <td style="text-align:right;"> 254.7 </td>
   <td style="text-align:right;"> 444.1 </td>
   <td style="text-align:right;"> 90.4 </td>
   <td style="text-align:right;"> 239.8 </td>
   <td style="text-align:right;"> 408.3 </td>
   <td style="text-align:right;"> 94.8 </td>
   <td style="text-align:right;"> -6 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KEN </td>
   <td style="text-align:right;"> 116.4 </td>
   <td style="text-align:right;"> 201.0 </td>
   <td style="text-align:right;"> 44.2 </td>
   <td style="text-align:right;"> 116.2 </td>
   <td style="text-align:right;"> 173.1 </td>
   <td style="text-align:right;"> 60.1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KGZ </td>
   <td style="text-align:right;"> 114.7 </td>
   <td style="text-align:right;"> 191.5 </td>
   <td style="text-align:right;"> 45.0 </td>
   <td style="text-align:right;"> 176.9 </td>
   <td style="text-align:right;"> 293.0 </td>
   <td style="text-align:right;"> 73.6 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KHM </td>
   <td style="text-align:right;"> 46.8 </td>
   <td style="text-align:right;"> 73.4 </td>
   <td style="text-align:right;"> 24.8 </td>
   <td style="text-align:right;"> 64.4 </td>
   <td style="text-align:right;"> 92.8 </td>
   <td style="text-align:right;"> 42.5 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KIR </td>
   <td style="text-align:right;"> 269.1 </td>
   <td style="text-align:right;"> 486.8 </td>
   <td style="text-align:right;"> 70.2 </td>
   <td style="text-align:right;"> 266.1 </td>
   <td style="text-align:right;"> 462.4 </td>
   <td style="text-align:right;"> 84.8 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KOR </td>
   <td style="text-align:right;"> 242.3 </td>
   <td style="text-align:right;"> 358.0 </td>
   <td style="text-align:right;"> 144.7 </td>
   <td style="text-align:right;"> 111.2 </td>
   <td style="text-align:right;"> 163.1 </td>
   <td style="text-align:right;"> 67.6 </td>
   <td style="text-align:right;"> -54 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KWT </td>
   <td style="text-align:right;"> 26.0 </td>
   <td style="text-align:right;"> 36.8 </td>
   <td style="text-align:right;"> 11.2 </td>
   <td style="text-align:right;"> 25.8 </td>
   <td style="text-align:right;"> 32.6 </td>
   <td style="text-align:right;"> 14.9 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LAO </td>
   <td style="text-align:right;"> 90.0 </td>
   <td style="text-align:right;"> 136.9 </td>
   <td style="text-align:right;"> 44.5 </td>
   <td style="text-align:right;"> 176.9 </td>
   <td style="text-align:right;"> 261.9 </td>
   <td style="text-align:right;"> 99.3 </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LBN </td>
   <td style="text-align:right;"> 48.2 </td>
   <td style="text-align:right;"> 74.6 </td>
   <td style="text-align:right;"> 22.9 </td>
   <td style="text-align:right;"> 52.6 </td>
   <td style="text-align:right;"> 76.9 </td>
   <td style="text-align:right;"> 30.6 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LBR </td>
   <td style="text-align:right;"> 134.1 </td>
   <td style="text-align:right;"> 153.3 </td>
   <td style="text-align:right;"> 114.5 </td>
   <td style="text-align:right;"> 114.9 </td>
   <td style="text-align:right;"> 140.2 </td>
   <td style="text-align:right;"> 87.5 </td>
   <td style="text-align:right;"> -14 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LBY </td>
   <td style="text-align:right;"> 82.3 </td>
   <td style="text-align:right;"> 122.9 </td>
   <td style="text-align:right;"> 38.8 </td>
   <td style="text-align:right;"> 65.1 </td>
   <td style="text-align:right;"> 88.8 </td>
   <td style="text-align:right;"> 37.4 </td>
   <td style="text-align:right;"> -21 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LCA </td>
   <td style="text-align:right;"> 65.1 </td>
   <td style="text-align:right;"> 118.2 </td>
   <td style="text-align:right;"> 14.3 </td>
   <td style="text-align:right;"> 81.7 </td>
   <td style="text-align:right;"> 142.6 </td>
   <td style="text-align:right;"> 29.1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LKA </td>
   <td style="text-align:right;"> 204.3 </td>
   <td style="text-align:right;"> 352.1 </td>
   <td style="text-align:right;"> 77.8 </td>
   <td style="text-align:right;"> 383.8 </td>
   <td style="text-align:right;"> 619.1 </td>
   <td style="text-align:right;"> 151.0 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LSO </td>
   <td style="text-align:right;"> 339.8 </td>
   <td style="text-align:right;"> 446.2 </td>
   <td style="text-align:right;"> 228.6 </td>
   <td style="text-align:right;"> 231.7 </td>
   <td style="text-align:right;"> 306.2 </td>
   <td style="text-align:right;"> 158.7 </td>
   <td style="text-align:right;"> -32 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LTU </td>
   <td style="text-align:right;"> 302.8 </td>
   <td style="text-align:right;"> 551.4 </td>
   <td style="text-align:right;"> 85.4 </td>
   <td style="text-align:right;"> 350.6 </td>
   <td style="text-align:right;"> 618.2 </td>
   <td style="text-align:right;"> 119.5 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LUX </td>
   <td style="text-align:right;"> 89.7 </td>
   <td style="text-align:right;"> 132.6 </td>
   <td style="text-align:right;"> 50.0 </td>
   <td style="text-align:right;"> 158.8 </td>
   <td style="text-align:right;"> 243.1 </td>
   <td style="text-align:right;"> 82.6 </td>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LVA </td>
   <td style="text-align:right;"> 177.1 </td>
   <td style="text-align:right;"> 329.5 </td>
   <td style="text-align:right;"> 48.2 </td>
   <td style="text-align:right;"> 321.8 </td>
   <td style="text-align:right;"> 563.2 </td>
   <td style="text-align:right;"> 120.9 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MAR </td>
   <td style="text-align:right;"> 76.8 </td>
   <td style="text-align:right;"> 70.7 </td>
   <td style="text-align:right;"> 82.8 </td>
   <td style="text-align:right;"> 81.4 </td>
   <td style="text-align:right;"> 62.3 </td>
   <td style="text-align:right;"> 100.1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MDA </td>
   <td style="text-align:right;"> 148.1 </td>
   <td style="text-align:right;"> 270.5 </td>
   <td style="text-align:right;"> 40.4 </td>
   <td style="text-align:right;"> 180.5 </td>
   <td style="text-align:right;"> 301.2 </td>
   <td style="text-align:right;"> 77.8 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MDG </td>
   <td style="text-align:right;"> 112.5 </td>
   <td style="text-align:right;"> 175.9 </td>
   <td style="text-align:right;"> 53.0 </td>
   <td style="text-align:right;"> 144.7 </td>
   <td style="text-align:right;"> 210.6 </td>
   <td style="text-align:right;"> 74.2 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MDV </td>
   <td style="text-align:right;"> 34.4 </td>
   <td style="text-align:right;"> 49.1 </td>
   <td style="text-align:right;"> 15.3 </td>
   <td style="text-align:right;"> 95.7 </td>
   <td style="text-align:right;"> 105.0 </td>
   <td style="text-align:right;"> 84.7 </td>
   <td style="text-align:right;"> 179 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MEX </td>
   <td style="text-align:right;"> 58.8 </td>
   <td style="text-align:right;"> 100.7 </td>
   <td style="text-align:right;"> 20.4 </td>
   <td style="text-align:right;"> 42.8 </td>
   <td style="text-align:right;"> 76.2 </td>
   <td style="text-align:right;"> 11.8 </td>
   <td style="text-align:right;"> -27 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MHL </td>
   <td style="text-align:right;"> 233.1 </td>
   <td style="text-align:right;"> 335.3 </td>
   <td style="text-align:right;"> 125.7 </td>
   <td style="text-align:right;"> 261.4 </td>
   <td style="text-align:right;"> 390.6 </td>
   <td style="text-align:right;"> 128.9 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MKD </td>
   <td style="text-align:right;"> 66.3 </td>
   <td style="text-align:right;"> 103.7 </td>
   <td style="text-align:right;"> 33.3 </td>
   <td style="text-align:right;"> 84.1 </td>
   <td style="text-align:right;"> 117.3 </td>
   <td style="text-align:right;"> 53.3 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MLI </td>
   <td style="text-align:right;"> 67.5 </td>
   <td style="text-align:right;"> 101.9 </td>
   <td style="text-align:right;"> 32.0 </td>
   <td style="text-align:right;"> 75.4 </td>
   <td style="text-align:right;"> 103.8 </td>
   <td style="text-align:right;"> 47.9 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MLT </td>
   <td style="text-align:right;"> 44.7 </td>
   <td style="text-align:right;"> 76.2 </td>
   <td style="text-align:right;"> 14.6 </td>
   <td style="text-align:right;"> 49.5 </td>
   <td style="text-align:right;"> 81.9 </td>
   <td style="text-align:right;"> 20.3 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MMR </td>
   <td style="text-align:right;"> 53.6 </td>
   <td style="text-align:right;"> 92.0 </td>
   <td style="text-align:right;"> 20.0 </td>
   <td style="text-align:right;"> 76.8 </td>
   <td style="text-align:right;"> 120.7 </td>
   <td style="text-align:right;"> 35.9 </td>
   <td style="text-align:right;"> 43 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MNE </td>
   <td style="text-align:right;"> 98.6 </td>
   <td style="text-align:right;"> 148.0 </td>
   <td style="text-align:right;"> 53.0 </td>
   <td style="text-align:right;"> 106.1 </td>
   <td style="text-align:right;"> 163.6 </td>
   <td style="text-align:right;"> 52.9 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MNG </td>
   <td style="text-align:right;"> 163.0 </td>
   <td style="text-align:right;"> 278.1 </td>
   <td style="text-align:right;"> 54.8 </td>
   <td style="text-align:right;"> 179.0 </td>
   <td style="text-align:right;"> 281.0 </td>
   <td style="text-align:right;"> 80.9 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MOZ </td>
   <td style="text-align:right;"> 176.6 </td>
   <td style="text-align:right;"> 321.0 </td>
   <td style="text-align:right;"> 62.4 </td>
   <td style="text-align:right;"> 159.1 </td>
   <td style="text-align:right;"> 264.8 </td>
   <td style="text-align:right;"> 66.8 </td>
   <td style="text-align:right;"> -10 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MRT </td>
   <td style="text-align:right;"> 66.6 </td>
   <td style="text-align:right;"> 103.1 </td>
   <td style="text-align:right;"> 31.0 </td>
   <td style="text-align:right;"> 77.2 </td>
   <td style="text-align:right;"> 114.4 </td>
   <td style="text-align:right;"> 43.5 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MUS </td>
   <td style="text-align:right;"> 85.6 </td>
   <td style="text-align:right;"> 138.1 </td>
   <td style="text-align:right;"> 35.0 </td>
   <td style="text-align:right;"> 129.9 </td>
   <td style="text-align:right;"> 190.2 </td>
   <td style="text-align:right;"> 73.0 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MWI </td>
   <td style="text-align:right;"> 125.7 </td>
   <td style="text-align:right;"> 232.5 </td>
   <td style="text-align:right;"> 39.6 </td>
   <td style="text-align:right;"> 131.3 </td>
   <td style="text-align:right;"> 196.2 </td>
   <td style="text-align:right;"> 71.1 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MYS </td>
   <td style="text-align:right;"> 74.9 </td>
   <td style="text-align:right;"> 109.4 </td>
   <td style="text-align:right;"> 38.6 </td>
   <td style="text-align:right;"> 93.9 </td>
   <td style="text-align:right;"> 132.3 </td>
   <td style="text-align:right;"> 56.5 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NAM </td>
   <td style="text-align:right;"> 124.6 </td>
   <td style="text-align:right;"> 235.8 </td>
   <td style="text-align:right;"> 30.1 </td>
   <td style="text-align:right;"> 178.8 </td>
   <td style="text-align:right;"> 309.7 </td>
   <td style="text-align:right;"> 61.4 </td>
   <td style="text-align:right;"> 43 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NER </td>
   <td style="text-align:right;"> 75.8 </td>
   <td style="text-align:right;"> 109.9 </td>
   <td style="text-align:right;"> 42.9 </td>
   <td style="text-align:right;"> 79.6 </td>
   <td style="text-align:right;"> 105.8 </td>
   <td style="text-align:right;"> 53.1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NGA </td>
   <td style="text-align:right;"> 75.9 </td>
   <td style="text-align:right;"> 98.0 </td>
   <td style="text-align:right;"> 54.5 </td>
   <td style="text-align:right;"> 86.1 </td>
   <td style="text-align:right;"> 95.4 </td>
   <td style="text-align:right;"> 76.9 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NIC </td>
   <td style="text-align:right;"> 65.2 </td>
   <td style="text-align:right;"> 103.5 </td>
   <td style="text-align:right;"> 30.3 </td>
   <td style="text-align:right;"> 79.2 </td>
   <td style="text-align:right;"> 129.4 </td>
   <td style="text-align:right;"> 33.5 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NLD </td>
   <td style="text-align:right;"> 91.4 </td>
   <td style="text-align:right;"> 129.9 </td>
   <td style="text-align:right;"> 54.5 </td>
   <td style="text-align:right;"> 107.5 </td>
   <td style="text-align:right;"> 145.2 </td>
   <td style="text-align:right;"> 73.3 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NOR </td>
   <td style="text-align:right;"> 99.1 </td>
   <td style="text-align:right;"> 143.2 </td>
   <td style="text-align:right;"> 54.7 </td>
   <td style="text-align:right;"> 142.6 </td>
   <td style="text-align:right;"> 210.6 </td>
   <td style="text-align:right;"> 75.8 </td>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NPL </td>
   <td style="text-align:right;"> 86.0 </td>
   <td style="text-align:right;"> 106.4 </td>
   <td style="text-align:right;"> 68.0 </td>
   <td style="text-align:right;"> 113.3 </td>
   <td style="text-align:right;"> 141.9 </td>
   <td style="text-align:right;"> 85.2 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NZL </td>
   <td style="text-align:right;"> 113.1 </td>
   <td style="text-align:right;"> 174.5 </td>
   <td style="text-align:right;"> 55.7 </td>
   <td style="text-align:right;"> 133.4 </td>
   <td style="text-align:right;"> 213.8 </td>
   <td style="text-align:right;"> 56.6 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> OMN </td>
   <td style="text-align:right;"> 31.5 </td>
   <td style="text-align:right;"> 42.9 </td>
   <td style="text-align:right;"> 12.5 </td>
   <td style="text-align:right;"> 36.9 </td>
   <td style="text-align:right;"> 49.5 </td>
   <td style="text-align:right;"> 17.4 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PAK </td>
   <td style="text-align:right;"> 46.3 </td>
   <td style="text-align:right;"> 45.8 </td>
   <td style="text-align:right;"> 46.8 </td>
   <td style="text-align:right;"> 45.6 </td>
   <td style="text-align:right;"> 42.2 </td>
   <td style="text-align:right;"> 49.4 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PAN </td>
   <td style="text-align:right;"> 53.0 </td>
   <td style="text-align:right;"> 90.4 </td>
   <td style="text-align:right;"> 16.0 </td>
   <td style="text-align:right;"> 58.2 </td>
   <td style="text-align:right;"> 100.3 </td>
   <td style="text-align:right;"> 15.7 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PER </td>
   <td style="text-align:right;"> 34.0 </td>
   <td style="text-align:right;"> 51.4 </td>
   <td style="text-align:right;"> 17.1 </td>
   <td style="text-align:right;"> 44.8 </td>
   <td style="text-align:right;"> 61.9 </td>
   <td style="text-align:right;"> 28.4 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PHL </td>
   <td style="text-align:right;"> 58.3 </td>
   <td style="text-align:right;"> 87.2 </td>
   <td style="text-align:right;"> 29.6 </td>
   <td style="text-align:right;"> 111.1 </td>
   <td style="text-align:right;"> 170.1 </td>
   <td style="text-align:right;"> 52.7 </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PNG </td>
   <td style="text-align:right;"> 238.1 </td>
   <td style="text-align:right;"> 358.5 </td>
   <td style="text-align:right;"> 112.2 </td>
   <td style="text-align:right;"> 284.0 </td>
   <td style="text-align:right;"> 420.9 </td>
   <td style="text-align:right;"> 138.5 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> POL </td>
   <td style="text-align:right;"> 150.8 </td>
   <td style="text-align:right;"> 271.3 </td>
   <td style="text-align:right;"> 37.0 </td>
   <td style="text-align:right;"> 140.3 </td>
   <td style="text-align:right;"> 244.1 </td>
   <td style="text-align:right;"> 44.3 </td>
   <td style="text-align:right;"> -7 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PRI </td>
   <td style="text-align:right;"> 61.5 </td>
   <td style="text-align:right;"> 112.5 </td>
   <td style="text-align:right;"> 16.1 </td>
   <td style="text-align:right;"> 99.3 </td>
   <td style="text-align:right;"> 187.6 </td>
   <td style="text-align:right;"> 21.1 </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PRK </td>
   <td style="text-align:right;"> 113.0 </td>
   <td style="text-align:right;"> 164.4 </td>
   <td style="text-align:right;"> 74.9 </td>
   <td style="text-align:right;"> 125.2 </td>
   <td style="text-align:right;"> 174.7 </td>
   <td style="text-align:right;"> 94.9 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PRT </td>
   <td style="text-align:right;"> 84.3 </td>
   <td style="text-align:right;"> 142.9 </td>
   <td style="text-align:right;"> 34.2 </td>
   <td style="text-align:right;"> 134.4 </td>
   <td style="text-align:right;"> 219.6 </td>
   <td style="text-align:right;"> 63.0 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PRY </td>
   <td style="text-align:right;"> 58.8 </td>
   <td style="text-align:right;"> 87.4 </td>
   <td style="text-align:right;"> 30.7 </td>
   <td style="text-align:right;"> 41.0 </td>
   <td style="text-align:right;"> 59.1 </td>
   <td style="text-align:right;"> 23.6 </td>
   <td style="text-align:right;"> -30 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PSE </td>
   <td style="text-align:right;"> 38.4 </td>
   <td style="text-align:right;"> 59.1 </td>
   <td style="text-align:right;"> 16.9 </td>
   <td style="text-align:right;"> 51.9 </td>
   <td style="text-align:right;"> 80.1 </td>
   <td style="text-align:right;"> 24.6 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> QAT </td>
   <td style="text-align:right;"> 48.3 </td>
   <td style="text-align:right;"> 60.5 </td>
   <td style="text-align:right;"> 11.9 </td>
   <td style="text-align:right;"> 75.5 </td>
   <td style="text-align:right;"> 96.2 </td>
   <td style="text-align:right;"> 30.2 </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ROU </td>
   <td style="text-align:right;"> 96.7 </td>
   <td style="text-align:right;"> 169.3 </td>
   <td style="text-align:right;"> 27.7 </td>
   <td style="text-align:right;"> 100.2 </td>
   <td style="text-align:right;"> 163.3 </td>
   <td style="text-align:right;"> 40.9 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RUS </td>
   <td style="text-align:right;"> 278.1 </td>
   <td style="text-align:right;"> 498.6 </td>
   <td style="text-align:right;"> 87.6 </td>
   <td style="text-align:right;"> 373.2 </td>
   <td style="text-align:right;"> 665.7 </td>
   <td style="text-align:right;"> 123.8 </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RWA </td>
   <td style="text-align:right;"> 114.6 </td>
   <td style="text-align:right;"> 199.8 </td>
   <td style="text-align:right;"> 53.8 </td>
   <td style="text-align:right;"> 229.6 </td>
   <td style="text-align:right;"> 313.0 </td>
   <td style="text-align:right;"> 157.3 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SAU </td>
   <td style="text-align:right;"> 29.8 </td>
   <td style="text-align:right;"> 44.6 </td>
   <td style="text-align:right;"> 8.8 </td>
   <td style="text-align:right;"> 24.2 </td>
   <td style="text-align:right;"> 34.6 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> -19 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SDN </td>
   <td style="text-align:right;"> 51.7 </td>
   <td style="text-align:right;"> 75.8 </td>
   <td style="text-align:right;"> 27.2 </td>
   <td style="text-align:right;"> 62.0 </td>
   <td style="text-align:right;"> 83.1 </td>
   <td style="text-align:right;"> 40.7 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SEN </td>
   <td style="text-align:right;"> 128.4 </td>
   <td style="text-align:right;"> 211.2 </td>
   <td style="text-align:right;"> 51.3 </td>
   <td style="text-align:right;"> 130.5 </td>
   <td style="text-align:right;"> 206.8 </td>
   <td style="text-align:right;"> 56.3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SGP </td>
   <td style="text-align:right;"> 80.0 </td>
   <td style="text-align:right;"> 110.6 </td>
   <td style="text-align:right;"> 52.1 </td>
   <td style="text-align:right;"> 140.5 </td>
   <td style="text-align:right;"> 183.1 </td>
   <td style="text-align:right;"> 100.2 </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SLB </td>
   <td style="text-align:right;"> 168.2 </td>
   <td style="text-align:right;"> 243.2 </td>
   <td style="text-align:right;"> 91.9 </td>
   <td style="text-align:right;"> 217.8 </td>
   <td style="text-align:right;"> 308.2 </td>
   <td style="text-align:right;"> 120.3 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SLE </td>
   <td style="text-align:right;"> 101.0 </td>
   <td style="text-align:right;"> 134.8 </td>
   <td style="text-align:right;"> 68.2 </td>
   <td style="text-align:right;"> 79.1 </td>
   <td style="text-align:right;"> 111.6 </td>
   <td style="text-align:right;"> 47.4 </td>
   <td style="text-align:right;"> -22 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SLV </td>
   <td style="text-align:right;"> 95.2 </td>
   <td style="text-align:right;"> 169.0 </td>
   <td style="text-align:right;"> 36.5 </td>
   <td style="text-align:right;"> 120.0 </td>
   <td style="text-align:right;"> 182.1 </td>
   <td style="text-align:right;"> 66.0 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SOM </td>
   <td style="text-align:right;"> 101.8 </td>
   <td style="text-align:right;"> 152.1 </td>
   <td style="text-align:right;"> 54.9 </td>
   <td style="text-align:right;"> 115.5 </td>
   <td style="text-align:right;"> 159.6 </td>
   <td style="text-align:right;"> 70.9 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SRB </td>
   <td style="text-align:right;"> 136.2 </td>
   <td style="text-align:right;"> 218.4 </td>
   <td style="text-align:right;"> 62.8 </td>
   <td style="text-align:right;"> 191.9 </td>
   <td style="text-align:right;"> 295.3 </td>
   <td style="text-align:right;"> 100.9 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SSD </td>
   <td style="text-align:right;"> 121.3 </td>
   <td style="text-align:right;"> 187.4 </td>
   <td style="text-align:right;"> 45.8 </td>
   <td style="text-align:right;"> 133.4 </td>
   <td style="text-align:right;"> 195.6 </td>
   <td style="text-align:right;"> 58.5 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> STP </td>
   <td style="text-align:right;"> 35.2 </td>
   <td style="text-align:right;"> 51.2 </td>
   <td style="text-align:right;"> 20.1 </td>
   <td style="text-align:right;"> 27.8 </td>
   <td style="text-align:right;"> 38.8 </td>
   <td style="text-align:right;"> 18.0 </td>
   <td style="text-align:right;"> -21 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SUR </td>
   <td style="text-align:right;"> 253.3 </td>
   <td style="text-align:right;"> 406.3 </td>
   <td style="text-align:right;"> 109.4 </td>
   <td style="text-align:right;"> 251.1 </td>
   <td style="text-align:right;"> 346.7 </td>
   <td style="text-align:right;"> 159.8 </td>
   <td style="text-align:right;"> -1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SVK </td>
   <td style="text-align:right;"> 103.8 </td>
   <td style="text-align:right;"> 184.9 </td>
   <td style="text-align:right;"> 29.9 </td>
   <td style="text-align:right;"> 146.1 </td>
   <td style="text-align:right;"> 254.7 </td>
   <td style="text-align:right;"> 48.5 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SVN </td>
   <td style="text-align:right;"> 149.8 </td>
   <td style="text-align:right;"> 255.8 </td>
   <td style="text-align:right;"> 52.1 </td>
   <td style="text-align:right;"> 289.0 </td>
   <td style="text-align:right;"> 474.3 </td>
   <td style="text-align:right;"> 126.9 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SWE </td>
   <td style="text-align:right;"> 115.6 </td>
   <td style="text-align:right;"> 164.4 </td>
   <td style="text-align:right;"> 67.2 </td>
   <td style="text-align:right;"> 170.9 </td>
   <td style="text-align:right;"> 243.1 </td>
   <td style="text-align:right;"> 101.7 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SWZ </td>
   <td style="text-align:right;"> 261.0 </td>
   <td style="text-align:right;"> 466.9 </td>
   <td style="text-align:right;"> 85.7 </td>
   <td style="text-align:right;"> 180.7 </td>
   <td style="text-align:right;"> 319.2 </td>
   <td style="text-align:right;"> 68.8 </td>
   <td style="text-align:right;"> -31 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SYC </td>
   <td style="text-align:right;"> 77.2 </td>
   <td style="text-align:right;"> 131.0 </td>
   <td style="text-align:right;"> 23.5 </td>
   <td style="text-align:right;"> 142.1 </td>
   <td style="text-align:right;"> 256.9 </td>
   <td style="text-align:right;"> 37.1 </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SYR </td>
   <td style="text-align:right;"> 30.3 </td>
   <td style="text-align:right;"> 48.4 </td>
   <td style="text-align:right;"> 11.5 </td>
   <td style="text-align:right;"> 37.1 </td>
   <td style="text-align:right;"> 56.9 </td>
   <td style="text-align:right;"> 16.2 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TCD </td>
   <td style="text-align:right;"> 113.5 </td>
   <td style="text-align:right;"> 148.6 </td>
   <td style="text-align:right;"> 78.0 </td>
   <td style="text-align:right;"> 87.0 </td>
   <td style="text-align:right;"> 113.5 </td>
   <td style="text-align:right;"> 63.4 </td>
   <td style="text-align:right;"> -23 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TGO </td>
   <td style="text-align:right;"> 126.4 </td>
   <td style="text-align:right;"> 212.8 </td>
   <td style="text-align:right;"> 58.4 </td>
   <td style="text-align:right;"> 113.7 </td>
   <td style="text-align:right;"> 165.2 </td>
   <td style="text-align:right;"> 68.4 </td>
   <td style="text-align:right;"> -10 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> THA </td>
   <td style="text-align:right;"> 102.4 </td>
   <td style="text-align:right;"> 164.7 </td>
   <td style="text-align:right;"> 43.0 </td>
   <td style="text-align:right;"> 143.4 </td>
   <td style="text-align:right;"> 210.9 </td>
   <td style="text-align:right;"> 79.4 </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TJK </td>
   <td style="text-align:right;"> 55.5 </td>
   <td style="text-align:right;"> 81.7 </td>
   <td style="text-align:right;"> 29.9 </td>
   <td style="text-align:right;"> 59.0 </td>
   <td style="text-align:right;"> 85.6 </td>
   <td style="text-align:right;"> 32.9 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TKM </td>
   <td style="text-align:right;"> 88.2 </td>
   <td style="text-align:right;"> 136.1 </td>
   <td style="text-align:right;"> 38.8 </td>
   <td style="text-align:right;"> 89.7 </td>
   <td style="text-align:right;"> 137.3 </td>
   <td style="text-align:right;"> 45.1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TLS </td>
   <td style="text-align:right;"> 72.7 </td>
   <td style="text-align:right;"> 103.6 </td>
   <td style="text-align:right;"> 41.3 </td>
   <td style="text-align:right;"> 121.7 </td>
   <td style="text-align:right;"> 153.2 </td>
   <td style="text-align:right;"> 89.2 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TON </td>
   <td style="text-align:right;"> 60.3 </td>
   <td style="text-align:right;"> 80.3 </td>
   <td style="text-align:right;"> 39.9 </td>
   <td style="text-align:right;"> 56.2 </td>
   <td style="text-align:right;"> 65.0 </td>
   <td style="text-align:right;"> 47.6 </td>
   <td style="text-align:right;"> -7 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TTO </td>
   <td style="text-align:right;"> 101.7 </td>
   <td style="text-align:right;"> 167.3 </td>
   <td style="text-align:right;"> 36.1 </td>
   <td style="text-align:right;"> 145.5 </td>
   <td style="text-align:right;"> 233.8 </td>
   <td style="text-align:right;"> 58.9 </td>
   <td style="text-align:right;"> 43 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TUN </td>
   <td style="text-align:right;"> 32.2 </td>
   <td style="text-align:right;"> 48.5 </td>
   <td style="text-align:right;"> 16.4 </td>
   <td style="text-align:right;"> 33.1 </td>
   <td style="text-align:right;"> 45.5 </td>
   <td style="text-align:right;"> 20.5 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TUR </td>
   <td style="text-align:right;"> 32.5 </td>
   <td style="text-align:right;"> 51.2 </td>
   <td style="text-align:right;"> 14.5 </td>
   <td style="text-align:right;"> 42.1 </td>
   <td style="text-align:right;"> 64.0 </td>
   <td style="text-align:right;"> 20.3 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TWN </td>
   <td style="text-align:right;"> 140.2 </td>
   <td style="text-align:right;"> 191.1 </td>
   <td style="text-align:right;"> 91.0 </td>
   <td style="text-align:right;"> 96.9 </td>
   <td style="text-align:right;"> 119.8 </td>
   <td style="text-align:right;"> 72.5 </td>
   <td style="text-align:right;"> -31 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TZA </td>
   <td style="text-align:right;"> 87.0 </td>
   <td style="text-align:right;"> 141.0 </td>
   <td style="text-align:right;"> 38.9 </td>
   <td style="text-align:right;"> 105.1 </td>
   <td style="text-align:right;"> 154.9 </td>
   <td style="text-align:right;"> 57.9 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> UGA </td>
   <td style="text-align:right;"> 123.7 </td>
   <td style="text-align:right;"> 162.2 </td>
   <td style="text-align:right;"> 91.8 </td>
   <td style="text-align:right;"> 142.3 </td>
   <td style="text-align:right;"> 125.3 </td>
   <td style="text-align:right;"> 159.1 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> UKR </td>
   <td style="text-align:right;"> 224.6 </td>
   <td style="text-align:right;"> 413.2 </td>
   <td style="text-align:right;"> 62.8 </td>
   <td style="text-align:right;"> 231.8 </td>
   <td style="text-align:right;"> 416.3 </td>
   <td style="text-align:right;"> 81.1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> URY </td>
   <td style="text-align:right;"> 169.0 </td>
   <td style="text-align:right;"> 284.8 </td>
   <td style="text-align:right;"> 68.2 </td>
   <td style="text-align:right;"> 117.4 </td>
   <td style="text-align:right;"> 197.9 </td>
   <td style="text-align:right;"> 48.0 </td>
   <td style="text-align:right;"> -31 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> USA </td>
   <td style="text-align:right;"> 122.6 </td>
   <td style="text-align:right;"> 192.8 </td>
   <td style="text-align:right;"> 56.0 </td>
   <td style="text-align:right;"> 120.4 </td>
   <td style="text-align:right;"> 200.6 </td>
   <td style="text-align:right;"> 47.8 </td>
   <td style="text-align:right;"> -2 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> UZB </td>
   <td style="text-align:right;"> 94.9 </td>
   <td style="text-align:right;"> 143.2 </td>
   <td style="text-align:right;"> 50.8 </td>
   <td style="text-align:right;"> 91.1 </td>
   <td style="text-align:right;"> 140.8 </td>
   <td style="text-align:right;"> 44.1 </td>
   <td style="text-align:right;"> -4 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VCT </td>
   <td style="text-align:right;"> 69.5 </td>
   <td style="text-align:right;"> 121.8 </td>
   <td style="text-align:right;"> 15.8 </td>
   <td style="text-align:right;"> 67.0 </td>
   <td style="text-align:right;"> 115.6 </td>
   <td style="text-align:right;"> 22.2 </td>
   <td style="text-align:right;"> -4 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VEN </td>
   <td style="text-align:right;"> 84.5 </td>
   <td style="text-align:right;"> 150.7 </td>
   <td style="text-align:right;"> 20.8 </td>
   <td style="text-align:right;"> 82.1 </td>
   <td style="text-align:right;"> 144.8 </td>
   <td style="text-align:right;"> 23.3 </td>
   <td style="text-align:right;"> -3 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VNM </td>
   <td style="text-align:right;"> 75.3 </td>
   <td style="text-align:right;"> 107.4 </td>
   <td style="text-align:right;"> 46.6 </td>
   <td style="text-align:right;"> 94.0 </td>
   <td style="text-align:right;"> 125.5 </td>
   <td style="text-align:right;"> 67.6 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VUT </td>
   <td style="text-align:right;"> 178.8 </td>
   <td style="text-align:right;"> 280.5 </td>
   <td style="text-align:right;"> 77.0 </td>
   <td style="text-align:right;"> 215.6 </td>
   <td style="text-align:right;"> 330.4 </td>
   <td style="text-align:right;"> 97.3 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> WSM </td>
   <td style="text-align:right;"> 100.6 </td>
   <td style="text-align:right;"> 148.7 </td>
   <td style="text-align:right;"> 51.6 </td>
   <td style="text-align:right;"> 149.9 </td>
   <td style="text-align:right;"> 225.3 </td>
   <td style="text-align:right;"> 69.4 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YEM </td>
   <td style="text-align:right;"> 60.1 </td>
   <td style="text-align:right;"> 82.0 </td>
   <td style="text-align:right;"> 38.4 </td>
   <td style="text-align:right;"> 67.5 </td>
   <td style="text-align:right;"> 85.5 </td>
   <td style="text-align:right;"> 49.7 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ZAF </td>
   <td style="text-align:right;"> 117.7 </td>
   <td style="text-align:right;"> 192.6 </td>
   <td style="text-align:right;"> 52.6 </td>
   <td style="text-align:right;"> 186.5 </td>
   <td style="text-align:right;"> 285.7 </td>
   <td style="text-align:right;"> 98.1 </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ZMB </td>
   <td style="text-align:right;"> 131.8 </td>
   <td style="text-align:right;"> 217.5 </td>
   <td style="text-align:right;"> 52.6 </td>
   <td style="text-align:right;"> 175.2 </td>
   <td style="text-align:right;"> 240.0 </td>
   <td style="text-align:right;"> 103.4 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ZWE </td>
   <td style="text-align:right;"> 286.0 </td>
   <td style="text-align:right;"> 443.1 </td>
   <td style="text-align:right;"> 157.3 </td>
   <td style="text-align:right;"> 177.1 </td>
   <td style="text-align:right;"> 273.6 </td>
   <td style="text-align:right;"> 94.7 </td>
   <td style="text-align:right;"> -38 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

### World Maps

#### Data Prep


```r
# Trim into 7 equal sized groups

df_desc <- df_desc %>%
  mutate(suicide_pcm_owd_c = Hmisc::cut2(suicide_pcm_owd, g=7),
         suicide_m_pcm_owd_c = Hmisc::cut2(suicide_m_pcm_owd , g=7),
         suicide_f_pcm_owd_c = Hmisc::cut2(suicide_f_pcm_owd , g=7),
         change_total_c = Hmisc::cut2(change_total, g=7)) %>%
  group_by(suicide_pcm_owd_c) %>% #for labelling
  mutate(total_min = min(suicide_pcm_owd),
         total_max = max(suicide_pcm_owd)) %>%
  ungroup()

# males label
df_desc <- df_desc %>%
  group_by(suicide_m_pcm_owd_c) %>%
  mutate(male_min = min(suicide_m_pcm_owd),
         male_max = max(suicide_m_pcm_owd)) %>%
  ungroup()

# females label
df_desc <- df_desc %>%
  group_by(suicide_f_pcm_owd_c) %>%
  mutate(female_min = min(suicide_f_pcm_owd),
         female_max = max(suicide_f_pcm_owd)) %>%
  ungroup()

# change label
df_desc <- df_desc %>%
  group_by(change_total_c) %>%
  mutate(change_min = min(change_total),
         change_max = max(change_total)) %>%
  ungroup()

# label file
df_label <- matrix(nrow = 8, ncol = 7)
df_label[1,] <- sort(round(unique(df_desc$total_min), 0))
df_label[2,] <- sort(round(unique(df_desc$total_max), 0))
df_label[3,] <- sort(round(unique(df_desc$male_min), 0))
df_label[4,] <- sort(round(unique(df_desc$male_max), 0))
df_label[5,] <- sort(round(unique(df_desc$female_min), 0))
df_label[6,] <- sort(round(unique(df_desc$female_max), 0))
df_label[7,] <- sort(round(unique(df_desc$change_min), 0))
df_label[8,] <- sort(round(unique(df_desc$change_max), 0)) 

total_label <- c(paste0("<",df_label[2,1]), paste0(df_label[1,2],"-",df_label[2,2]), paste0(df_label[1,3],"-",df_label[2,3]), paste0(df_label[1,4],"-",df_label[2,4]), paste0(df_label[1,5],"-",df_label[2,5]), paste0(df_label[1,6],"-",df_label[2,6]),
                 paste0(">",df_label[1,7]))

male_label <- c(paste0("<",df_label[4,1]), paste0(df_label[3,2],"-",df_label[4,2]), paste0(df_label[3,3],"-",df_label[4,3]), paste0(df_label[3,4],"-",df_label[4,4]), paste0(df_label[3,5],"-",df_label[4,5]), paste0(df_label[3,6],"-",df_label[4,6]),
                 paste0(">",df_label[3,7]))

female_label <- c(paste0("<",df_label[6,1]), paste0(df_label[5,2],"-",df_label[6,2]), paste0(df_label[5,3],"-",df_label[6,3]), paste0(df_label[5,4],"-",df_label[6,4]), paste0(df_label[5,5],"-",df_label[6,5]), paste0(df_label[5,6],"-",df_label[6,6]),
                 paste0(">",df_label[5,7]))

change_label <- c(paste0("<",df_label[8,1],"%"), paste0(df_label[7,2],"- ",df_label[8,2]), paste0(df_label[7,3],"-",df_label[8,3]), paste0(df_label[7,4],"-",df_label[8,4]), paste0(df_label[7,5],"-",df_label[8,5]), paste0(df_label[7,6],"-",df_label[8,6]),
                 paste0(">",(df_label[8,6])+1,"%"))
                 
df_desc <- df_desc %>%
  mutate(suicide_total_factor = factor(suicide_pcm_owd_c, labels = total_label),
         suicide_male_factor = factor(suicide_m_pcm_owd_c, labels = male_label),
         suicide_female_factor = factor(suicide_f_pcm_owd_c, labels = female_label),
         suicide_change_factor = factor(change_total_c, label = change_label))

# setup map
map_desc <- joinCountryData2Map(df_desc,
                                joinCode = "ISO3",
                                nameJoinColumn = "iso3c")
```

```
## 192 codes from your data successfully matched countries in the map
## 0 codes from your data failed to match with a country code in the map
## 51 codes from the map weren't represented in your data
```

```r
map_desc <- subset(map_desc, continent != "Antarctica")
```

#### Total Suicides


```r
agg_png(filename = here::here("results/Fig1_total_map.png"), res = 144, width = 3000, height = 2100)
map01 <- mapCountryData(map_desc, 
                        nameColumnToPlot = "suicide_total_factor", 
                        addLegend = T,
                        mapTitle = "Suicides Per Million Capita",
                        missingCountryCol = "ivory",
                        #colourPalette = c('olivedrab4','olivedrab3','olivedrab1', 'goldenrod1','goldenrod3','brown2','brown4'),
                        colourPalette = viridisLite::mako(n = 7, direction = -1),
                        catMethod = 'categorical')
dev.off()
```

```
## png 
##   2
```

```r
knitr::include_graphics(here::here("results/Fig1_total_map.png"))
```

<img src="C:/Users/chw/OneDrive/Uni Bremen/Semester 7/open_science/suicide_replication/results/Fig1_total_map.png" width="3000" />


#### Males


```r
agg_png(filename = here::here("results/Fig1_males_map.png"), res = 144, width = 3000, height = 2100)
map01 <- mapCountryData(map_desc, 
                        nameColumnToPlot = "suicide_male_factor", 
                        addLegend = T,
                        mapTitle = "Male Suicides Per Million Male Capita",
                        missingCountryCol = "ivory",
                        #colourPalette = c('olivedrab4','olivedrab3','olivedrab1', 'goldenrod1','goldenrod3','brown2','brown4'),
                        colourPalette = viridisLite::mako(n = 7, direction = -1),
                        catMethod = 'fixedWidth')
```

```
## using catMethod='categorical' for non numeric data in mapCountryData
```

```r
dev.off()
```

```
## png 
##   2
```

```r
knitr::include_graphics(here::here("results/Fig1_males_map.png"))
```

<img src="C:/Users/chw/OneDrive/Uni Bremen/Semester 7/open_science/suicide_replication/results/Fig1_males_map.png" width="3000" />


#### Females


```r
agg_png(filename = here::here("results/Fig1_females_map.png"), res = 144, width = 3000, height = 2100)
map01 <- mapCountryData(map_desc, 
                        nameColumnToPlot = "suicide_female_factor", 
                        addLegend = T,
                        mapTitle = "Female Suicides Per Million Female Capita",
                        missingCountryCol = "ivory",
                        #colourPalette = c('olivedrab4','olivedrab3','olivedrab1', 'goldenrod1','goldenrod3','brown2','brown4'),
                        colourPalette = viridisLite::mako(n = 7, direction = -1),
                        catMethod = 'fixedWidth')
```

```
## using catMethod='categorical' for non numeric data in mapCountryData
```

```r
dev.off()
```

```
## png 
##   2
```

```r
knitr::include_graphics(here::here("results/Fig1_females_map.png"))
```

<img src="C:/Users/chw/OneDrive/Uni Bremen/Semester 7/open_science/suicide_replication/results/Fig1_females_map.png" width="3000" />

### Change 1990-2018


```r
agg_png(filename = here::here("results/Fig2_change.png"), res = 144, width = 3000, height = 2100)
map01 <- mapCountryData(map_desc, 
                        nameColumnToPlot = "suicide_change_factor", 
                        addLegend = T,
                        mapTitle = "Percent Change in Suicide Rate from 1990-2018",
                        missingCountryCol = "ivory",
                        #colourPalette = c('olivedrab4','olivedrab3','olivedrab1', 'goldenrod1','goldenrod3','brown2','brown4'),
                        colourPalette = viridisLite::mako(n = 7, direction = -1),
                        catMethod = 'fixedWidth')
```

```
## using catMethod='categorical' for non numeric data in mapCountryData
```

```r
dev.off()
```

```
## png 
##   2
```

```r
knitr::include_graphics(here::here("results/Fig2_change.png"))
```

<img src="C:/Users/chw/OneDrive/Uni Bremen/Semester 7/open_science/suicide_replication/results/Fig2_change.png" width="3000" />


### Descriptive Table




```r
# more elegant solution

df_raw %>%
  ungroup() %>% 
  summarise_at(vars(suicide_rate_total_owd, suicide_rate_male_owd, suicide_rate_female_owd, unemp_i, alc_all_types_i, fertility_i,
                    pop65,crisis_idx_i, atwar_i, gdp_cap_i, regime_i, gini_disp_i, fem_labour_i, christpct_ii, islampct_ii, buddhapct_ii,
                    hindupct_ii, nonreligpct_ii, jewishpct_ii, otherpct_ii), 
               funs(mean(., na.rm = T), sd(., na.rm = T), min(., na.rm = T), max(., na.rm = T), N = sum(!is.na(.), na.rm = T))) %>% 
  # So far same as above
  # Create stats columns automatically with ".value" and regex pattern
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), 
               # First pattern = Variable name, second = statistic
               names_pattern = "(.+)_(mean|sd|min|max|N)") %>% 
  # Capitalize first letter
  rename_with(~str_to_title(.)) %>% 
  kable(digits = 2) %>% 
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Sd </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Max </th>
   <th style="text-align:right;"> N </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> suicide_rate_total_owd </td>
   <td style="text-align:right;"> 11.95 </td>
   <td style="text-align:right;"> 8.42 </td>
   <td style="text-align:right;"> 1.53 </td>
   <td style="text-align:right;"> 98.83 </td>
   <td style="text-align:right;"> 5376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> suicide_rate_male_owd </td>
   <td style="text-align:right;"> 186.29 </td>
   <td style="text-align:right;"> 136.86 </td>
   <td style="text-align:right;"> 25.20 </td>
   <td style="text-align:right;"> 1454.60 </td>
   <td style="text-align:right;"> 5376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> suicide_rate_female_owd </td>
   <td style="text-align:right;"> 57.70 </td>
   <td style="text-align:right;"> 43.49 </td>
   <td style="text-align:right;"> 6.10 </td>
   <td style="text-align:right;"> 487.10 </td>
   <td style="text-align:right;"> 5376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> unemp_i </td>
   <td style="text-align:right;"> 8.61 </td>
   <td style="text-align:right;"> 5.98 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 38.80 </td>
   <td style="text-align:right;"> 3014 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> alc_all_types_i </td>
   <td style="text-align:right;"> 4.88 </td>
   <td style="text-align:right;"> 4.07 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 20.24 </td>
   <td style="text-align:right;"> 5061 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fertility_i </td>
   <td style="text-align:right;"> 3.25 </td>
   <td style="text-align:right;"> 1.69 </td>
   <td style="text-align:right;"> 1.05 </td>
   <td style="text-align:right;"> 8.61 </td>
   <td style="text-align:right;"> 5228 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pop65 </td>
   <td style="text-align:right;"> 7.11 </td>
   <td style="text-align:right;"> 4.99 </td>
   <td style="text-align:right;"> 0.69 </td>
   <td style="text-align:right;"> 27.11 </td>
   <td style="text-align:right;"> 5174 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> crisis_idx_i </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.45 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 5152 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> atwar_i </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 5376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gdp_cap_i </td>
   <td style="text-align:right;"> 13.16 </td>
   <td style="text-align:right;"> 18.35 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 234.00 </td>
   <td style="text-align:right;"> 5301 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> regime_i </td>
   <td style="text-align:right;"> 4.60 </td>
   <td style="text-align:right;"> 3.02 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 9.00 </td>
   <td style="text-align:right;"> 4592 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gini_disp_i </td>
   <td style="text-align:right;"> 39.12 </td>
   <td style="text-align:right;"> 8.69 </td>
   <td style="text-align:right;"> 17.50 </td>
   <td style="text-align:right;"> 67.20 </td>
   <td style="text-align:right;"> 4139 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fem_labour_i </td>
   <td style="text-align:right;"> 40.08 </td>
   <td style="text-align:right;"> 9.70 </td>
   <td style="text-align:right;"> 7.91 </td>
   <td style="text-align:right;"> 56.01 </td>
   <td style="text-align:right;"> 5068 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> christpct_ii </td>
   <td style="text-align:right;"> 0.52 </td>
   <td style="text-align:right;"> 0.36 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.99 </td>
   <td style="text-align:right;"> 5376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> islampct_ii </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:right;"> 0.36 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 5376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> buddhapct_ii </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:right;"> 5376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hindupct_ii </td>
   <td style="text-align:right;"> 0.02 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.87 </td>
   <td style="text-align:right;"> 5376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonreligpct_ii </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.11 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.80 </td>
   <td style="text-align:right;"> 5376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> jewishpct_ii </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.57 </td>
   <td style="text-align:right;"> 5376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> otherpct_ii </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:right;"> 5376 </td>
  </tr>
</tbody>
</table>

