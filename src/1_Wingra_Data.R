library(tidyverse)
library(lubridate)
library(deSolve)
library(MetBrewer)
library(patchwork)
library(zoo)
library(metR)

################# Data loading and cleaning ################ 
### Load climate data set from NTL-LTER core data
# Convert to annual water-year totals

# Load most recent 2024 data from Dane County airport
met2024 = read_csv('InputData/3857041.csv') |> 
  mutate(precip_raw_mm = PRCP, snow_raw_cm = SNOW/10) |> 
  rename(sampledate = DATE) |> 
  mutate(year4 = year(sampledate)) |> 
  select(year4, sampledate, precip_raw_mm, snow_raw_cm)

# Load core dataset (currently ends Dec 2023)
yearMet = read_csv('InputData/ntl20_v13.csv') |> 
  bind_rows(met2024) |> # bind 2024 data
  rename(year = year4) |> 
  mutate(year = if_else(month(sampledate) >= 7, year + 1, year)) |> 
  group_by(year) |> 
  summarise(precip_raw_m = sum(precip_raw_mm, na.rm = T)/1000, snow_raw_m = sum(snow_raw_cm, na.rm = T)/100) |> 
  filter(year >= 1962, year <= 2024)

# ET monthly from open ET monthly ensemble
openET = read_csv('InputData/WingraOpenETensemble.csv')

# Mean ET 0.627
openETyear = openET |> 
  mutate(year4 = year(Date)) |> 
  group_by(year4) |> 
  summarise(et = sum(et_ensemble_mad/1000)) |> 
  ungroup() |> summarise(et = mean(et))

# Subtracting ET from Precipitation to find runoff (assuming ET = 0.75 each year)
# If runoff was 0 for a year, assume that it is 0.05
ET_Precip <- yearMet %>% 
  # mutate(runoff = (precip_raw_m - 0.75)) %>% 
  mutate(runoff = precip_raw_m) %>%
  mutate(runoff = if_else(runoff < 0, 0.05, runoff))

### Load chloride data set from Public Health Madison Dane County
YaharaLakes = read_csv('InputData/MadisonLakes_chloride[38].csv')
allLakes = read_csv('InputData/MadisonLakes_chloride[38].csv')|> 
  filter(Lake == 'Wingra')
allLakes$Date <- as.Date(allLakes$Date, format = "%m/%d/%Y")

# Calculate annual Cl values by averaging out June-September
annualCl = allLakes %>% 
  filter(month(Date) %in% c(6:9)) |> 
  group_by(year = year(Date)) %>% 
  summarise(Chloride.mgL = mean(Chloride.mgL)) %>%
  filter(year >= 1962)

### Load Road Salt data set from City of Madison Streets Division
# Convert road salt tons to kg (907.185 kg/ 1 ton)
# Divide all values by 1252066.52 meters (city of Madison salt route mileage) 
# 1 kg of NaCl, considering the molar mass ratio, approximately 390g of sodium and 610g of chloride
roadSalt = read_csv('InputData/CityMadison_roadsalt_2024update.csv') |> 
  select(YearEnd, TotalSalt_tons) |>
  rename(salt_tons = TotalSalt_tons, year = YearEnd) |>
  mutate(salt_kg = salt_tons * 907.185) |>
  mutate(salt_kg = salt_kg * 0.610) |> # convert to chloride
  mutate(salt_kg_m = salt_kg / 1252066.52) |>
  mutate(salt_kg_m_2 = salt_kg_m*1.5) |> 
  filter(year >= 1962)

