##################################################################
# 
##################################################################

# Load libraries
library(tidyverse)
library(lubridate)
library(deSolve)
library(MetBrewer)
library(patchwork)
library(zoo)
library(metR)

# Load chloride data, remove outliers
source('src/chlorideProcess.R')
# Load GIS datasets to calculate areas, road network 
source('src/GISprocess.R')

################# Data loading and cleaning ################ 
### Load climate data
rawmet = read_csv('InputData/Climate/3944435.csv') |> 
  mutate(PRCP = if_else(PRCP > 1000, NA, PRCP))

ggplot(rawmet) +
  geom_path(aes(x = DATE, y = SNOW, col = STATION))


# Data from arboretum and charmany farms
arbMet = read_csv('InputData/Climate/3944435.csv') |> 
  group_by(DATE) |> 
  summarise(PRCP = mean(PRCP, na.rm = T), SNOW = mean(SNOW, na.rm = T), 
            TMAX = mean(TMAX, na.rm = T), TMIN = mean(TMIN, na.rm = T)) |> #Take mean of two stations 
  mutate(year = year(DATE)) |> mutate(month = month(DATE)) |> 
  # mutate(year = if_else(month(sampledate) >= 10, year + 1, year)) |> 
  group_by(year, month) |> 
  summarise(sampledate = first(DATE), precip_raw_m = sum(PRCP, na.rm = T)/1000, 
            snow_raw_m = sum(SNOW, na.rm = T)/1000, 
            days_above_5 = sum(TMAX > 5, na.rm = T)) |> 
  filter(year >= 1962, year <= 2024)

# ET monthly from open ET monthly ensemble
openET.wingra = read_csv('InputData/WingraOpenETensemble.csv')
# ET monthly from open ET monthly ensemble for Mendota
openET.mendota = read_csv('InputData/mendotaOpenETensemble.csv')
ggplot(openET.mendota) +
  geom_path(aes(x = Date, y = et_ensemble_mad)) +
  geom_path(data = openET.wingra, aes(x = Date, y = et_ensemble_mad), color = 'lightblue')

# Mean ET 0.627 for Wingra
openETmonth = openET.wingra |> 
  mutate(year = year(Date), month = month(Date)) |> 
  group_by(month) |> 
  summarise(et = mean(et_ensemble_mad/1000)) |> 
  ungroup() 

# Subtracting ET from Precipitation to find runoff (assuming ET = 0.75 each year)
# If runoff was 0 for a year, assume that it is 0.05
ET_Precip <- arbMet %>% 
  ungroup() |> 
  left_join(openETmonth) |> 
  mutate(frozen = if_else(days_above_5 < 5, TRUE, FALSE)) |> 
  mutate(group = cumsum(lag(frozen, default = FALSE) == FALSE)) |> 
  group_by(group) |> 
  mutate(usePrecip_m = sum(precip_raw_m)) |> 
  mutate(usePrecip_m = if_else(frozen == TRUE, 0, usePrecip_m)) |> 
  # mutate(runoff = (precip_raw_m - 0.75)) %>% 
  mutate(runoff = usePrecip_m - et) %>%
  mutate(runoff = if_else(runoff < 0, 0, runoff))

ggplot(ET_Precip) + 
  geom_path(aes(x = sampledate, y = runoff))

### Load chloride data set from Public Health Madison Dane County
cl.outliers

# Calculate annual Cl values by averaging out June-September
monthlyCl = cl.outliers %>% 
  filter(Lake %in% c('Wingra')) |> 
  mutate(year = year(Date), month = month(Date)) |>
  group_by(Lake, year, month) %>% 
  summarise(Date = first(Date), Chloride.mgL = mean(cl.clean, na.rm = T)) %>%
  mutate(sampledate = ym(paste(year, month))) 

### Load Road Salt data set from City of Madison Streets Division
# Convert road salt tons to kg (907.185 kg/ 1 ton)
# Divide all values by 1252066.52 meters (city of Madison salt route mileage) 
# 1 kg of NaCl, considering the molar mass ratio, approximately 390g of sodium and 610g of chloride
roadSalt = read_csv('InputData/CityMadison_roadsalt_2024update.csv') 
ggplot(roadSalt)+
  geom_point(aes(x = TotalSalt_tons, y = DaneCounty))

#### For Wingra
roadSalt = read_csv('InputData/CityMadison_roadsalt_2024update.csv') |> 
  select(YearStart, wateryear = YearEnd, TotalSalt_tons, DaneCounty, ExtraSalt_Private, ExtraSalt_Private_steady) |>
  mutate(wingra_tons = (TotalSalt_tons * 0.0866) + (ExtraSalt_Private_steady* 0.0866)) |> 
  mutate(salt_kg = wingra_tons * 907.185) |>
  mutate(cl_kg = salt_kg * 0.610)

saltuse = arbMet |> 
  left_join(
    arbMet |> 
      mutate(wateryear = if_else(month(sampledate) >= 10, year + 1, year)) |> 
      group_by(wateryear) |> 
      mutate(totalSnow = sum(snow_raw_m)) |> 
      mutate(snowPer = snow_raw_m/totalSnow) |> 
      select(sampledate, wateryear, snowPer)) |> 
  left_join(roadSalt |> select(wateryear, cl_kg)) |> 
  mutate(monthlyCl_kg_m = snowPer*cl_kg) |> 
  mutate(monthlyCl_kg_m = if_else(is.na(monthlyCl_kg_m), 0, monthlyCl_kg_m))
