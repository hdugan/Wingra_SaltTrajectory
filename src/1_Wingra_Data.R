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
# Load map
source('src/Figure_Map.R')

################# Data loading and cleaning ################ 
### Load climate data
source('src/0_LoadMet.R')

# Data from arboretum and charmany farms

# ET monthly from open ET monthly ensemble
openET.wingra = read_csv('InputData/WingraOpenETensemble.csv')

# Mean ET 0.627 for Wingra
openETmonth = openET.wingra |> 
  mutate(year = year(Date), month = month(Date)) |> 
  group_by(month) |> 
  summarise(et = mean(et_ensemble_mad/1000)) |> 
  ungroup() 

# Subtracting ET from Precipitation to find runoff (assuming ET = 0.75 each year)
# If runoff was 0 for a year, assume that it is 0.05
ET_Precip <- met.month %>% 
  ungroup() |> 
  left_join(openETmonth) |> 
  mutate(frozen = if_else(days_above_0 < 5, TRUE, FALSE)) |> 
  mutate(group = cumsum(lag(frozen, default = FALSE) == FALSE)) |> 
  group_by(group) |> 
  mutate(usePrecip_m = sum(precip_raw_m)) |> 
  mutate(usePrecip_m = if_else(frozen == TRUE, 0, usePrecip_m)) |> 
  # mutate(runoff = (precip_raw_m - 0.75)) %>% 
  mutate(runoff = usePrecip_m - et) %>%
  mutate(runoff = if_else(runoff < 0, 0, runoff))

range(ET_Precip$runoff)

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
  select(YearStart, wateryear = YearEnd, TotalSnow_in, TotalSalt_tons, DaneCounty) |>
  left_join(met.year) %>% 
  mutate(ExtraPrivate_Estimate = case_when(wateryear <= 1970 ~ totalSnow * 39.37 * 50,
                                           wateryear <= 1980 ~ totalSnow * 39.37 * 200,
                                           wateryear <= 2010 ~ totalSnow * 39.37 * 200,
                                           wateryear <= 2025 ~ totalSnow * 39.37 * 300)) %>% 
  # mutate(ExtraPrivate_Estimate = case_when(wateryear <= 1970 ~ TotalSnow_in * 50,
  #                                          wateryear <= 1980 ~ TotalSnow_in * 150,
  #                                          wateryear <= 2010 ~ TotalSnow_in * 200,
  #                                          wateryear <= 2025 ~ TotalSnow_in * 300)) %>% 
  mutate(wingra_tons = (TotalSalt_tons * 0.0866) + (ExtraPrivate_Estimate* 0.0866)) |> 
  mutate(privatePublicRatio = ExtraPrivate_Estimate/TotalSalt_tons) %>% 
  mutate(salt_kg = wingra_tons * 907.185) |>
  mutate(cl_kg = salt_kg * 0.610)

saltuse = met.month |> 
  left_join(
    met.month |> 
      mutate(wateryear = if_else(month(sampledate) >= 10, year + 1, year)) |> 
      group_by(wateryear) |> 
      mutate(totalSnow = sum(snow_raw_m)) |> 
      mutate(snowPer = snow_raw_m/totalSnow) |> 
      select(sampledate, wateryear, snowPer)) |> 
  left_join(roadSalt |> select(wateryear, cl_kg)) |> 
  mutate(monthlyCl_kg_m = snowPer*cl_kg) |> 
  mutate(monthlyCl_kg_m = if_else(is.na(monthlyCl_kg_m), 0, monthlyCl_kg_m))

# range private:public
range(roadSalt %>% filter(wateryear >= 1980) %>% pull(privatePublicRatio))
mean(roadSalt %>% filter(wateryear >= 1980) %>% pull(privatePublicRatio))
