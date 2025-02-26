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
monthMet = read_csv('InputData/ntl20_v13.csv') |> 
  bind_rows(met2024) |> # bind 2024 data
  rename(year = year4) |> mutate(month = month(sampledate)) |> 
  # mutate(year = if_else(month(sampledate) >= 7, year + 1, year)) |> 
  group_by(year, month) |> 
  summarise(sampledate = first(sampledate), precip_raw_m = sum(precip_raw_mm, na.rm = T)/1000, snow_raw_m = sum(snow_raw_cm, na.rm = T)/100) |> 
  filter(year >= 1962, year <= 2024)

arbMet = read_csv('InputData/Climate/3944435.csv') |> 
  group_by(DATE) |> 
  summarise(PRCP = mean(PRCP, na.rm = T), SNOW = mean(SNOW, na.rm = T), 
            TMAX = mean(TMAX, na.rm = T), TMIN = mean(TMIN, na.rm = T)) |> #Take mean of two stations 
  mutate(year = year(DATE)) |> mutate(month = month(DATE)) |> 
  # mutate(year = if_else(month(sampledate) >= 7, year + 1, year)) |> 
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
openETmonth = openET.wingra |> bind_rows(openET.mendota) |> 
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

# roadSalt = read_csv('InputData/CityMadison_roadsalt_2024update.csv') |> 
#   select(YearEnd, TotalSalt_tons) |>
#   rename(salt_tons = TotalSalt_tons, wateryear = YearEnd) |>
#   mutate(salt_kg = salt_tons * 907.185) |>
#   mutate(salt_kg = salt_kg * 0.610) |> # convert to chloride
#   mutate(salt_kg_m = salt_kg / 1252066.52) |>
#   mutate(salt_kg_m_2 = salt_kg_m*1.5) |> 
#   filter(wateryear >= 1962)
# 
# saltuse = monthMet |> left_join(
#   monthMet |> 
#   # filter(snow_raw_m > 0) |>
#   mutate(wateryear = if_else(month(sampledate) >= 7, year + 1, year)) |> 
#   group_by(wateryear) |> 
#   mutate(totalSnow = sum(snow_raw_m)) |> 
#   mutate(snowPer = snow_raw_m/totalSnow) |> 
#   select(sampledate, wateryear, snowPer)) |> 
#   left_join(roadSalt |> select(wateryear, salt_kg_m)) |> 
#   mutate(monthlysalt_kg_m = snowPer*salt_kg_m) |> 
#   mutate(monthlysalt_kg_m = if_else(is.na(monthlysalt_kg_m), 0, monthlysalt_kg_m))

#### For Wingra
roadSalt = read_csv('InputData/CityMadison_roadsalt_2024update.csv') |> 
  select(YearStart, wateryear = YearEnd, TotalSalt_tons, DaneCounty) |>
  mutate(wingra_tons = (TotalSalt_tons * 0.0866)) |> 
  mutate(salt_kg = wingra_tons * 907.185) |>
  mutate(cl_kg = salt_kg * 0.610)

saltuse = arbMet |> 
  left_join(
    arbMet |> 
      # filter(snow_raw_m > 0) |>
      mutate(wateryear = if_else(month(sampledate) >= 7, year + 1, year)) |> 
      group_by(wateryear) |> 
      mutate(totalSnow = sum(snow_raw_m)) |> 
      mutate(snowPer = snow_raw_m/totalSnow) |> 
      select(sampledate, wateryear, snowPer)) |> 
  left_join(roadSalt |> select(wateryear, cl_kg)) |> 
  mutate(monthlyCl_kg_m = snowPer*cl_kg) |> 
  mutate(monthlyCl_kg_m = if_else(is.na(monthlyCl_kg_m), 0, monthlyCl_kg_m))

#### For Mendota ####
# roadSalt = read_csv('InputData/CityMadison_roadsalt_2024update.csv') |> 
#   select(YearStart, wateryear = YearEnd, TotalSalt_tons, DaneCounty) |>
#   filter(YearStart >= 2007) |> 
#   mutate(mendota_tons = (TotalSalt_tons * 0.225) + (DaneCounty*0.16)) |> 
#   mutate(salt_kg = mendota_tons * 907.185) |>
#   mutate(cl_kg = salt_kg * 0.610)
# 
# saltuse = monthMet |> 
#   filter(year >= 2007) |> 
#   left_join(
#   monthMet |> 
#     # filter(snow_raw_m > 0) |>
#     mutate(wateryear = if_else(month(sampledate) >= 7, year + 1, year)) |> 
#     group_by(wateryear) |> 
#     mutate(totalSnow = sum(snow_raw_m)) |> 
#     mutate(snowPer = snow_raw_m/totalSnow) |> 
#     select(sampledate, wateryear, snowPer)) |> 
#   left_join(roadSalt |> select(wateryear, cl_kg)) |> 
#   mutate(monthlyCl_kg_m = snowPer*cl_kg) |> 
#   mutate(monthlyCl_kg_m = if_else(is.na(monthlyCl_kg_m), 0, monthlyCl_kg_m))
#   
