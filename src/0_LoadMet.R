###############################################################
######################## Load met data ########################
###############################################################

### Load climate data from Arb and Charmany Farms
rawmet = read_csv('InputData/Climate/3944435.csv') |> 
  mutate(PRCP = if_else(PRCP > 1000, NA, PRCP)) %>% 
  filter(STATION == 'USC00471416')  # select Charmany Farms

#### Data from Dane County airport ###
# Load most recent 2024 data from Dane County airport
met2024 = read_csv('InputData/Climate/3857041.csv') |> 
  mutate(avg_air_temp_adjusted = TAVG) %>% 
  rename(sampledate = DATE) |> 
  select(sampledate, avg_air_temp_adjusted)

# Load core dataset (currently ends Dec 2023)
airportMet = read_csv('InputData/Climate/ntl20_v13.csv') |> 
  bind_rows(met2024) |> # bind 2024 data
  group_by(sampledate) |> 
  summarise(temp_mean = mean(avg_air_temp_adjusted, na.rm = T)) %>% 
  filter(sampledate >= as.Date('1962-01-01'))

# Join Charmany precip with Dane County air temp
met.day = rawmet %>% select(sampledate = DATE, PRCP, SNOW) %>% 
  left_join(airportMet)

met.month = met.day |> 
  mutate(year = year(sampledate)) |> mutate(month = month(sampledate)) |> 
  # mutate(year = if_else(month(sampledate) >= 10, year + 1, year)) |> 
  group_by(year, month) |> 
  summarise(sampledate = first(sampledate), precip_raw_m = sum(PRCP, na.rm = T)/1000, 
            snow_raw_m = sum(SNOW, na.rm = T)/1000, 
            days_above_0 = sum(temp_mean > 0, na.rm = T)) |> 
  filter(year >= 1962, year <= 2024)

met.year = met.month |> 
  mutate(wateryear = if_else(month(sampledate) >= 10, year + 1, year)) |> 
  group_by(wateryear) |> 
  summarise(totalSnow = sum(snow_raw_m), totalPrecip = sum(precip_raw_m)) |> 
  filter(wateryear != 2025) |> 
  filter(wateryear != 1963)

# ggplot(met.year) +
#   geom_col(aes(x = wateryear, y = totalPrecip))
#   
