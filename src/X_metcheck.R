######### Data from Dane County airport #########
# Load most recent 2024 data from Dane County airport
met2024 = read_csv('InputData/Climate/3857041.csv') |> 
  mutate(precip_raw_mm = PRCP, snow_raw_cm = SNOW/10, avg_air_temp_adjusted = TAVG) |> 
  rename(sampledate = DATE) |> 
  mutate(year4 = year(sampledate)) |> 
  select(year4, sampledate, precip_raw_mm, snow_raw_cm, avg_air_temp_adjusted)

# Load core dataset (currently ends Dec 2023)
airportMet = read_csv('InputData/Climate/ntl20_v13.csv') |> 
  bind_rows(met2024) |> # bind 2024 data
  group_by(sampledate) |> 
  summarise(temp_mean = mean(avg_air_temp_adjusted, na.rm = T), 
            precip_raw_m = sum(precip_raw_mm, na.rm = T)/1000, 
            snow_raw_m = sum(snow_raw_cm, na.rm = T)/100) |> 
  filter(sampledate >= as.Date('1962-01-01'))

# Creat annual met based on water year
airport.yearMet = airportMet |> 
  mutate(wateryear = if_else(month(sampledate) >= 10, year(sampledate) + 1, year(sampledate))) |> 
  group_by(wateryear) |> 
  summarise(totalSnow = sum(snow_raw_m), totalPrecip = sum(precip_raw_m)) |> 
  filter(wateryear != 2025) |> 
  filter(wateryear != 1963)

######### Data from arboretum and charmany farms #########
rawmet = read_csv('InputData/Climate/3944435.csv') |> 
  mutate(PRCP = if_else(PRCP > 1000, NA, PRCP))

arbMet = rawmet |> 
  group_by(DATE, STATION) |> 
  summarise(PRCP = mean(PRCP, na.rm = T), SNOW = mean(SNOW, na.rm = T), 
            TMAX = mean(TMAX, na.rm = T), TMIN = mean(TMIN, na.rm = T)) |> #Take mean of two stations 
  mutate(year = year(DATE)) |> mutate(month = month(DATE)) |> 
  # mutate(year = if_else(month(sampledate) >= 10, year + 1, year)) |> 
  group_by(year, month, STATION) |> 
  summarise(sampledate = first(DATE), 
            tmax = mean(TMAX, na.rm = T), 
            precip_raw_m = sum(PRCP, na.rm = T)/1000, 
            snow_raw_m = sum(SNOW, na.rm = T)/1000, 
            days_above_5 = sum(TMAX > 5, na.rm = T)) |> 
  filter(year >= 1962, year <= 2024)

yearMet = arbMet |> 
  mutate(wateryear = if_else(month(sampledate) >= 10, year + 1, year)) |> 
  group_by(wateryear, STATION) |> 
  summarise(totalSnow = sum(snow_raw_m), totalPrecip = sum(precip_raw_m)) |> 
  filter(wateryear != 2025) |> 
  filter(wateryear != 1963)

ggplot(yearMet) +
  geom_path(aes(x = wateryear, totalSnow, color = STATION)) +
  geom_path(data = airport.yearMet, aes(x = wateryear, totalSnow))

ggplot(yearMet) +
  geom_path(aes(x = wateryear, totalPrecip, color = STATION)) +
  geom_path(data = airport.yearMet, aes(x = wateryear, totalPrecip))

ggplot(yearMet) +
  geom_path(aes(x = wateryear, totalPrecip, color = STATION)) +
  geom_path(data = airport.yearMet, aes(x = wateryear, totalPrecip))

# Check temperature 
ggplot(rawmet) +
  geom_path(aes(x = DATE, y = TMAX, color = STATION)) +
  geom_path(data = airportMet, aes(x = sampledate, y = temp_mean))
