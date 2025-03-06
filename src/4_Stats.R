library(tidyverse)

# Chloride ranges
monthlyCl %>% filter(year == 1963)
filter(monthlyCl %>% ungroup(), Chloride.mgL == max(Chloride.mgL, na.rm = T))
filter(monthlyCl %>% ungroup(), Chloride.mgL == min(Chloride.mgL, na.rm = T))
monthlyCl %>% 
  mutate(wateryear = if_else(month(sampledate) >= 10, year + 1, year)) |> 
  group_by(wateryear) |> 
  summarize(
  min_value = min(Chloride.mgL, na.rm = T),
  max_value = max(Chloride.mgL, na.rm = T),
  range_value = max_value - min_value) %>% 
  arrange(desc(range_value))

# Road salt ranges
filter(roadSalt %>% ungroup(), TotalSalt_tons == max(TotalSalt_tons, na.rm = T))
filter(roadSalt %>% filter(wateryear >= 1963) %>% ungroup(), 
       TotalSalt_tons == min(TotalSalt_tons, na.rm = T))

roadSalt %>% filter(wateryear >= 2005 & wateryear < 2015) %>% summarise(mean(TotalSalt_tons))
roadSalt %>% filter(wateryear >= 2015) %>% summarise(mean(TotalSalt_tons))
7105/12129

# Climate stats
filter(met.year %>% ungroup(), totalSnow == max(totalSnow, na.rm = T))
tail(met.year, n = 10)

# Scenario based analysis
scenario_data <- bind_rows(
  ss.future.0 %>% mutate(scenario = "100% Reduction") |> mutate(reduction = '100%'),
  ss.future.25 %>% mutate(scenario = "75% Reduction") |> mutate(reduction = '75%'),
  ss.future.50 %>% mutate(scenario = "50% Reduction") |> mutate(reduction = '50%'),
  ss.future.75 %>% mutate(scenario = "25% Reduction") |> mutate(reduction = '25%'),
  ss.future.100 %>% mutate(scenario = "0% Reduction") |> mutate(reduction = '0%')) |> 
  mutate(scenario = factor(scenario, levels = c("0% Reduction", "25% Reduction", "50% Reduction", "75% Reduction", "100% Reduction"))) |> 
  mutate(reduction = factor(reduction, levels = c("0%", "25%", "50%", "75%", "100%")))

###### Annual means ######
scenario_data_annual = scenario_data |> 
  mutate(wateryear = if_else(month(sampledate) >= 10, year + 1, year)) |> 
  group_by(scenario, wateryear) |> 
  summarise(CL.mean = mean(CL.mean), CL.max = mean(CL.max), CL.min = mean(CL.min))
  
annualCl = monthlyCl |> 
  mutate(wateryear = if_else(month(sampledate) >= 10, year + 1, year)) |> 
  group_by(wateryear) |> 
  summarise(Chloride.mgL = mean(Chloride.mgL, na.rm = T))

annual.1960 = ss.1960 |> 
  mutate(wateryear = if_else(month(sampledate) >= 10, year(sampledate) + 1, year(sampledate))) |> 
  group_by(wateryear) |> 
  summarise(CL = mean(CL, na.rm = T))

###### High vs. Low snow years ######

# Define 95% and 5% percentiles for low and high precip and snow
precip.05 <- round(quantile(met.year$totalPrecip, 0.10),2)
precip.95 <- round(quantile(met.year$totalPrecip, 0.90),2)
snow.05 <- round(quantile(met.year$totalSnow, 0.10, na.rm = TRUE),2)
snow.95 <- round(quantile(met.year$totalSnow, 0.90, na.rm = TRUE),2)

precip.snow <- met.year %>%
  mutate(Rain = case_when(
    totalPrecip <= precip.05 ~ "Low Precip",
    totalPrecip >= precip.95 ~ "High Precip",
    TRUE ~ "Normal Precip")) %>%
  mutate(Snow = case_when(
    totalSnow <= snow.05 ~ "Low Snow",
    totalSnow >= snow.95 ~ "High Snow",
    TRUE ~ "Normal Snow")) %>%
  left_join(annualCl, by = "wateryear")

#### Met data #####
met.year %>% summarise(max(totalSnow), min(totalSnow), mean(totalSnow))
arrange(met.year, desc(totalSnow))

#### Chloride data #####
cl.outliers %>% filter(Lake == "Wingra") %>% group_by(outlier) %>% tally()

cl.outliers %>% 
  filter(Lake %in% c('Wingra')) %>% group_by(year = year(Date)) %>% tally() %>% 
  arrange(desc(year))

#### Parking lot data #####
sum(st_area(is_wingra.paved)) # driveways, parking, sidewalks ()
sum(st_area(is_wingra.parkinglots)) # driveways, parking, sidewalks (214 acres)
sum(st_area(is_wingra.roads))
sum(st_area(is_wingra.paved))/sum(st_area(is_wingra.roads))
# 2268453 [m^2] = 560 acres
# 230 tons per event
# st_area(wingraCat) 4895 acres
# sum(st_area(is_wingra.roads)) #453 acres

# Industry std calls for 6-800lbs per acre... but often higher
sum(st_area(is_wingra.parkinglots))
# 869414.8 [m^2]; 214 acres
# 113 tons per event 
parking.kg = 214 * 1000 * 0.453592 # 1000 pounds per acre converted to kg
parking.kg * 0.610 # convert to kg chloride 

parking.tons = 214 * 1000 * 0.0005
roadSalt$TotalSalt_tons * 0.0866
roadSalt$ExtraPrivate_Estimate * 0.0866 # 1000 tons a year 

#### Scenarios ####
# Calculating the 20 year window average per scenario
y.2050s <- scenario_data %>% 
  filter(year >= 2050 & year < 2060) %>% 
  group_by(scenario) %>%
  summarize(avg_concentration = mean(CL.mean, na.rm = TRUE), min(CL.mean, na.rm = T), max(CL.mean, na.rm = T))
print(y.2050s)

y.2100 <- scenario_data %>% 
  filter(year >= 2090) %>% 
  group_by(scenario) %>%
  summarize(avg_concentration = mean(CL.mean, na.rm = TRUE), min(CL.mean, na.rm = T), max(CL.mean, na.rm = T))
print(y.2100)

