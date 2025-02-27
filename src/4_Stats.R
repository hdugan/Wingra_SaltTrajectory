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
scenario_data_annaul = scenario_data |> 
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

###### High vs. Low snow years #######
# Creat annual met based on water year
yearMet = arbMet |> 
  mutate(wateryear = if_else(month(sampledate) >= 10, year + 1, year)) |> 
  mutate(summerRain = if_else(!month %in% c(11,12,1,2,3,4), precip_raw_m, 0)) |> 
  group_by(wateryear) |> 
  summarise(totalSnow = sum(snow_raw_m), totalPrecip = sum(precip_raw_m), summerPrecip = sum(summerRain)) |> 
  filter(wateryear != 2025) |> 
  filter(wateryear != 1963)

# Define 95% and 5% percentiles for low and high precip and snow
precip.05 <- round(quantile(yearMet$totalPrecip, 0.10),2)
precip.95 <- round(quantile(yearMet$totalPrecip, 0.90),2)
snow.05 <- round(quantile(yearMet$totalSnow, 0.10, na.rm = TRUE),2)
snow.95 <- round(quantile(yearMet$totalSnow, 0.90, na.rm = TRUE),2)

precip.snow <- yearMet %>%
  mutate(Rain = case_when(
    totalPrecip <= precip.05 ~ "Low Precip",
    totalPrecip >= precip.95 ~ "High Precip",
    TRUE ~ "Normal Precip")) %>%
  mutate(Snow = case_when(
    totalSnow <= snow.05 ~ "Low Snow",
    totalSnow >= snow.95 ~ "High Snow",
    TRUE ~ "Normal Snow")) %>%
  left_join(annualCl, by = "wateryear")


################# Statistics ################# 
times = 1:63
inits = c(SW = 0.5e7, SL=51483.6) # Starting values in 1962 (estimates)

# Create 10-year rolling means of Precipitation and Road Salt 
salt_df_mean = salt_df |> mutate(salt_input = rollapply(salt_input, 10, mean, align = 'center', fill = "extend"))
p_df_mean = p_df |> mutate(p = rollapply(p, 10, mean, align = 'center', fill = "extend"))

# Run model with 10-year rolling means
ss.mean <- ode(inits,times,dSalt, parms = pars, p_df = p_df_mean, salt_df = salt_df_mean) |> 
  as_tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 5') %>% 
  mutate(CL = (SL/volume)*1000) |> 
  mutate(year = time + 1961)

# Run model with actual precip, 10-year mean of salt
ss.precip <- ode(inits,times,dSalt, parms = pars, p_df = p_df, salt_df = salt_df_mean) |> 
  as_tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 5') %>% 
  mutate(CL = (SL/volume)*1000) |> 
  mutate(year = time + 1961)

# Run model with actual salt, 10-year mean of precip
ss.salt <- ode(inits,times,dSalt, parms = pars, p_df = p_df_mean, salt_df = salt_df) |> 
  as_tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 5') %>% 
  mutate(CL = (SL/volume)*1000) |> 
  mutate(year = time + 1961)

# Calculate RMSE of scenarios
rmse.precip = sqrt(mean((ss.precip$CL - ss.1960$CL)^2))
rmse.salt = sqrt(mean((ss.salt$CL - ss.1960$CL)^2))
rmse.mean = sqrt(mean((ss.mean$CL - ss.1960$CL)^2))

### Comparing historical chloride with modeled chloride
# Calculate RMSE and r^2 between real and modeled chloride
comparison_data <- merge(annualCl, ss.1960, by = "year", suffixes = c(".actual", ".modeled"))
rmse.real.model = sqrt(mean((comparison_data$Chloride.mgL - comparison_data$CL)^2))
r.sq2 = round(summary(lm(comparison_data$Chloride.mgL~comparison_data$CL))$r.squared, 2)

### Does historical road salt total match snow totals? 
# run R^2
salt_snow = roadSalt %>% 
  left_join(yearMet, by = "year") %>% 
  filter(year >= 2000 & year <= 2024) 
r.sq1 = round(summary(lm(salt_snow$snow_raw_m~salt_snow$salt_tons))$r.squared, 2)
# run R^2 for road salt and precip
r.sq7 = round(summary(lm(salt_snow$precip_raw_m~salt_snow$salt_tons))$r.squared, 2)

### Total accumulation R^2 yearMet, aes(year, snow_raw_m
r.sq3 = round(summary(lm(yearMet$snow_raw_m~yearMet$year))$r.squared, 2)
r.sq4 = round(summary(lm(yearMet$precip_raw_m~yearMet$year))$r.squared, 2)

# Slope
summary(lm(yearMet$snow_raw_m~yearMet$year))
summary(lm(yearMet$precip_raw_m~yearMet$year))


 

# Define chloride, precip, and snow ranges and combine them
chloride_range <- c(4.12000, 125.00667)
precipitation_range <- c(0.1373, 1.2410)
snow_range <- c(0.01931, 0.25753)
combined_range <- range(precipitation_range, snow_range)

# Calculate the scale factor for both precipitation and snow with chloride
scale_factor_combined <- diff(chloride_range) / diff(combined_range)

# Calculate regressions for wingra and road salt accumulations
r.sq5 = round(summary(lm(annualCl$Chloride.mgL~annualCl$year))$r.squared, 2)
r.sq6 = round(summary(lm(roadSalt$salt_kg~roadSalt$year))$r.squared, 2)
r.sq8 = round(summary(lm(annualCl$Chloride.mgL~annualCl$year))$r.squared, 2)

# Calculating the 20 year window average per scenario
last_20_years <- scenario_data %>% 
  filter(year >= 2141 & year <= 2160)

# Calculate the average concentration for each scenario
average_concentrations <- scenario_data %>%
  group_by(scenario) %>%
  summarize(avg_concentration = mean(CL.mean, na.rm = TRUE))
print(average_concentrations)

last_20_years <- last_20_years %>%
  group_by(scenario) %>%
  summarize(avg_concentration = mean(CL.mean, na.rm = TRUE))
print(average_concentrations)

