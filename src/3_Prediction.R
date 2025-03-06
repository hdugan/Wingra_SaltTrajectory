library(tidyverse)

##################### Prediction Model ######################
# Here take the actual runoff values, but for the future randomly sample 100 different scenarios 
# normal distribution with mean and SD taken from actual values
set.seed(12) # set seed for reproducibility
# As apply statement 
p.future <- lapply(1:100, function(i) {
  tibble(sampledate = seq.Date(as.Date('2024-12-01'), as.Date('2099-12-01'), by = 'month')) %>%
    mutate(
      time = row_number() + 737, 
      month = month(sampledate),
      runoff = sapply(month, function(m) {
        sample(ET_Precip$runoff[month(ET_Precip$sampledate) == m], 1)
      }))
})

# Also run scenario where it's 10% wetter overall
p.future.10wetter <- lapply(1:100, function(i) {
  tibble(sampledate = seq.Date(as.Date('2024-12-01'), as.Date('2099-12-01'), by = 'month')) %>%
    mutate(
      time = row_number() + 737, 
      month = month(sampledate),
      runoff = sapply(month, function(m) {
        sample(ET_Precip$runoff[month(ET_Precip$sampledate) == m], 1)
      })) %>% 
    mutate(runoff = 1.10*runoff)
})

# Check if runoff scenarios look the same, check timeseries
bind_rows(p.future) |> group_by(month = month(sampledate)) |> summarise(runoff = mean(runoff)) |> 
  ggplot() + geom_col(aes(month, runoff))

ET_Precip |> group_by(month = month(sampledate)) |> summarise(runoff = mean(runoff)) |> 
  ggplot() + geom_col(aes(month, runoff))

bind_rows(p.future) |> group_by(year = year(sampledate)) |> summarise(runoff = mean(runoff)) |> 
  ggplot() + geom_col(aes(year, runoff))

# Current mean road salt use 
meanSaltUse = saltuse |> filter(year >= 2000) |> group_by(month) |> summarise(mean_monthlyCl_kg_m = mean(monthlyCl_kg_m))
sdSaltUse = saltuse |> filter(year >= 2000) |> group_by(month) |> summarise(sd_monthlyCl_kg_m = sd(monthlyCl_kg_m))

# Set up standard tibble 
salt.future = tibble(sampledate = seq.Date(as.Date('2024-12-01'), as.Date('2099-12-01'), by = 'month')) |> 
  mutate(
    time = row_number() + 737, 
    month = month(sampledate)) |> 
  left_join(meanSaltUse) |> left_join(sdSaltUse) 

# Dataframe of road salt use based on historical norms
roadsalt.future <- function(decrease) {
  salt.future |> 
  rowwise() %>%  # Ensure row-wise operation
  mutate(salt_input = rnorm(1, mean = mean_monthlyCl_kg_m*decrease, sd = sd_monthlyCl_kg_m*decrease)) %>% 
  mutate(salt_input = if_else(salt_input < 0, 0, salt_input))}

set.seed(12) # set seed for reproducibility 
salt.future.100 = roadsalt.future(1)
ggplot(salt.future.100) + geom_path(aes(x = sampledate, y = salt_input))

# Decrease useage 
salt.future.75 = salt.future.100 |> mutate(salt_input = 0.75 * salt_input)
salt.future.50 = salt.future.100 |> mutate(salt_input = 0.50 * salt_input)
salt.future.25 = salt.future.100 |> mutate(salt_input = 0.25 * salt_input)
salt.future.0 = salt.future.100 |> mutate(salt_input = 0 * salt_input)

#Gather parameters for predicting into the future 
times = 738:1638
inits = c(SW = ss.1960 |> slice_tail() |> pull(SW), SL = ss.1960 |> slice_tail() |> pull(SL))

#Solve through time from 2024 to 2160 with current road salt use 
ss.list.100 <- lapply(1:100, function(i) {
  ode(inits, times, dSalt, parms = pars, p_df = p.future[[i]], salt_df = salt.future.100) |> 
    as_tibble() |> 
    mutate(across(everything(), as.numeric)) |>  
    mutate(CL = (SL / as.numeric(wingra.volume)) * 1000,  # Compute CL
           sampledate = as.Date("1963-06-01") %m+% months(time))  # Compute sampledate
})
# Bind scenarios together with min, mean, and max
ss.future.100 = bind_rows(ss.list.100, .id = "id") |> 
  group_by(time, year = year(sampledate), month = month(sampledate)) |> 
  summarise(sampledate = first(sampledate), CL.mean = mean(CL), CL.max = max(CL), CL.min = min(CL))

#Solve through time from 2024 to 2160 with 75% road salt use 
ss.list.75 <- lapply(1:100, function(i) {
  ode(inits, times, dSalt, parms = pars, p_df = p.future[[i]], salt_df = salt.future.75) |> 
    as_tibble() |> 
    mutate(across(everything(), as.numeric)) |>  
    mutate(CL = (SL / as.numeric(wingra.volume)) * 1000,  # Compute CL
           sampledate = as.Date("1963-06-01") %m+% months(time))  # Compute sampledate
})
# Bind scenarios together with min, mean, and max
ss.future.75 = bind_rows(ss.list.75, .id = "id") |> 
  group_by(time, year = year(sampledate), month = month(sampledate)) |> 
  summarise(sampledate = first(sampledate), CL.mean = mean(CL), CL.max = max(CL), CL.min = min(CL))

#Solve through time from 2024 to 2160 with 50% road salt use 
ss.list.50 <- lapply(1:100, function(i) {
  ode(inits, times, dSalt, parms = pars, p_df = p.future[[i]], salt_df = salt.future.50) |> 
    as_tibble() |> 
    mutate(across(everything(), as.numeric)) |>  
    mutate(CL = (SL / as.numeric(wingra.volume)) * 1000,  # Compute CL
           sampledate = as.Date("1963-06-01") %m+% months(time))  # Compute sampledate
})
# Bind scenarios together with min, mean, and max
ss.future.50 = bind_rows(ss.list.50, .id = "id") |> 
  group_by(time, year = year(sampledate), month = month(sampledate)) |> 
  summarise(sampledate = first(sampledate), CL.mean = mean(CL), CL.max = max(CL), CL.min = min(CL))

#Solve through time from 2024 to 2160 with 25% road salt use 
ss.list.25 <- lapply(1:100, function(i) {
  ode(inits, times, dSalt, parms = pars, p_df = p.future[[i]], salt_df = salt.future.25) |> 
    as_tibble() |> 
    mutate(across(everything(), as.numeric)) |>  
    mutate(CL = (SL / as.numeric(wingra.volume)) * 1000,  # Compute CL
           sampledate = as.Date("1963-06-01") %m+% months(time))  # Compute sampledate
})
# Bind scenarios together with min, mean, and max
ss.future.25 = bind_rows(ss.list.25, .id = "id") |> 
  group_by(time, year = year(sampledate), month = month(sampledate)) |> 
  summarise(sampledate = first(sampledate), CL.mean = mean(CL), CL.max = max(CL), CL.min = min(CL))

#Solve through time from 2024 to 2160 with 0 road salt use 
ss.list.0 <- lapply(1:100, function(i) {
  ode(inits, times, dSalt, parms = pars, p_df = p.future[[i]], salt_df = salt.future.0) |> 
    as_tibble() |> 
    mutate(across(everything(), as.numeric)) |>  
    mutate(CL = (SL / as.numeric(wingra.volume)) * 1000,  # Compute CL
           sampledate = as.Date("1963-06-01") %m+% months(time))  # Compute sampledate
})
# Bind scenarios together with min, mean, and max
ss.future.0 = bind_rows(ss.list.0, .id = "id") |> 
  group_by(time, year = year(sampledate), month = month(sampledate)) |> 
  summarise(sampledate = first(sampledate), CL.mean = mean(CL), CL.max = max(CL), CL.min = min(CL))

#Solve through time from 2024 to 2160 with current road salt use but 10% wetter
ss.list.100.10wetter <- lapply(1:100, function(i) {
  ode(inits, times, dSalt, parms = pars, p_df = p.future.10wetter[[i]], salt_df = salt.future.100) |> 
    as_tibble() |> 
    mutate(across(everything(), as.numeric)) |>  
    mutate(CL = (SL / as.numeric(wingra.volume)) * 1000,  # Compute CL
           sampledate = as.Date("1963-06-01") %m+% months(time))  # Compute sampledate
})
# Bind scenarios together with min, mean, and max
ss.future.100.10wetter = bind_rows(ss.list.100.10wetter, .id = "id") |> 
  group_by(time, year = year(sampledate), month = month(sampledate)) |> 
  summarise(sampledate = first(sampledate), CL.mean = mean(CL), CL.max = max(CL), CL.min = min(CL))

