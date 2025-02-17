##################### Prediciton Model ######################
# Here take the actual runoff values, but for the future randomly sample 100 different scenarios 
# normal distribution with mean and SD taken from actual values
p.future = list()
for (i in 1:100) {
  p.future[[i]] = data.frame(time = 63:200, 
                             p = (rnorm(138, mean(ET_Precip$runoff), sd(ET_Precip$runoff)))) |> 
    mutate(p = if_else(p < 0, 0.05, p))
}

# Current mean road salt use 
meanSaltUse = roadSalt |> filter(year >= 2000) |> summarise(mean(salt_kg_m_2)) |> as.numeric()
sdSaltUse = roadSalt |> filter(year >= 2000) |> summarise(sd(salt_kg_m_2)) |> as.numeric()

# Dataframe of road salt use if it didn't decrease
salt.future = data.frame(time = 63:200, 
                         salt_input = rnorm(138, meanSaltUse, sdSaltUse)) # 7 is the mean road salt use from 2000-present
# Dataframe of road salt use if decreased to 1/4
salt.future.25 = data.frame(time = 63:200, 
                            salt_input = rnorm(138, meanSaltUse*0.75, sdSaltUse*0.75)) 
# Dataframe of road salt use if decreased to 1/2
salt.future.50 = data.frame(time = 63:200, 
                            salt_input = rnorm(138, meanSaltUse*0.50, sdSaltUse*0.50)) 
# Dataframe of road salt use if decreased to 3/4
salt.future.75 = data.frame(time = 63:200, 
                            salt_input = rnorm(138, meanSaltUse*0.25, sdSaltUse*0.25)) 
# Dataframe of road salt use if decreased to 100%
salt.future.100 = data.frame(time = 63:200, 
                            salt_input = 0) 
#Gather parameters for predicting into the future 
times = 63:200
inits = c(SW = ss.1960 |> slice_tail() |> pull(SW), SL = ss.1960 |> slice_tail() |> pull(SL))

#Solve through time from 2024 to 2160 with current road salt use 
ss.list = list()
for (i in 1:20) {
  ss.list[[i]] <- ode(inits,times,dSalt, parms = pars, p_df = p.future[[i]], salt_df = salt.future) |> 
    as_tibble() |> mutate_all(list(as.numeric)) |> 
    mutate(scenario = 'alpha = 5') %>% 
    mutate(CL = (SL/volume)*1000) |> 
    mutate(year = time + 1961)}

# Bind scenarios together with min, mean, and max
ss.future = bind_rows(ss.list, .id = "id") |> 
  group_by(time, year) |> 
  summarise(CL.mean = mean(CL), CL.max = max(CL), CL.min = min(CL))

#Solve through time from 2024 to 2160 using road salt data frame of 1/4 reduced salt use 
ss.list.25 = list()
for (i in 1:20) {
  ss.list.25[[i]] <- ode(inits,times,dSalt, parms = pars, p_df = p.future[[i]], salt_df = salt.future.25) |> 
    as_tibble() |> mutate_all(list(as.numeric)) |> 
    mutate(scenario = 'alpha = 5') %>% 
    mutate(CL = (SL/volume)*1000) |> 
    mutate(year = time + 1961)}

# Bind scenarios together with min, mean, and max for 1/4 reduction
ss.future.25 = bind_rows(ss.list.25) |> 
  group_by(time, year) |> 
  summarise(CL.mean = mean(CL), CL.max = max(CL), CL.min = min(CL))

# Solve through time from 2024 to 2160 using road salt data frame of 1/2 reduced salt use 
ss.list.50 = list()
for (i in 1:20) {
  ss.list.50[[i]] <- ode(inits,times,dSalt, parms = pars, p_df = p.future[[i]], salt_df = salt.future.50) |> 
    as_tibble() |> mutate_all(list(as.numeric)) |> 
    mutate(scenario = 'alpha = 5') %>% 
    mutate(CL = (SL/volume)*1000) |> 
    mutate(year = time + 1961)}

# Bind scenarios together with min, mean, and max to 1/2 application rate
ss.future.50 = bind_rows(ss.list.50) |> 
  group_by(time, year) |> 
  summarise(CL.mean = mean(CL), CL.max = max(CL), CL.min = min(CL))

# Solve through time from 2024 to 2160 using road salt data frame of 3/4 reduced salt use 
ss.list.75 = list()
for (i in 1:20) {
  ss.list.75[[i]] <- ode(inits,times,dSalt, parms = pars, p_df = p.future[[i]], salt_df = salt.future.75) |> 
    as_tibble() |> mutate_all(list(as.numeric)) |> 
    mutate(scenario = 'alpha = 5') %>% 
    mutate(CL = (SL/volume)*1000) |> 
    mutate(year = time + 1961)}

# Bind scenarios together with min, mean, and max to 3/4 application rate
ss.future.75 = bind_rows(ss.list.75) |> 
  group_by(time, year) |> 
  summarise(CL.mean = mean(CL), CL.max = max(CL), CL.min = min(CL))

# Solve through time from 2024 to 2160 using road salt data frame of 100% reduced salt use 
ss.list.100 = list()
for (i in 1:20) {
  ss.list.100[[i]] <- ode(inits,times,dSalt, parms = pars, p_df = p.future[[i]], salt_df = salt.future.100) |> 
    as_tibble() |> mutate_all(list(as.numeric)) |> 
    mutate(scenario = 'alpha = 5') %>% 
    mutate(CL = (SL/volume)*1000) |> 
    mutate(year = time + 1961)}

# Bind scenarios together with min, mean, and max to 0 application rate
ss.future.100 = bind_rows(ss.list.100) |> 
  group_by(time, year) |> 
  summarise(CL.mean = mean(CL), CL.max = max(CL), CL.min = min(CL))

