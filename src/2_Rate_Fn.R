##################################################################
# Dynamic model function
##################################################################

################# Rate function for salt model ################# 
#State variables are:
#SW - Mass of salt in the watershed, kg Cl-
#SL - Mass of salt in the lake, kg Cl-

#A - Watershed area, m2 (note 1e6 m2 is 1 km2)
#p - runoff, m y-1
#phi - Salt yield of watershed per unit of precipitation, m-1
#V - Lake volume, m3

# To modify the ODE function in R so that it accepts a dynamic variable for p
# rather than a static parameter, you'll need to adjust how p is passed into the
# function. Instead of including p as part of the pars argument, 
# introduce p as it's own data.frame 

# p_df: This is the data.frame containing time and p values. 
# It should have two columns: one for time and one for p.
# p <- p_df$p[which.min(abs(p_df$time - time))]: 
# Finds the value of p in p_df that corresponds to the closest time value in the data frame.

dSalt <- function(time, state, pars, p_df, salt_df) {
  with(as.list(c(state, pars)), {
    # Extract the dynamic p based on the current time
    p <- p_df$runoff[which.min(abs(p_df$time - time))]
    
    # Similarly, extract dynamic road salt based on the current time (in kg)
    salt_input <- salt_df$salt_input[which.min(abs(salt_df$time - time))]
    salt_input = salt_input

    # usep becomes p that is retained in the watershed (r_d proportion that runsoff into lake)
    usep = p*(1 - r_d)
    usep <- ifelse(usep < 0, 0, usep)
    
    # cl_d = proportion of chloride that runs off into lake 
    dSW <- (1-cl_d)*(salt_input) - usep * phi * SW
    
    # Rate of change in Cl- in lake, kg y-1
    dSL <- (cl_d * salt_input) + (usep * phi * SW) - (usep + p*r_d) * A * (1/V) * SL
    
    # Return the rates of change
    return(list(c(dSW, dSL)))
  })
}

# Dataframes for model input for p and salt 
p_df = ET_Precip %>% ungroup() |> 
  filter(sampledate >= as.Date('1963-07-01')) |> 
  mutate(time = row_number()) %>% 
  select(time, runoff)

salt_df = saltuse %>%  ungroup() |> 
  mutate(time = row_number()) %>% 
  select(time, salt_input = monthlyCl_kg_m)

#### Gather parameters #### 
# Lake areas (V)
wingra.volume = as.numeric(st_area(yaharaLakes |> filter(NAME == 'Lake Wingra')) * 0.092903 * 2.7)
# Watershed areas (A)
wingra.area = as.numeric(st_area(wingraCat)* 0.092903)
# # Road meters (not required)
# wingra.roads = sum(as.numeric(st_length(wingraRoads))) * 0.3048 # convert survey foot to meters

# Timesteps in months
times = 1:737
inits = c(SW = 3e6, SL=51484) # Starting values in 1963 (estimate for watershed)
pars <- c(cl_d = 0.2, r_d = 0.1, phi = 0.2, A = wingra.area, V = wingra.volume)

# Run model from 1963 to 2024
ss.1960 <- ode(inits,times,dSalt, parms = pars, p_df = p_df, salt_df = salt_df) |> 
  as_tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(CL = (SL/as.numeric(wingra.volume))*1000) |> 
  mutate(sampledate = as.Date("1963-06-01") %m+% months(time))

# Calculate RMSE and r^2 between real and modeled chloride
# Join to observed values 
comparison_data = ss.1960 |> left_join(monthlyCl) 
rmse.monthly = sqrt(mean((comparison_data$Chloride.mgL - comparison_data$CL)^2, na.rm = T))
r.sq2 = round(summary(lm(comparison_data$Chloride.mgL~comparison_data$CL))$r.squared, 2)
print(rmse.monthly)

############ Run 10-year Means ###############
# Create 10-year rolling means of Precipitation and Road Salt 
salt_df_mean = salt_df |> mutate(salt_input = rollapply(salt_input, 5*12, mean, align = 'center', fill = "extend"))
p_df_mean = p_df |> mutate(runoff = rollapply(runoff, 5*12, mean, align = 'center', fill = "extend"))

# Run model with 10-year rolling means
ss.mean <- ode(inits,times,dSalt, parms = pars, p_df = p_df_mean, salt_df = salt_df_mean) |> 
  as_tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 5') %>% 
  mutate(CL = (SL/wingra.volume)*1000) |> 
  mutate(sampledate = as.Date("1963-06-01") %m+% months(time))

# Run model with actual precip, 10-year mean of salt
ss.precip <- ode(inits,times,dSalt, parms = pars, p_df = p_df, salt_df = salt_df_mean) |> 
  as_tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(CL = (SL/wingra.volume)*1000) |> 
  mutate(sampledate = as.Date("1963-06-01") %m+% months(time))

# Run model with actual salt, 10-year mean of precip
ss.salt <- ode(inits,times,dSalt, parms = pars, p_df = p_df_mean, salt_df = salt_df) |> 
  as_tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(CL = (SL/wingra.volume)*1000) |> 
  mutate(sampledate = as.Date("1963-06-01") %m+% months(time))

# Calculate RMSE of scenarios 
df.out = data.frame(sampledate = ss.1960$sampledate,
                    Chloride.mgL = comparison_data$Chloride.mgL, 
                    CL.model = ss.1960$CL, 
                    CL.precip = ss.precip$CL, 
                    CL.salt = ss.salt$CL,
                    CL.mean = ss.mean$CL)

# Filter to post 2000
df.out.2000 = df.out %>% filter(sampledate >= as.Date('2000-01-01'))

rmse.model.2000 = sqrt(mean((df.out.2000$CL.model - df.out.2000$Chloride.mgL)^2, na.rm = T)); print(rmse.model.2000)
round(summary(lm(df.out.2000$CL.model~df.out.2000$Chloride.mgL))$r.squared, 2)

rmse.precip.2000 = sqrt(mean((df.out.2000$CL.precip - df.out.2000$Chloride.mgL)^2, na.rm = T)); print(rmse.precip.2000)
round(summary(lm(df.out.2000$CL.precip~df.out.2000$Chloride.mgL))$r.squared, 2)

rmse.salt.2000 = sqrt(mean((df.out.2000$CL.salt - df.out.2000$Chloride.mgL)^2, na.rm = T)); print(rmse.salt.2000)
rmse.mean.2000 = sqrt(mean((df.out.2000$CL.mean - df.out.2000$Chloride.mgL)^2, na.rm = T)); print(rmse.mean.2000)
