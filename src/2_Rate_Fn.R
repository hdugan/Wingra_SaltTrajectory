################# Rate function for salt model ################# 
#State variables are:
#SW - Mass of salt in the watershed, kg Cl-
#SL - Mass of salt in the lake, kg Cl-

#Parameters are:
#alpha - Application rate of road salt, kg (m road)-1 y-1

#delta - Road density, m m-2
#A - Watershed area, m2 (note 1e6 m2 is 1 km2)
#p - Precipitation (net of evapotranspiration), m y-1
#phi - Salt yield of watershed per unit of precipitation, m-1
#V - Lake volume, m3
# roads_m = roads meters (wingra = 71.76 miles or 115486.53 meters)

### From Lake Wingra Watershed Management Plan
# https://www.cityofmadison.com/engineering/documents/projects/LakeWingraWMP-Section3-Infiltration.pdf
# Estimates based on GIS analysis and City databases indicate that of the 3,636 acres within the
# watershed, 2,550 acres (70 percent) are pervious and 1,086 acres (30 percent) are impervious. Of
# these pervious areas, 1,790 acres (70 percent) are nonresidential land use including multifamily
# residential and 760 (30 percent) are residential (single-family and duplex) land use.

# Original function with dynamic inputs 
dSalt_OG <- function(time, state, pars, p_df, salt_df) {
  with(as.list(c(state, pars)), {
    # Extract the dynamic p based on the current time
    p <- p_df$p[which.min(abs(p_df$time - time))]
    p = p - 0.67
    p <- ifelse(p < 0, 0, p)
    
    # Extract dynamic road salt based on the current time
    salt_input <- salt_df$salt_input[which.min(abs(salt_df$time - time))]
    # Update alpha parameter based dynamic salt input
    alpha <- salt_input

    dSW <- (alpha * roads_m) - p * phi * SW
    # Rate of change in Cl- in lake, kg y-1
    dSL <- p * phi * SW - p * A * (1/V) * SL
    
    # Return the rates of change
    return(list(c(dSW, dSL)))
  })
}


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
    
    # Extract dynamic road salt based on the current time (in kg)
    salt_input <- salt_df$salt_input[which.min(abs(salt_df$time - time))]
    salt_input = salt_input
    # # Update alpha parameter based dynamic salt input
    # alpha <- salt_input
    
    # Assuming 82% of the salt applied goes into the watershed
    # Rate of change in Cl- in watershed, kg y-1
    # dSW <- 0.82*(alpha * roads_m) - p * phi * SW
    
    # Create variable that accounts for 0.7 is impervious surface, and ET of 0.5
    # cl_d = 0.3 #0.345 #chloride directly into the lake 
    # r_d = 0.1 #runoff directly into the lake

    usep = p*(1 - r_d)
    usep <- ifelse(usep < 0, 0, usep)
    
    dSW <- (1-cl_d)*(salt_input) - usep * phi * SW
    
    # Assuming 18% of the salt applied goes directly into the lake 
    # Rate of change in Cl- in lake, kg y-1
    # dSL <- (0.18 * alpha * roads_m) + (p * phi * SW) - p * A * (1/V) * SL
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
# Road meters
wingra.roads = sum(as.numeric(st_length(wingraRoads))) * 0.3048 # convert survey foot to meters

#times = 1:63
times = 1:737
inits = c(SW = 2e6, SL=51483.6) # Starting values in 1963 (estimate for watershed)
pars <- c(cl_d = 0.17, r_d = 0.2, phi = 0.2, A = wingra.area, V = wingra.volume) 
# pars <- c(cl_d = 0.25, r_d = 0.2, phi = 0.18, A = wingra.area, V = wingra.volume) 


# Run model from 1960 to 2024
ss.1960 <- ode(inits,times,dSalt, parms = pars, p_df = p_df, salt_df = salt_df) |> 
  as_tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(CL = (SL/as.numeric(wingra.volume))*1000) |> 
  mutate(sampledate = as.Date("1963-06-01") %m+% months(time))

# Calculate RMSE and r^2 between real and modeled chloride
# Join to observed values 
comparison_data = ss.1960 |> left_join(monthlyCl) |>  
  mutate(Chloride.mgL_6m = rollmean(Chloride.mgL, k = 4, fill = NA, align = "right")) |> 
  mutate(CL_6m = rollmean(CL, k = 4, fill = NA, align = "right")) |> 
  mutate(resids = abs(Chloride.mgL_6m - CL_6m)) 

rmse.monthly = sqrt(mean((comparison_data$Chloride.mgL - comparison_data$CL)^2, na.rm = T))
rmse.6month = sqrt(mean((comparison_data$Chloride.mgL_6m - comparison_data$CL_6m)^2, na.rm = T))
# r.sq2 = round(summary(lm(comparison_data$Chloride.mgL~comparison_data$CL))$r.squared, 2)

