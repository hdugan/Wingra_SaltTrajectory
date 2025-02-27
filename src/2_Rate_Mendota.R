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

# p_df: This is the data.frame containing time and p values. 
# It should have two columns: one for time and one for p.
# p <- p_df$p[which.min(abs(p_df$time - time))]: 
# Finds the value of p in p_df that corresponds to the closest time value in the data frame.

dSalt <- function(time, state, pars, p_df, salt_df) {
  with(as.list(c(state, pars)), {
    # Extract the dynamic p based on the current time
    p <- p_df$p[which.min(abs(p_df$time - time))]
    q = q_df$q[which.min(abs(p_df$time - time))]
    
    # Extract dynamic road salt based on the current time
    salt_input <- salt_df$salt_input[which.min(abs(salt_df$time - time))]
    # Update alpha parameter based dynamic salt input
    alpha <- salt_input
    
    # Assuming 82% of the salt applied goes into the watershed
    # Rate of change in Cl- in watershed, kg y-1
    # dSW <- 0.82*(alpha * roads_m) - p * phi * SW
    
    # Create variable that accounts for 0.7 is impervious surface, and ET of 0.5
    cl_d = 0.5 #chloride directly into the lake
    r_d = 0.20 #runoff directly into the lake
    # 
    # usep = p*(1 - r_d)
    # usep <- ifelse(usep < 0, 0, usep)
    
    dSW <- (salt_input*(1-cl_d)) - (p * phi * SW)
    
    # Assuming 18% of the salt applied goes directly into the lake 
    # Rate of change in Cl- in lake, kg y-1
    # dSL <- (0.18 * alpha * roads_m) + (p * phi * SW) - p * A * (1/V) * SL
    dSL <- (salt_input*cl_d) + (p * phi * SW) - (q * (1/V) * SL)
    # dSL <- (salt_input*1.5)  - (q * (1/V) * SL)
    # Return the rates of change
    return(list(c(dSW, dSL)))
  })
}

# Dataframes for model input for p and salt 
p_df = mendotaQ_monthly %>% ungroup() |> 
  mutate(time = row_number()) %>% 
  select(time, p = r)

q_df = mendotaQ_monthly %>% ungroup() |> 
  mutate(time = row_number()) %>% 
  select(time, q = Q_m3)

salt_df = saltuse %>% ungroup() |> 
  mutate(time = row_number()) %>% 
  select(time, salt_input = monthlyCl_kg_m)

# ggplot(p_df) + geom_path(aes(x = time, y = p))
# ggplot(salt_df) + geom_path(aes(x = time, y = salt_input))

#### Gather parameters #### 
# Lake areas (V)
mendota.volume = as.numeric(st_area(yaharaLakes |> filter(NAME == 'Lake Mendota')) * 0.092903 * 12.8)
# Watershed areas (A)
mendota.area = as.numeric(st_area(mendotaCat)* 0.092903)
# Road meters
mendota.roads = sum(as.numeric(st_length(mendotaRoads))) * 0.3048 # convert survey foot to meters

#times = 1:63
times = 1:204
inits = c(SW = 0.8e8, SL= 20349814.08) # Starting values in 2007 (estimates)
pars <- c(phi = 0.2, roads_m = mendota.roads, A = mendota.area, V = mendota.volume) 

# Run model from 1960 to 2024
ss.1960 <- ode(inits,times,dSalt, parms = pars, p_df = p_df, salt_df = salt_df) |> 
  as_tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(CL = (SL/as.numeric(mendota.volume))*1000) |> 
  mutate(sampledate = as.Date("2007-01-01") %m+% months(time))


# Calculate RMSE and r^2 between real and modeled chloride
comparison_data <- merge(monthlyCl |> filter(Lake == 'Mendota'), ss.1960, by = "sampledate", suffixes = c(".actual", ".modeled"))
rmse.real.model = sqrt(mean((comparison_data$Chloride.mgL - comparison_data$CL)^2))
r.sq2 = round(summary(lm(comparison_data$Chloride.mgL~comparison_data$CL))$r.squared, 2)

# RMSE relationship between observed and modeled chloride with a rolling chloride line 
ggplot(ss.1960) +
  geom_point(data = monthlyCl |> filter(Lake == 'Mendota'), aes(x = sampledate, y = Chloride.mgL, color = "Observed Chloride"),
             shape = 20, stroke = 0.2, size = 0.4) +
  geom_path(aes(x = sampledate, y = CL, color = "Modeled Chloride"), linetype = 1, linewidth = 0.5) + # model output
  geom_point(aes(x = sampledate, y = CL, color = "Modeled Chloride")) + # model output
  ylab("Chloride"~(mg~Cl^"-"~L^-1)) +
  xlab('Year') + 
  xlim(as.Date('2000-01-01'), NA) +
  annotate("text", x = as.Date('2010-01-01'), y = Inf, label = paste("RMSE =", round(rmse.real.model, 2)),
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +  # Display RMSE on the plot
  scale_color_manual(values = c("Observed Chloride" = "grey50", 
                                "Modeled Chloride" = "black", 
                                "Summer Annual Chloride" = "limegreen")) +
  theme_bw(base_size = 10) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size =8),
        legend.position = c(0.15,0.80),
        legend.key.height = unit(0.12,'cm'),
        plot.title = element_text(hjust = 0.5))

