library(tidyverse)
library(lubridate)
library(deSolve)
library(MetBrewer)
library(patchwork)

### LOAD PRECIPITATION DATA
#convert precipitation from mm to m
dailyMet <- read_csv("~/Dropbox/currentprojects/Wingra_Lizzie/finalchangmet[42].csv") |> 
  rename(date = sampledate, precip_raw_m = precip_raw_mm) |> 
  mutate(date = as.Date(date), year = year(date)) |>
  #Filter this date based on time period of preference
  filter(date >= as.Date('1960-01-01'))
#Calculate yearly accumulation of precip
yearMet <- dailyMet |> 
  group_by(year) |>
  summarize(precip_raw_m = sum(precip_raw_m) / 1000)

###Subtracting ET from Precipitation to find runoff (assuming ET = 0.7 each year )
ET_Precip <- yearMet %>% 
  mutate(runoff_percent = (precip_raw_m - 0.70)/precip_raw_m) %>% 
  mutate(runoff_percent = if_else(runoff_percent < 0, 0, runoff_percent))

allLakes = read_csv('~/Dropbox/currentprojects/Wingra_Lizzie/MadisonLakes_chloride[38].csv') |> 
  filter(Lake == 'Wingra')

annualCl = allLakes %>% group_by(year = year(Date)) %>% 
  summarise(Chloride.mgL = mean(Chloride.mgL), Date = mean(Date))

ggplot(allLakes) +
  geom_point(aes(Date, Chloride.mgL), fill = '#F24D29', size = 0.8, shape = 21, stroke = 0.1) +
  geom_smooth(aes(Date, Chloride.mgL), color = '#F24D29', linewidth = 0.2, se = FALSE) +
  geom_path(data = annualCl, aes(x = Date, y = Chloride.mgL), color = 'black') +
  labs(y = "Chloride"~(mg~L^-1), title = 'Increasing chloride in Lake Wingra', 
       caption = 'Data from WI DNR, city of Madison, NTL-LTER') + 
  theme_bw(base_size = 10) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        axis.title.x = element_blank(),
        legend.position = c(0.25,0.85),
        legend.key.height = unit(0.1,'cm'),
        plot.title = element_text(size = 10),
        plot.caption = element_text(size = 6, color = '#F24D29'))

################# Rate function for salt model ################# 
#State variables are:
#SW - Mass of salt in the watershed, kg Cl-
#SL - Mass of salt in the lake, kg Cl-

#Parameters are:
#alpha - Application rate of road salt, kg (m road)-1 y-1
# 5 * 2.2 * 1609.34 # alpha = 5, converted to lb/miles
300 * 0.45 / 1609 #lb/mile converted to kg/m

#delta - Road density, m m-2
#A - Watershed area, m2 (note 1e6 m2 is 1 km2)
#p - Precipitation (net of evapotranspiration), m y-1
#phi - Salt yield of watershed per unit of precipitation, m-1
#V - Lake volume, m3
# roads_m = roads meters (wingra = 71.76 miles or 115486.53 meters)

# # Original ODE function
# dSalt <- function(time, state, pars) {
#   with(as.list(c(state,pars)), {
#     #Rate of change in Cl- in watershed, kg y-1
#     dSW <- alpha*roads_m - p*phi*SW
#     #Rate of change in Cl- in lake, kg y-1
#     dSL <- p*phi*SW - p*A*(1/V)*SL
#     #Return the rates of change
#     return(list(c(dSW,dSL)))
#   })
# }

# To modify the ODE function in R so that it accepts a dynamic variable for p
# rather than a static parameter, you'll need to adjust how p is passed into the
# function. Instead of including p as part of the pars argument, 
# introduce p as it's own data.frame 

# p_df: This is the data.frame containing time and p values. 
# It should have two columns: one for time and one for p.
# p <- p_df$p[which.min(abs(p_df$time - time))]: 
# Finds the value of p in p_df that corresponds to the closest time value in the data frame.

dSalt <- function(time, state, pars, p_df) {
  with(as.list(c(state, pars)), {
    # Extract the dynamic p based on the current time
    p <- p_df$p[which.min(abs(p_df$time - time))]
    
    # Rate of change in Cl- in watershed, kg y-1
    dSW <- alpha * roads_m - p * phi * SW
    # Rate of change in Cl- in lake, kg y-1
    dSL <- p * phi * SW - p * A * (1/V) * SL
    
    # Return the rates of change
    return(list(c(dSW, dSL)))
  })
}

# Here take the actual runoff values, but for the future randomly sample 
# normal distribution with mean and SD taken from actual values
p_df = ET_Precip %>% 
  mutate(time = row_number()) %>% 
  select(time, p = runoff_percent) %>% 
  bind_rows(data.frame(time = 64:200, 
                       p = rnorm(137, mean(ET_Precip$runoff_percent), sd(ET_Precip$runoff_percent))))
  
#Gather parameters
saltrate = 0.084 * 50 * 1.5 # An estimate salt rate
volume = 3677400
# 1.5 represents other contributions beyond city
# 0.084 is salting per event, and 50 is the number of times that the salt route is run
times = 1:200
inits = c(SW=0,SL=0)
pars <- c(phi = 0.08, alpha = saltrate, roads_m = 115486.53, A = 19826594, V = volume)

#Solve through time
ss.5 <- ode(inits,times,dSalt, parms = pars, p_df = p_df) |> 
  as_tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 5') %>% 
  mutate(CL = (SL/volume)*1000)

ggplot(ss.5) +
  geom_point(data = allLakes |> filter(Lake == 'Wingra'), aes(x = year(Date) - 1960, y = Chloride.mgL),
             fill = 'gold', shape = 21, stroke = 0.2) +
  geom_path(aes(x = time, y = CL, group = scenario, color = scenario), linetype = 1, linewidth = 1) +
  geom_path(data = annualCl, aes(x = year(Date) - 1960, y = Chloride.mgL), color = 'grey30') +
  scale_color_manual(values = rev(met.brewer("Hokusai2", 4))) +
  ylab("Road Salt Concentration"~(mg~Cl^"-"~L^-1)) +
  xlab('Year') +
  scale_x_continuous(breaks = seq(0,200, by = 50), labels = seq(1960, 1960+200, by = 50)) +
  theme_bw(base_size = 9) +
  theme(legend.position = "none", #c(0.15,0.87),
        legend.title = element_blank(),
        legend.key.size = unit(0.3, 'cm'),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.box.background = element_rect(fill = "transparent", colour = "transparent"))

# ggsave('figures/wingra.png', width = 4, height = 3, dpi = 500)
