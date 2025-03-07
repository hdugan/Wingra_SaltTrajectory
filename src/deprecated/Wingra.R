library(tidyverse)
library(lubridate)
library(deSolve)
library(MetBrewer)
library(patchwork)

allLakes = read_csv('datain/MadisonLakes_chloride.csv') |> 
  filter(Lake == 'Wingra')

ggplot(allLakes) +
  geom_point(aes(Date, Chloride.mgL), fill = '#F24D29', size = 0.8, shape = 21, stroke = 0.1) +
  geom_smooth(aes(Date, Chloride.mgL), color = '#F24D29', linewidth = 0.2, se = FALSE) +
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

dSalt <- function(time, state, pars) {
  with(as.list(c(state,pars)), {
    #Rate of change in Cl- in watershed, kg y-1
    dSW <- alpha*roads_m - p*phi*SW
    #Rate of change in Cl- in lake, kg y-1
    dSL <- p*phi*SW - p*A*(1/V)*SL
    #Return the rates of change
    return(list(c(dSW,dSL)))
  })
}

#Gather parameters
saltrate = 0.084 * 50 * 1.5 # An estimate salt rate
volume = 3677400
# 1.5 represents other contributions beyond city
# 0.084 is salting per event, and 50 is the number of times that the salt route is run
times = 1:200
inits = c(SW=0,SL=0)
pars <- c(phi = 0.1, alpha = saltrate, roads_m = 115486.53, A = 19826594, p = 0.25, V = volume)

#Solve through time
ss.5 <- ode(inits,times,dSalt,pars) |> 
  as_tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 5')

#Solve through time
times = 60:200
inits = c(SW = ss.5[60,]$SW, SL = ss.5[60,]$SL)

#Solve through time for 1/2 the rate of salting
pars[2] = saltrate / 2
ss.4 = ode(inits,times,dSalt,pars) |> 
  as_tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 2')
#Solve through time for 3/4 rate of salting 
pars[2] = saltrate * 3/4
ss.2 = ode(inits,times,dSalt,pars) |> 
  as.tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 4')
#Solve through time for zero salting 
pars[2] = 0
ss.0 = ode(inits,times,dSalt,pars) |> 
  as.tibble() |> mutate_all(list(as.numeric)) |> 
  mutate(scenario = 'alpha = 0')

ss.out = ss.5 |> bind_rows(ss.4) |> bind_rows(ss.2) |> bind_rows(ss.0) |> 
  mutate(CL = (SL/volume)*1000)

ggplot(ss.out) +
  geom_point(data = allLakes |> filter(Lake == 'Wingra'), aes(x = year(Date) - 1960, y = Chloride.mgL),
             fill = 'gold', shape = 21, stroke = 0.2) +
  geom_path(aes(x = time, y = CL, group = scenario, color = scenario), linetype = 1, linewidth = 1) +
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
