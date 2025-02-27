
##########################################################################################
# RMSE relationship between observed and modeled chloride with a rolling chloride line 
ggplot(ss.1960) +
  geom_point(data = monthlyCl, aes(x = sampledate, y = Chloride.mgL),
             shape = 21, stroke = 0.2, size = 1, fill = 'darkred') +
  geom_path(aes(x = sampledate, y = CL), linetype = 1, linewidth = 0.5) + # model output
  geom_point(aes(x = sampledate, y = CL), size = 0.5) + # model output
  ylab("Chloride"~(mg~Cl^"-"~L^-1)) + 
  annotate("text", x = as.Date('1990-01-01'), y = Inf, label = paste("RMSE =", round(rmse.monthly, 2)),
           hjust = 1.1, vjust = 1.5, size = 3, fontface = "bold") +  # Display RMSE on the plot
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank(), 
        legend.text = element_text(size =8),
        legend.position = c(0.15,0.80),
        legend.key.height = unit(0.12,'cm'))

ggsave('Figures_new/ModelOutput.png', width = 6.5, height = 3, dpi = 500)

##########################################################################################
# R^2 Relationship between observed and modeled chloride 
r.sq2 = round(summary(lm(comparison_data$Chloride.mgL~comparison_data$CL))$r.squared, 2)

ggplot(comparison_data) +
  geom_abline(linetype = 2, width = 0.2) +
  geom_point(aes(x = Chloride.mgL, y = CL), stroke = 0.2, shape = 21, fill = 'lightblue4') +
  geom_smooth(aes(x = Chloride.mgL, y = CL), color = 'grey20', linewidth = 1, method = "lm", se = FALSE) +  
  annotate("text", x = Inf, y = Inf, label = paste("R² =", r.sq2), 
           hjust = 2, vjust = 2, size = 4, fontface = "bold") +
  ylab("Ovserved Chloride"~(mg~Cl^"-"~L^-1)) + 
  ylab("Modeled Chloride"~(mg~Cl^"-"~L^-1)) + 
  xlim(20,158)+ ylim(20,158) +
  theme_bw(base_size = 10)
ggsave('Figures_new/r2.png', width = 3, height = 3, dpi = 500)

##########################################################################################
# Violin plot of future road salt reduction scenarios
blue_shades <- c("#c5d3e0", "#9ecae1", "#4292c6", "#2171b5", "#084594")

p.violin = ggplot(scenario_data) +
  geom_violin(aes(x = reduction, y = CL.mean, fill = scenario),trim = FALSE, alpha = 0.7, linewidth = 0.3) +  
  geom_boxplot(aes(x = reduction, y = CL.mean, fill = scenario), width = 0.1, linewidth = 0.3, color = "black", alpha = 0.6, outlier.size = 0.5) + 
  scale_fill_manual(values = blue_shades) +
  theme_bw(base_size = 10) +
  ylab("Chloride Concentration"~(mg~Cl^"-"~L^-1)) +
  xlab("Scenario") +
  ylim(0,210) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.key.size = unit(0.3, 'cm'),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.box.background = element_rect(fill = "transparent", colour = "transparent")); p.violin

ggsave('Figures_new/Future_Violin.png', width = 6.5, height = 4, dpi = 500)

##########################################################################################
# Relationship between road salt reduction scenarios (0%, 25%, 50%, 75%, 100%)
p.futureTS = ggplot(scenario_data) +
  # plot future trajectory
  geom_ribbon(aes(ymin = CL.min, ymax = CL.max, x = sampledate, fill = scenario, group = scenario), alpha = 0.5) +
  geom_path(aes(x = sampledate, y = CL.mean, group = scenario), linetype = 1, linewidth = 0.3) +
  # plot historical data
  geom_point(data = monthlyCl, aes(x = sampledate, y = Chloride.mgL), color= 'red4',
             shape = 20, stroke = 0.2, size = 0.4) +
  # plot historical model output 
  geom_path(data = ss.1960, aes(x = sampledate, y = CL), linetype = 1, linewidth = 0.3) +
  # geom_path(data = annualCl, aes(x = year, y = Chloride.mgL, color = "Summer Annual Chloride")) +
  scale_color_manual(values = blue_shades) +
  scale_fill_manual(values = blue_shades) +
  ylab("Chloride Concentration"~(mg~Cl^"-"~L^-1)) +
  xlab('Year') +
  ylim(0,210) + xlim(as.Date('1963-01-01'), NA) +
  theme_bw(base_size = 10) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size =8),
        legend.position = c(0.2,0.80),
        legend.key.height = unit(0.12,'cm'),
        plot.title = element_text(hjust = 0.5));
ggsave('Figures_new/FutureTS_monthly.png', width = 6.5, height = 4, dpi = 500)

p.futureTS + p.violin + plot_layout(widths = c(1,0.4)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))
ggsave('Figures_new/FutureTS_combo.png', width = 6.5, height = 4, dpi = 500)

# Relationship between road salt reduction scenarios (0%, 25%, 50%, 75%, 100%)
ggplot(scenario_data_annaul) +
  # plot future trajectory
  geom_ribbon(aes(ymin = CL.min, ymax = CL.max, x = year, fill = scenario, group = scenario), alpha = 0.5) +
  geom_path(aes(x = year, y = CL.mean, group = scenario), linetype = 1, linewidth = 0.3) +
  # plot historical data
  geom_point(data = annualCl, aes(x = year, y = Chloride.mgL), color= 'red4',
             shape = 20, stroke = 0.2, size = 0.8) +
  # plot historical model output 
  geom_path(data = annual.1960, aes(x = year, y = CL), linetype = 1, linewidth = 0.3) +
  # geom_path(data = annualCl, aes(x = year, y = Chloride.mgL, color = "Summer Annual Chloride")) +
  scale_color_manual(values = blue_shades) +
  scale_fill_manual(values = blue_shades) +
  ylab("Road Salt Concentration"~(mg~Cl^"-"~L^-1)) +
  xlab('Year') +
  theme_bw(base_size = 10) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size =8),
        legend.position = c(0.15,0.80),
        legend.key.height = unit(0.12,'cm'),
        plot.title = element_text(hjust = 0.5));
ggsave('Figures_new/FutureTS_annual.png', width = 6.5, height = 4, dpi = 500)

##########################################################################################
# Relationship between high/low rain/snow
scalefactor = 80

ggplot(precip.snow) + 
  geom_point(data = annualCl, aes(x = wateryear, y = Chloride.mgL)) +
  geom_path(data = annualCl, aes(x = wateryear, y = Chloride.mgL)) +
  geom_point(data = precip.snow |> filter(Snow == 'Low Snow'), aes(x = wateryear, y = totalSnow*scalefactor, shape = Snow),
             color = '#ebe8dd', size = 2, stroke = 1.5) +
  geom_point(data = precip.snow |> filter(Snow == 'High Snow'), aes(x = wateryear, y = totalSnow*scalefactor, shape = Snow),
             color = '#a9d7eb', size = 2, stroke = 1.5) +
  geom_point(data = precip.snow |> filter(Rain == 'Low Precip'), aes(x = wateryear, y = totalPrecip*scalefactor, shape = Rain),
           fill = '#e0d9bc', size = 3) +
  geom_point(data = precip.snow |> filter(Rain == 'High Precip'), aes(x = wateryear, y = totalPrecip*scalefactor, shape = Rain),
            fill = '#0c5575', size = 3) +
  scale_y_continuous(
    name = expression("Road Salt Concentration"~(mg~Cl^"-"~L^-1)),
    sec.axis = sec_axis(~ ./scalefactor, name = "Precipitation (m)")) +
  xlab("Year") +
  theme_bw(base_size = 10) +
  scale_shape_manual(name = "Weather Type",
                     values = c("Low Precip" = 25, 
                                "High Precip" = 24, 
                                "Low Snow" = 8, 
                                "High Snow" = 8)) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 8),
        legend.position = c(0.25, 0.75),
        legend.key.height = unit(0.4, 'cm'),
        plot.title = element_text(hjust = 0.5))
ggsave("Figures_new/PrecipYears.png", width = 3, height = 3, units = "in", dpi = 500, bg = 'white')

##########################################################################################
# Plot rain and snow timeseries 
p.rain = ggplot(yearMet) +
  geom_col(aes(x = wateryear, y = totalPrecip), fill = '#0c5575') +
  ylab('Total Precipitaiton (m)') +
  annotate("text", x = -Inf, y = Inf, label = '💧', size = 8, vjust = 1.5, hjust = -0.7) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

p.snow = ggplot(yearMet) +
  geom_col(aes(x = wateryear, y = totalSnow), fill = '#a9d7eb') +
  ylab('Total Snow (m)') +
  annotate("text", x = -Inf, y = Inf, label = '❄️', size = 8, vjust = 1.5, hjust = -0.7) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

p.rain/p.snow
ggsave("Figures_new/metTS.png", width = 6, height = 3, units = "in", dpi = 500, bg = 'white')

# # Compare to Dane County airport for gut check 
# p.rain2 = ggplot(airport.yearMet) +
#   geom_col(aes(x = wateryear, y = totalPrecip), fill = '#0c5575') +
#   ylab('Total Precipitaiton (m)') +
#   theme_bw(base_size = 10) +
#   theme(axis.title.x = element_blank())
# 
# p.snow2 = ggplot(airport.yearMet) +
#   geom_col(aes(x = wateryear, y = totalSnow), fill = '#a9d7eb') +
#   ylab('Total Snow (m)') +
#   theme_bw(base_size = 10) +
#   theme(axis.title.x = element_blank())
# 
# (p.rain + p.snow) / (p.rain2 + p.snow2)

  