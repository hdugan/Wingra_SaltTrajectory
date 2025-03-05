
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Model Fit ####
# RMSE relationship between observed and modeled chloride with a rolling chloride line 
p.model = ggplot(ss.1960) +
  geom_point(data = monthlyCl, aes(x = sampledate, y = Chloride.mgL),
             shape = 21, stroke = 0.2, size = 1, fill = 'darkred') +
  geom_path(aes(x = sampledate, y = CL), linetype = 1, linewidth = 0.5) + # model output
  # geom_point(aes(x = sampledate, y = CL), size = 0.5) + # model output
  ylab("Chloride"~(mg~L^-1)) + 
  annotate("text", x = as.Date('1940-01-01'), y = Inf, label = paste("RMSE =", round(rmse.monthly, 2)),
           hjust = 0, vjust = 1.5, size = 3, fontface = "bold") +  # Display RMSE on the plot
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank(), 
        legend.text = element_text(size =8),
        legend.position = c(0.15,0.80),
        legend.key.height = unit(0.12,'cm'))

ggsave('Figures_new/ModelOutput.png', width = 6.5, height = 3, dpi = 500)

# RMSE relationship between observed and modeled chloride with a rolling chloride line 
p.model.2000 = ggplot(df.out.2000) +
  geom_point(aes(x = sampledate, y = Chloride.mgL),
             shape = 21, stroke = 0.2, size = 1, fill = 'darkred') +
  geom_path(aes(x = sampledate, y = CL.model), linetype = 1, linewidth = 0.5) + # model output
  geom_path(aes(x = sampledate, y = CL.precip), color = '#80a8e8') +
  ylab("Chloride"~(mg~L^-1)) + 
  annotate("text", x = as.Date('2000-01-01'), y = Inf, label = paste("RMSE =", round(rmse.model.2000, 2)),
           hjust = 0, vjust = 1.5, size = 3, fontface = "bold") +  # Display RMSE on the plot
  annotate("text", x = as.Date('2000-01-01'), y = Inf, label = paste("RMSE =", round(rmse.precip.2000, 2)),
           hjust = 0, vjust = 3.5, size = 3, color = '#80a8e8', fontface = "bold") +  # Display RMSE on the plot
  xlim(as.Date('2000-01-01'), NA) +
  ylim(58,NA) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank(), 
        legend.text = element_text(size =8),
        legend.position = c(0.15,0.80),
        legend.key.height = unit(0.12,'cm')); p.model.2000

ggsave('Figures_new/ModelOutput_2000.png', width = 6.5, height = 3, dpi = 500)

p.model / p.model.2000 + plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8), plot.margin = unit(c(0.5,0.5,0.5,0.5), "mm"))
ggsave('Figures_new/ModelOutput_Combo.png', width = 6.5, height = 3, dpi = 500)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
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

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Violin plot ####
# future road salt reduction scenarios
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

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Reduction scenarios ####
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
ggplot(scenario_data_annual) +
  # plot future trajectory
  geom_ribbon(aes(ymin = CL.min, ymax = CL.max, x = wateryear, fill = scenario, group = scenario), alpha = 0.5) +
  geom_path(aes(x = wateryear, y = CL.mean, group = scenario), linetype = 1, linewidth = 0.3) +
  # plot historical data
  geom_point(data = annualCl, aes(x = wateryear, y = Chloride.mgL), color= 'red4',
             shape = 20, stroke = 0.2, size = 0.8) +
  # plot historical model output 
  geom_path(data = annual.1960, aes(x = wateryear, y = CL), linetype = 1, linewidth = 0.3) +
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

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# High/low precip years ####
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
ggsave("Figures_new/PrecipYears.png", width = 6, height = 3, units = "in", dpi = 500, bg = 'white')

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Rain and snow timeseries ####
p.rain = ggplot(met.year) +
  geom_col(aes(x = wateryear, y = totalPrecip), fill = '#0c5575') +
  ylab('Precip (m)') +
  xlim(1963, 2025) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

p.snow = ggplot(met.year) +
  geom_col(aes(x = wateryear, y = totalSnow), fill = '#a9d7eb') +
  ylab('Snow (m)') +
  xlim(1963, 2025) +
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

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
##### Plot Road Salt timeseries ####
p.roadsalt = roadSalt %>% select(wateryear, privatePublicRatio, TotalSalt_tons, ExtraPrivate_Estimate) %>% 
  rename(City = TotalSalt_tons, Private = ExtraPrivate_Estimate) %>% 
  pivot_longer(cols = 3:4, names_to = 'use', values_to = 'tons') %>% 
  mutate(use = factor(use, levels = c('Private', 'City'))) %>% 
  ggplot() +
  geom_col(aes(x = wateryear, y = tons * 0.0866, fill = use)) +
  geom_path(aes(x = wateryear, y = privatePublicRatio * 1000), linewidth = 0.2, linetype = 2) +
  geom_point(aes(x = wateryear, y = privatePublicRatio * 1000), size = 0.6) +
  scale_fill_manual(values = c('#dedaad','#c9b900'), labels = c('Private\n(estimate)','City\n(actual)')) +
  scale_y_continuous(
    name = expression("Road Salt " ~ ("tons")), 
    # name = "Road Salt (kilotons)", 
    sec.axis = sec_axis(~ . / 1000, name = "Private:Public ratio")
  ) + 
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.1,0.75),
        legend.title = element_blank(),
        legend.margin = margin(1, 1, 1, 1),  # Reduce margin (top, right, bottom, left)
        legend.key.size = unit(0.3, 'cm'),
        legend.box.background = element_rect(colour = "black"));  p.roadsalt

ggsave("Figures_new/roadSalt.png", width = 6, height = 3, units = "in", dpi = 500, bg = 'white')

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Plot chloride timeseries
p.chloride = ggplot(monthlyCl) +
  geom_path(aes(x = sampledate, y = Chloride.mgL)) +
  geom_point(aes(x = sampledate, y = Chloride.mgL), shape = 21, fill = 'lightblue3', stroke = 0.2) +
  ylab("Chloride"~(mg~L^-1)) + 
  xlim(as.Date('1963-01-01'), as.Date('2024-12-31')) +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank())

p.chloride / (p.roadsalt + xlim(1963,2025)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8)) 

ggsave("Figures_new/Cl_roadSalt.png", width = 6, height = 4, units = "in", dpi = 500, bg = 'white')

p.chloride /  (p.roadsalt + xlim(1963,2025)) / p.snow / p.rain + plot_layout(heights = c(1,1,0.6, 0.6)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8), plot.margin = unit(c(0.5,0.5,0.5,0.5), "mm"))

ggsave("Figures_new/Cl_roadSalt_met.png", width = 6, height = 5.5, units = "in", dpi = 500, bg = 'white')

