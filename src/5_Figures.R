################# Figures ################# 
# General figure of increasing chloride in all Yahara lakes
YaharaLakes$Date <- as.Date(YaharaLakes$Date, format = "%m/%d/%Y")
ggplot(YaharaLakes) +
  geom_point(aes(as.Date(Date), Chloride.mgL, fill = Lake), size = 1, shape = 21, stroke = 0.1, color = "transparent") +
  geom_smooth(method = "loess", aes(as.Date(Date), Chloride.mgL, color = Lake), se = FALSE, linewidth = 1) +
  labs(x = "Year",y = "Chloride"~(mg/L)) + 
  scale_color_manual(values = c("#CC79A7", "#D55E00", "#F0E442", "#0072B2", "#009E73"),
                     labels = c("Lake Kegonsa", "Lake Mendota", "Lake Monona", "Lake Waubesa", "Lake Wingra")) +
  scale_fill_manual(values = c("#CC79A7", "#D55E00", "#F0E442", "#0072B2", "#009E73"),
                    labels = c("Lake Kegonsa", "Lake Mendota", "Lake Monona", "Lake Waubesa", "Lake Wingra")) +
  scale_x_date(date_labels = "%Y") +
  theme_bw(base_size = 10) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 15),
        legend.position = c(0.20,0.80),
        legend.key.height = unit(0.1,'cm'),
        plot.title = element_text(hjust = 0.5));
ggsave("Figures/YaharaLakes.png", width = 6.5, height = 4, units = "in", dpi = 500, bg = 'white')

#Road salt application from City of Madison
ggplot(roadSalt) +
  geom_col(aes(x = year,y = salt_kg), fill = "#DDCC77") +
  labs(x = "Year",y = "Salt"~(Kg)) + 
  theme_bw(base_size = 10)
ggsave("Figures/roadSalt.png", width = 6.5, height = 4, units = "in", dpi = 500, bg = 'white')

# Yearly accumulated snow plot
snow.cum <- ggplot(yearMet, aes(year, snow_raw_m)) +
  geom_line(color = "seashell3", linewidth = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = snow_raw_m), fill = "seashell3", alpha = 0.8) +  # Shaded area under the line
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  xlab("Year") +
  ylab("Total Snowfall (m)") +
  theme_bw(base_size = 10)
snow.cum

# Yearly accumulated precip plot
precip.cum <- ggplot(yearMet, aes(year, precip_raw_m)) +
  geom_line(color = "skyblue2", size = 0.5) +
  geom_ribbon(aes(ymin = 0, ymax = precip_raw_m), fill = "skyblue2", alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  xlab("Year") +
  ylab("Total Precipitation (m)") +
  theme_bw(base_size = 10)
precip.cum

# Join plots for manuscript
precip.cum + snow.cum + plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))
ggsave("Figures/precip_snow.png", width = 6.5, height = 4, units = "in", dpi = 500, bg = 'white')
 
# R^2 relationship between historical road salt use and snowfall 
plot_snow <- ggplot(salt_snow) +
geom_point(aes(x = snow_raw_m, y = salt_tons), fill = 'black') +
  geom_smooth(aes(x = snow_raw_m, y = salt_tons), color = 'limegreen', linewidth = 1, method = "lm", se = FALSE) +  
  geom_text(aes(x = snow_raw_m, y = salt_tons, label = year), size = 2, nudge_x = 0.15) +
  annotate("text", x = Inf, y = Inf, label = paste("R² =", r.sq1), 
           hjust = 2, vjust = 2, size = 4, fontface = "bold") +
  xlab("Accumulated Snowfall (m)") +
  ylab("Salt (tons)")+
  theme_bw(base_size = 10) 

# R^2 relationship between historical road salt use and precipitation
plot_precip <- ggplot(salt_snow) +  # Assuming salt_precip is your dataset for precipitation
  geom_point(aes(x = precip_raw_m, y = salt_tons), color = 'black') +
  geom_smooth(aes(x = precip_raw_m, y = salt_tons), color = 'limegreen', linewidth = 1, method = "lm", se = FALSE) +  
  annotate("text", x = Inf, y = Inf, label = paste("R² =", r.sq7), 
           hjust = 2, vjust = 2, size = 4, fontface = "bold") +
  xlab("Accumulated Precipitation (m)") +
  ylab("Salt (tons)") +
  theme_bw(base_size = 10)

salt_snow_precip <- plot_snow + plot_precip + 
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))
ggsave('Figures/salt_snow_precip.png', width = 10, height = 6, dpi = 500)


# RMSE relationship between observed and modeled chloride with a rolling chloride line 
ggplot(ss.1960) +
  geom_point(data = allLakes |> filter(Lake == 'Wingra'), aes(x = year(Date), y = Chloride.mgL, color = "Observed Chloride"),
             shape = 20, stroke = 0.2, size = 0.4) +
  geom_path(aes(x = year, y = CL, color = "Modeled Chloride"), linetype = 1, linewidth = 0.5) + # model output
  geom_path(data = annualCl, aes(x = year, y = Chloride.mgL, color = "Summer Annual Chloride"), linetype = 1, linewidth = 0.5) +
  ylab("Chloride"~(mg~Cl^"-"~L^-1)) +
  xlab('Year') + 
  xlim(1960, 2024) +
  annotate("text", x = Inf, y = Inf, label = paste("RMSE =", round(rmse.real.model, 2)), 
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +  # Display RMSE on the plot
  scale_color_manual(values = c("Observed Chloride" = "grey50", 
                                "Modeled Chloride" = "black", 
                                "Summer Annual Chloride" = "limegreen")) +
  theme_bw(base_size = 10) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size =8),
        legend.position = c(0.15,0.80),
        legend.key.height = unit(0.12,'cm'),
        plot.title = element_text(hjust = 0.5));
ggsave('Figures/ss.1960.png', width = 6.5, height = 4, dpi = 500)

# R^2 Relationship between observed and modeled chloride 
ggplot(comparison_data) +
  geom_point(aes(x = Chloride.mgL, y = CL), fill = 'lightblue4') +
  geom_smooth(aes(x = Chloride.mgL, y = CL), color = 'limegreen', linewidth = 1, method = "lm", se = FALSE) +  
  annotate("text", x = Inf, y = Inf, label = paste("R² =", r.sq2), 
           hjust = 2.0, vjust = 2, size = 4, fontface = "bold") +
  xlab("Observed Chloride Concentrations (mg/L)") +
  ylab("Modeled Chloride Concentrations (mg/L)")+
  theme_bw(base_size = 10)
ggsave('Figures/comparison_data.png', width = 6.5, height = 4, dpi = 500)

# Histogram of future road salt reduction scenarios
ggplot(scenario_data)+
  geom_violin(aes(x = scenario, y = CL.mean, fill = scenario),trim = FALSE, alpha = 0.7) +  
  geom_boxplot(aes(x = scenario, y = CL.mean, fill = scenario), width = 0.1, color = "black", alpha = 0.6) + 
  scale_fill_manual(values = c("No Reduction" = "lightblue2", "25% Reduction" = "lightblue3", "50% Reduction" = "lightblue4", "75% Reduction" = "grey50", "100% Reduction" = "grey30")) +
  theme_bw(base_size = 10) +
  ylab("Mean Chloride Concentration (mg/L)") +
  xlab("Scenario") +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.key.size = unit(0.3, 'cm'),
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.box.background = element_rect(fill = "transparent", colour = "transparent"))
ggsave('Figures/scenario_data.png', width = 6.5, height = 4, dpi = 500)


summary_stats <- scenario_data %>%
  group_by(scenario) %>%
  summarize(
    mean_CL = mean(CL.mean, na.rm = TRUE),
    max_CL = max(CL.mean, na.rm = TRUE),
    min_CL = min(CL.mean, na.rm = TRUE),
    range_CL = max(CL.mean, na.rm = TRUE) - min(CL.mean, na.rm = TRUE)
  )

# Set factor levels for consistent legend order
ss.future$Scenario <- factor("No Reduction", 
                             levels = c("No Reduction", "25% Reduction", "50% Reduction", 
                                        "75% Reduction", "100% Reduction"))
ss.future.25$Scenario <- factor("25% Reduction", 
                                levels = c("No Reduction", "25% Reduction", "50% Reduction", 
                                           "75% Reduction", "100% Reduction"))
ss.future.50$Scenario <- factor("50% Reduction", 
                                levels = c("No Reduction", "25% Reduction", "50% Reduction", 
                                           "75% Reduction", "100% Reduction"))
ss.future.75$Scenario <- factor("75% Reduction", 
                                levels = c("No Reduction", "25% Reduction", "50% Reduction", 
                                           "75% Reduction", "100% Reduction"))
ss.future.100$Scenario <- factor("100% Reduction", 
                                 levels = c("No Reduction", "25% Reduction", "50% Reduction", 
                                            "75% Reduction", "100% Reduction"))

# Relationship between road salt reduction scenarios (0%, 25%, 50%, 75%, 100%)
ggplot(ss.future) +
  # plot future trajectory
  geom_ribbon(aes(ymin = CL.min, ymax = CL.max, x = year, fill = Scenario), alpha = 0.5) +
  geom_path(aes(x = year, y = CL.mean), linetype = 1, linewidth = 0.6) +
  # plot future trajectory with 1/4 
  geom_ribbon(data = ss.future.25, aes(ymin = CL.min, ymax = CL.max, x = year, fill = Scenario), alpha = 0.7) +
  geom_path(data = ss.future.25, aes(x = year, y = CL.mean), linetype = 1, linewidth = 0.6) +
  # plot future trajectory with 1/2 reduced salt use
  geom_ribbon(data = ss.future.50, aes(ymin = CL.min, ymax = CL.max, x = year, fill = Scenario), alpha = 0.7) +
  geom_path(data = ss.future.50, aes(x = year, y = CL.mean), linetype = 1, linewidth = 0.6) +
  # plot future trajectory with 1/4 reduced salt use
  geom_ribbon(data = ss.future.75, aes(ymin = CL.min, ymax = CL.max, x = year, fill = Scenario), alpha = 0.7) +
  geom_path(data = ss.future.75, aes(x = year, y = CL.mean), linetype = 1, linewidth = 0.6) +
  # plot future trajectory with 100% reduced salt use
  geom_ribbon(data = ss.future.100, aes(ymin = CL.min, ymax = CL.max, x = year, fill = Scenario), alpha = 0.7) +
  geom_path(data = ss.future.100, aes(x = year, y = CL.mean), linetype = 1, linewidth = 0.6) +
  # plot historical data
  geom_point(data = allLakes |> filter(Lake == 'Wingra'), aes(x = year(Date), y = Chloride.mgL,  color = "Observed Chloride"),
             shape = 20, stroke = 0.2, size = 0.4) +
  # plot historical model output 
  geom_path(data = ss.1960, aes(x = year, y = CL, color = "Modeled Chloride"), linetype = 1, linewidth = 0.8) +
  geom_path(data = annualCl, aes(x = year, y = Chloride.mgL, color = "Summer Annual Chloride")) +
  # color mapping and labels
  scale_color_manual(name = "Scenario",
                     values = c("No Reduction" = "lightblue2",
                                "25% Reduction" = "lightblue3",
                                "50% Reduction" = "lightblue4",
                                "75% Reduction" = "grey50",
                                "100% Reduction" = "grey30",
                                "Modeled Chloride" = "black",
                                "Summer Annual Chloride" = "limegreen",
                                "Observed Chloride" = "grey70")) +
  
  scale_fill_manual(name = "Scenario",
                    values = c("No Reduction" = "lightblue2",
                               "25% Reduction" = "lightblue3",
                               "50% Reduction" = "lightblue4",
                               "75% Reduction" = "grey50",
                              "100% Reduction" = "grey30")) +
  ylab("Road Salt Concentration"~(mg~Cl^"-"~L^-1)) +
  xlab('Year') +
  xlim(1960,2160) +
  theme_bw(base_size = 10) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size =8),
        legend.position = c(0.15,0.80),
        legend.key.height = unit(0.12,'cm'),
        plot.title = element_text(hjust = 0.5));
ggsave('Figures/ss.future.png', width = 6.5, height = 4, dpi = 500)

# Relationship between high/low rain/snow
ggplot(precip.snow, aes(x = year)) + 
  geom_point(data = precip.snow %>% filter(Rain == "Low Precip"), aes(y = precip_raw_m*scale_factor_combined, fill = "Low Precip(<0.623m)", shape = "Low Precip(<0.623m)"), size = 4) +  
  geom_point(data = precip.snow %>% filter(Rain == "High Precip"), aes(y = precip_raw_m*scale_factor_combined, fill = "High Precip(>1.18m)", shape = "High Precip(>1.18m)"), size = 4) +  
  geom_point(data = precip.snow %>% filter(Snow == "Low Snow"), aes(y = snow_raw_m*scale_factor_combined, fill = "Low Snow(<0.65m)", shape = "Low Snow(<0.65m)"), size = 4) +  
  geom_point(data = precip.snow %>% filter(Snow == "High Snow"), aes(y = snow_raw_m*scale_factor_combined, fill = "High Snow(>1.85m)", shape = "High Snow(>1.85m)"), size = 4) +  
#  # plot historical raw data
  geom_point(data = allLakes |> filter(Lake == 'Wingra'), aes(x = year(Date), y = Chloride.mgL, color = "Observed Chloride"),
             shape = 20, stroke = 0.2, size = 0.8) +
  # plot historical annual chloride
  geom_path(data = annualCl, aes(x = year, y = Chloride.mgL, color = "Summer Annual Chloride")) +
  scale_y_continuous(
    name = "Chloride (mg/L)",
    sec.axis = sec_axis(~ ./scale_factor_combined, name = "Precipitation (m)")) +
  xlab("Year") +
  xlim(1960,2024) +
  theme_bw(base_size = 10) +
  scale_shape_manual(name = "Weather Type",
                     values = c("Low Precip(<0.623m)" = 25, 
                                "High Precip(>1.18m)" = 24, 
                                "Low Snow(<0.65m)" = 25, 
                                "High Snow(>1.85m)" = 24)) +
  scale_fill_manual(name = "Weather Type",
                    values = c("Low Precip(<0.623m)" = "skyblue1",
                               "High Precip(>1.18m)" = "skyblue3",
                               "Low Snow(<0.65m)" = "seashell1",
                               "High Snow(>1.85m)" = "seashell3")) +
  scale_color_manual(name = "Chloride in Lake Wingra", 
                     values = c("Summer Annual Chloride" = "limegreen",
                                "Observed Chloride" = "grey70")) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 10),
        legend.position = c(0.25, 0.75),
        legend.key.height = unit(0.12, 'cm'),
        plot.title = element_text(hjust = 0.5))
ggsave("Figures/precip.snow.png", width = 6.5, height = 5, units = "in", dpi = 500, bg = 'white')

# Stacked plot comparing RMSE's w/ mean, 10-year salt, + 10-year precip scenarios 
# Plot for mean scenario
  
ss.mean.plot <- ggplot(ss.mean, aes(x = year, y = CL)) +
  geom_point(data = allLakes |> filter(Lake == 'Wingra'), aes(x = year(Date), y = Chloride.mgL,  color = "Observed Chloride"),
             shape = 20, stroke = 0.2, size = 0.4, color = "grey50") +
  geom_path(data = ss.1960, aes(x = year, y = CL, color = "Modeled Chloride"), linetype = 1, linewidth = 0.3, color = "black") + # model output
  geom_line(linetype = 1, linewidth = 0.5, color = "#CC79A7") +
  xlab("Year") +
  ylab("Chloride"~(mg~Cl^"-"~L^-1)) +
  ggtitle("10-year mean of precipitation, 10-year mean of road salt scenario") +
  xlim(1960, 2024) +
  annotate("text", x = 2024, y = 15, label = paste("RMSE =", round(rmse.mean, 2)), 
             hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +  # Display RMSE on the plot
  theme_bw(base_size = 10)
ggsave(filename = "Figures/ss_mean_plot.png", plot = ss.mean.plot, width = 10, height = 5, dpi = 500)

# Plot for 10-year salt scenario 
ss.precip.plot <- ggplot(ss.precip, aes(x = year, y = CL)) +
  geom_point(data = allLakes |> filter(Lake == 'Wingra'), aes(x = year(Date), y = Chloride.mgL,  color = "Observed Chloride"),
             shape = 20, stroke = 0.2, size = 0.4, color = "grey50") +
  geom_path(data = ss.1960, aes(x = year, y = CL, color = "Modeled Chloride"), linetype = 1, linewidth = 0.3, color = "black") + # model output
  geom_line(linetype = 1, linewidth = 0.5, color = "#D55E00") +
  xlab("Year") +
  ylab("Chloride"~(mg~Cl^"-"~L^-1)) +
  ggtitle("Actual precipitation, 10-year mean of road salt scenario") +
  xlim(1960, 2024) +
  annotate("text", x = 2024, y = 15, label = paste("RMSE =", round(rmse.precip, 2)), 
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +  # Display RMSE on the plot
  theme_bw(base_size = 10)
ggsave(filename = "Figures/ss_precip_plot.png", plot = ss.precip.plot, width = 10, height = 5, dpi = 500)

# Plot for 10-year precipitation scenario
ss.salt.plot <- ggplot(ss.salt, aes(x = year, y = CL)) +
  geom_point(data = allLakes |> filter(Lake == 'Wingra'), aes(x = year(Date), y = Chloride.mgL,  color = "Observed Chloride"),
             shape = 20, stroke = 0.2, size = 0.4, color = "grey50") +
  geom_path(data = ss.1960, aes(x = year, y = CL, color = "Modeled Chloride"), linetype = 1, linewidth = 0.3, color = "black") + # model output
  geom_line(linetype = 1, linewidth = 0.5, color = "#0072B2") +
  xlab("Year") +
  ylab("Chloride"~(mg~Cl^"-"~L^-1)) +
  ggtitle("10-year mean of precipitation, actual road salt scenario") + 
  xlim(1960, 2024) +
  annotate("text", x = 2024, y = 15, label = paste("RMSE =", round(rmse.salt, 2)), 
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +  # Display RMSE on the plot
  theme_bw(base_size = 10) 
ggsave(filename = "Figures/ss_salt_plot.png", plot = ss.salt.plot, width = 10, height = 5, dpi = 500)

# Combine three plot
combined_rmse <- (ss.mean.plot) / (ss.precip.plot) / (ss.salt.plot) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ') ') &
  theme(plot.tag = element_text(size = 8))

combined_rmse
ggsave('Figures/combined_rmse.png', width = 6.5, height = 8, dpi = 500)

combined_data <- rbind(
  data.frame(year = ss.precip$year, CL = ss.precip$CL, Scenario = "Historical precip, 10-year road salt"),
  data.frame(year = ss.salt$year, CL = ss.salt$CL, Scenario = "10-year mean precip, historical road salt"),
  data.frame(year = ss.mean$year, CL = ss.mean$CL, Scenario = "10-year mean of both precip and road salt")
)

# Create the combined plot
combined_plot <- ggplot() +
  # Observed data points
  geom_point(
    data = allLakes |> filter(Lake == 'Wingra'),
    aes(x = year(Date), y = Chloride.mgL, color = "Observed Chloride"),
    shape = 20, stroke = 0.2, size = 0.4, alpha = 0.7
  ) +
  # Modeled historical chloride
  geom_path(
    data = ss.1960,
    aes(x = year, y = CL, color = "Modeled Chloride"),
    linetype = 1, linewidth = 0.3
  ) +
  # Scenario lines
  geom_line(
    data = combined_data,
    aes(x = year, y = CL, color = Scenario),
    linetype = 1, linewidth = 0.5
  ) +
  # Labels and aesthetics
  xlab("Year") +
  ylab(expression("Chloride"~(mg~Cl^-1~L^-1))) +
  xlim(1960, 2024) +
  theme_bw(base_size = 10) +
  # Custom legend and colors
  scale_color_manual(
    values = c(
      "Observed Chloride" = "grey50",
      "Modeled Chloride" = "black",
      "Historical precip, 10-year road salt" = "#D55E00", 
      "10-year mean precip, historical road salt" = "#CC79A7",
      "10-year mean of both precip and road salt" = "#0072B2"
    ),
    name = "Legend"
  ) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 10),
        legend.position = c(0.25, 0.75),
        legend.key.height = unit(0.12, 'cm'),
        plot.title = element_text(hjust = 0.5))
ggsave('Figures/combined_data.png', width = 10, height = 5, dpi = 500)
