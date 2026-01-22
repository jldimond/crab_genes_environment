# ============================================================
# 09_prey.R
# Purpose: Plotting larval crab and small crustacean biomass
# from Puget Sound Zooplankton Monitoring Program
# ============================================================

# Load libraries
library(ggplot2)
library(dplyr)
library(readr)

# Read data 
# data source: https://www.pugetsoundinfo.wa.gov/FileResource/DisplayResource/279911c0-2005-461e-b5df-a0f52f947cda
data <- read_csv("data/seasonal_average_simple_means.csv")

# Filter for target regions and types
filtered_data <- data %>%
  filter(region %in% c("NWA", "PS"),
         type %in% c("crab", "small_crust"),
         year_for_seasonal_metrics %in% c("2014", "2015", "2016", "2017",
                                          "2018", "2019", "2020", "2021",
                                          "2022", "2023"))

# Summarize mean biomass by year, season, type, and region
summary_data <- filtered_data %>%
  group_by(region, type, year_for_seasonal_metrics, season) %>%
  summarize(mean_biomass = mean(mean_biomass_untransformed, na.rm = TRUE), .groups = "drop")

# Ensure season order
summary_data$season <- factor(summary_data$season, levels = c("Spring", "Summer", "Fall", "Winter"))

# Plot
prey <- ggplot(summary_data, aes(x = season, 
                              y = mean_biomass, 
                              color = as.factor(year_for_seasonal_metrics), 
                              group = year_for_seasonal_metrics)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ region + type, scales = "free_y") +
  scale_color_viridis_d(name = "Year") +
  theme_classic() +
  labs(x = "Season",y = expression(paste("Biomass (mg C m"^{-3}, ")"))) +
  theme(panel.border = element_rect(linetype = "solid", linewidth = 0.75, fill = NA),
        text = element_text(size = 14)) 

ggsave("results/figures/prey_biomass.pdf", prey, width = 15, height = 15)
