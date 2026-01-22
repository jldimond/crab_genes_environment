# ============================================================
# 07_CTD_profiles.R
# Purpose: Plot CTD profiles from State of Washington
#          Department of Natural Resources Monitoring Stations
# ============================================================

library(dplyr)
library(ggplot2)
library(cowplot)

# Data from WA Dept. of Ecology
#https://ecology.wa.gov/research-data/monitoring-assessment/puget-sound-and-marine-monitoring

# Strait of Juan de Fuca, SJF001
SJF001 <- as.data.frame(read.csv("data/SJF001_EIMContinuousDepthSeriesData_2025Aug28_544088_Part1.csv", header = T))
# Strait of Georgia, GRG002 
GRG002 <- as.data.frame(read.csv("data/GRG002_EIMContinuousDepthSeriesData_2025Aug28_21058.csv", header = T))

SJF001$Field_Collection_Date <- mdy(SJF001$Field_Collection_Date)

SJF001_May2023 <- SJF001 %>%
  filter(Result_Parameter_Name == "Temperature, water",
         year(Field_Collection_Date) == 2023,
         month(Field_Collection_Date) == 5)

SJF001_Jun2023 <- SJF001 %>%
  filter(Result_Parameter_Name == "Temperature, water",
         year(Field_Collection_Date) == 2023,
         month(Field_Collection_Date) == 6)

SJF001_July2023 <- SJF001 %>%
  filter(Result_Parameter_Name == "Temperature, water",
         year(Field_Collection_Date) == 2023,
         month(Field_Collection_Date) == 7)

SJF001_Aug2023 <- SJF001 %>%
  filter(Result_Parameter_Name == "Temperature, water",
         year(Field_Collection_Date) == 2023,
         month(Field_Collection_Date) == 8)

SJF001_Sep2023 <- SJF001 %>%
  filter(Result_Parameter_Name == "Temperature, water",
         year(Field_Collection_Date) == 2023,
         month(Field_Collection_Date) == 9)

# Strait of Georgia, GRG002 

GRG002$Field_Collection_Date <- mdy(GRG002$Field_Collection_Date)

GRG002_May2023 <- GRG002 %>%
  filter(Result_Parameter_Name == "Temperature, water",
         year(Field_Collection_Date) == 2023,
         month(Field_Collection_Date) == 5)

GRG002_Jun2023 <- GRG002 %>%
  filter(Result_Parameter_Name == "Temperature, water",
         year(Field_Collection_Date) == 2023,
         month(Field_Collection_Date) == 6)

GRG002_Jul2023 <- GRG002 %>%
  filter(Result_Parameter_Name == "Temperature, water",
         year(Field_Collection_Date) == 2023,
         month(Field_Collection_Date) == 7)

GRG002_Aug2023 <- GRG002 %>%
  filter(Result_Parameter_Name == "Temperature, water",
         year(Field_Collection_Date) == 2023,
         month(Field_Collection_Date) == 8)

GRG002_Sep2023 <- GRG002 %>%
  filter(Result_Parameter_Name == "Temperature, water",
         year(Field_Collection_Date) == 2023,
         month(Field_Collection_Date) == 9)

#Reusable plotting function but without axis labels
make_temp_plot <- function(df1, df2, title = NULL) {
  ggplot() +
    geom_line(data = df1, aes(Result_Value, Depth_Value, color = "SJF001")) +
    geom_line(data = df2, aes(Result_Value, Depth_Value, color = "GRG002")) +
    scale_color_manual(values = c("SJF001" = "blue", "GRG002" = "red"),
                       name = "Station") +
    scale_x_continuous(limits = c(7.5, 16)) +
    scale_y_reverse(limits = c(150, 0)) +
    labs(x = NULL, y = NULL, title = title) +   # drop axis labels here
    theme_minimal() +
    theme(legend.position = "none", text = element_text(size = 18))
}

#Build plots
May <- make_temp_plot(SJF001_May2023, GRG002_May2023, "May 17/19")
Jun <- make_temp_plot(SJF001_Jun2023, GRG002_Jun2023, "Jun 12")
Jul <- make_temp_plot(SJF001_July2023, GRG002_Jul2023, "Jul 10/11")
Aug <- make_temp_plot(SJF001_Aug2023, GRG002_Aug2023, "Aug 14/16")
Sep <- make_temp_plot(SJF001_Sep2023, GRG002_Sep2023, "Sep 12/13")

#Extract legend once
legend <- get_legend(
  ggplot() +
    geom_line(data = SJF001_May2023, aes(Result_Value, Depth_Value, color = "SJF001")) +
    geom_line(data = GRG002_May2023, aes(Result_Value, Depth_Value, color = "GRG002")) +
    scale_color_manual(values = c("SJF001" = "blue", "GRG002" = "red"),
                       name = "Station") +
    theme_minimal() +
    theme(legend.position = "right", text = element_text(size = 18))
)

# Combine plots
plots <- plot_grid(May, Jun, Jul, Aug, Sep, nrow = 1)

# Add axis labels outside using cowplot::ggdraw
final <- plot_grid(
  plots, legend, rel_widths = c(1, 0.1)
)

xlab <- ggdraw() + draw_label("Temperature (°C)", size = 18)
ylab <- ggdraw() + draw_label("Depth (m)", angle = 90, size = 18)

plot_with_axes <- plot_grid(
  ylab, final, ncol = 2, rel_widths = c(0.05, 1)
)

CTD_profiles <- plot_grid(
  plot_with_axes, xlab, ncol = 1, rel_heights = c(1, 0.05)
)

ggsave("results/figures/CTD_profiles.pdf", CTD_profiles, width = 15, height = 15)
