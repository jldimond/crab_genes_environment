# ============================================================
# 04_sst_size_relationship.R
# Relate 28-day mean SST to crab size data using regression and mixed models
# ============================================================

library(tidyverse)
library(lubridate)
library(lme4)
library(mclust)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(ggpubr)
library(lmerTest)

#load datasets
daily_means_list <- readRDS("results/outputs/daily_means_list.rds")
sst_plot_data <- readRDS("results/outputs/sst_plot_data.rds")
crab_data_clean <- read.csv("results/clean/crab_data_clean.csv", header = TRUE)
crab_data_clean$Date <- as.Date(crab_data_clean$Date)

# light trap site to SST station mapping
site_to_list <- list(
  COR = "Hein Bank", GAD = "Hoodsport mean", PTW = "Hein Bank",
  SMS = "Georgia Strait", SQR = "Hein Bank", FHL = "Friday Harbor",
  MST = "Tacoma", NHB = "Neah Bay", WTP = "Westport"
)

# ---- calculate_28_day_mean ----

calculate_28_day_mean <- function(date, site, daily_means_list, site_to_list) {
  dataset <- site_to_list[[site]]
  temp_data <- daily_means_list[[dataset]]
  window <- temp_data %>% filter(day >= (date - 28) & day <= date)
  mean(window$mean_V15, na.rm = TRUE)
}

#get 2023 only dataset
crab_data2023 <- crab_data_clean %>%
  filter(
    format(Date, "%Y") == "2023",
    Site %in% c("WTP", "PTW", "COR", "FHL", "MST", "GAD", "SMS","SQR", "NHB"))

# Apply the function to each row in crab_data2023
crab_data2023 <- crab_data2023 %>%
  rowwise() %>%
  mutate(
    Mean_28_Day_V15 = calculate_28_day_mean(
      as.Date(Date), Site, daily_means_list, site_to_list
    )
  ) %>%
  ungroup()

# ---- Plot correlations ----

filtered_data <- crab_data2023 %>%
  filter(Site %in% c("WTP", "PTW", "COR", "FHL", "MST", "SMS","SQR", "NHB"))

sst_size1 <- ggplot(filtered_data, aes(x = Mean_28_Day_V15, y = CL)) +
  geom_point(color = "black", shape = 16, alpha = 0.5, size = 3) +  
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "solid", linewidth = 1) +  
  stat_cor(label.y = 3, size = 5) +
  labs(x = "Mean 28 Day Prior SST (°C)",
       y = "Carapace Length (mm)") +
  theme_classic() +
  theme(panel.border = element_rect(linetype = "solid", linewidth = 0.75, fill = NA)) +
  theme(legend.position = "right", text = element_text(size = 18))

# Individual site correlations 
sst_size2 <- ggplot(filtered_data, aes(x = Mean_28_Day_V15, y = CL, color = Site)) +
  geom_point(shape = 16, alpha = 0.8, size = 3) + 
  geom_smooth(method = "lm", se = TRUE, aes(color = Site), linetype = "solid", linewidth = 1) +  
  stat_cor(label.x.npc = 0.005, label.y.npc = 0.25,size = 4, show.legend = FALSE) +
  labs(x = "Mean 28 Day Prior SST (°C)",y = "Carapace Length (mm)",color = "Site") +
  theme_classic() +
  theme(panel.border = element_rect(linetype = "solid", linewidth = 0.75, fill = NA)) +
  theme(legend.position.inside = c(0.1, 0.1), text = element_text(size = 18)) 

# ---- Linear mixed effects model ----

model <- lmer(CL ~ Mean_28_Day_V15 + (1 | Site), data = filtered_data)
summary(model)
ranova(model)

#look at random effects: Best Linear Unbiased Predictors (BLUPs)—i.e., 
#how much each site’s intercept differs from the overall mean (positive = above average, negative = below).
ranef_df <- as.data.frame(ranef(model)$Site)
ranef_df$Site <- rownames(ranef(model)$Site)

ggplot(ranef_df, aes(x = reorder(Site, `(Intercept)`), y = `(Intercept)`)) +
  geom_col(fill = "steelblue") +
  labs(x = "Site", y = "Site Random Effect (Intercept Deviation)") +
  theme_minimal() +
  coord_flip()

# ---- SST and size plot ----

sst_plot_data$source <- factor(sst_plot_data$source, levels = c("Hoodsport mean", "Georgia Strait", "Westport", "Tacoma", "Neah Bay", "Friday Harbor", "Hein Bank"))

SST_filtered_data <- sst_plot_data %>%
  filter(source %in% c("Westport", "Tacoma", "Friday Harbor", "Georgia Strait", "Neah Bay", "Hein Bank"))

#exlude GAD
filtered_data <- crab_data2023 %>%
  filter(Site %in% c("COR", "PTW", "WTP", "FHL", "NHB", "MST", "SMS", "SQR"))

SST_plot <- ggplot() + 
  geom_point(data = filtered_data, aes(x = Date, y = CL, fill = factor(Site)), shape = 21, stroke = 0, alpha = 0.6, size = 3) +
  geom_line(data = SST_filtered_data, aes(x = as.Date(day), y = mean_V15, color = source, linetype = source), size = 1) +
  scale_y_continuous("Carapace Length (mm)", sec.axis = sec_axis(~ . * 1, name = "SST (°C)")) +
  scale_fill_manual(
    values = c("COR" = "#8dd3c7", "FHL" = '#ffffb3', "MST" = "#e31a1c", 
               "NHB" = "#1f78b4", "PTW" = "#ff7f00", "SMS" = "#b3b3b3", "SQR" = "#fccde5", "WTP" = "#33a02c"), 
    name = "Site") +
  scale_color_manual(
    values = c("Georgia Strait" = '#d73027', "Westport" ='#fc8d59', "Tacoma" = '#fee090',"Neah Bay" ='#e0f3f8', "Friday Harbor" = '#91bfdb',"Hein Bank"='#4575b4'), 
    name = "SST Dataset") +
  scale_linetype_manual(
    values = c("Georgia Strait" = "solid", "Westport" = "solid", "Neah Bay" = "solid", 
               "Tacoma" = "solid", "Friday Harbor" = "solid", "Hein Bank" = "solid"),
    guide = "none") +
  theme_classic() +
  theme(panel.border = element_rect(linetype = "solid", linewidth = 0.75, fill = NA)) +
  theme(legend.position = "right", text = element_text(size = 18))

# ---- Base map ----

#load station data
stations <- read.csv("data/stations.csv")
#exclude LOF and GAD
stations <- stations[-c(2,3,17),]
#get coast shapefile
coast <-st_read(dsn="data/coast_shapefile", layer = "GSHHS_f_L1")

station_map <- ggplot() + 
  geom_sf(data = coast)+
  geom_point(data = stations, aes(x = Lon, y = Lat, color = Location, shape= Location), size = 5, position=position_jitter(h=0,w=0.06)) +
  geom_text_repel(data = stations, aes(x = Lon, y = Lat, label = Name), size = 5, fontface = "bold") +  
  annotate(geom = "segment", x = -121.84, xend = -122.5,y= 46.6, yend = 46.6, 
           color = "grey22", size = 1.5)+
  annotate(geom = "text", x = -122.17, y = 46.73, label = "50km", color = "grey22",
           size = 5) +
  labs(x = "Longitude", y = "Latitude", color = "Location") +
  scale_x_continuous(breaks = seq(-125, -121, by = 1)) +
  scale_y_continuous(breaks = c(47,48,49)) +
  theme_minimal() +
  theme(legend.position= c(0.85, 0.9), text = element_text(size = 18)) +
  coord_sf(xlim = c(-125, -121.5),ylim = c(46.5,49.5))

# ---- Combined plot ----

# First assemble the full layout using grid.arrange()
combined_grob <- arrangeGrob(
  station_map,     # 1
  SST_plot,   # 2 
  sst_size1,     # 3
  sst_size2,     # 4
  
  layout_matrix = rbind(
    c(1, 1, 2, 2, 2, 2),  # top row
    c(3, 3, 3, 4, 4, 4)   # bottom row
  ),
  widths = c(1, 1, 1, 1, 1, 1),
  heights = c(1, 1)
)

# Then overlay the grob and add labels
temp_size_plot <- ggdraw(combined_grob) +
  draw_plot_label(
    label = c("A", "B", "C", "D"),
    x = c(0.01, 0.32, 0.01, 0.5),       # Top row
    y = c(0.99, 0.99, 0.49, 0.49),      # Bottom row
    size = 18
  )

ggsave("results/figures/SST_vs_size.pdf", temp_size_plot, width = 20, height = 15)
saveRDS(crab_data2023, "results/outputs/crab_data2023_sst.rds")
