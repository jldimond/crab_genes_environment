# ============================================================
# 01_size_and_CPUE_plots.R
# Purpose: Summarize 2023 larval size and CPUE, and plot
#          regional map with combined figure
# ============================================================

# ---- Libraries ----
library(tidyverse)
library(patchwork)
library(ggthemes)
library(ggrepel)
library(ggspatial)
library(viridis)
library(sf)
library(rworldmap)
library(cowplot)
library(gridExtra)

# ---- Load data ----
crab_data <- read.csv("results/clean/crab_data_clean.csv")
catch_all <- read.csv("data/20250410_LT_Counts_2022-2023.csv")

#change to proper date
crab_data$Date <- as.Date(crab_data$Date)

# ---- Calculate CPUE ----
catch_all <- catch_all %>%
  mutate(
    Total_M_magister = Metacarcinus_magister_megalopae + Metacarcinus_magister_instar,
    CPUE = Total_M_magister / Hours_Fished,
    logCPUE = log(CPUE + 1),
    Date = as.Date(Date, format = "%m/%d/%y")
  )

# ---- Filter for 2023 and target sites ----
target_sites <- c("WTP", "PTW", "COR", "MST", "SMS")
crab_data2023 <- crab_data %>% filter(format(Date, "%Y") == "2023", Site %in% target_sites)
catch2023 <- catch_all %>% filter(Year == 2023, Site %in% target_sites)

# ---- Plot ----
pMain2023 <- ggplot() +
  geom_point(data = crab_data2023, aes(x = Date, y = CL, color = factor(Site)), shape = 19, alpha = 0.9, size = 3) +
  scale_color_manual(
    values = c("COR" = "#e41a1c", "MST" = "#984ea3", "PTW" = "#4daf4a", "SMS" = "#377eb8", "WTP" = "#ff7f00"), 
    name = "Site") +
  labs(x = "Date (2023)",y = "Carapace Length (mm)",color = "Site") +
  theme_classic() +
  theme(legend.position = "none", text = element_text(size = 20)) +
  scale_x_date(limits = as.Date(c("2023-04-15", "2023-09-20"))) +
  scale_y_continuous(limits = c(3, 9))


pTop2023 <- ggplot(catch2023[!is.na(catch2023$logCPUE),], aes(x = Date, y = logCPUE, color = Site)) +
  geom_line(size=2) +
  scale_color_manual(
    values = c("COR" = "#e41a1c", "MST" = "#984ea3", "PTW" = "#4daf4a", "SMS" = "#377eb8", "WTP" = "#ff7f00"), 
    name = "Site") +
  labs(x = "",y = "logCPUE",color = "Site") +
  theme_classic() +
  theme(legend.position = "none", text = element_text(size = 20)) +
  scale_x_date(limits = as.Date(c("2023-04-15", "2023-09-20"))) +
  scale_y_continuous(limits = c(0, 8))


pRight2023 <- ggplot(crab_data2023, aes(x = CL, color = Site)) +
  geom_density(size=2) +  
  scale_color_manual(
    values = c("COR" = "#e41a1c", "MST" = "#984ea3", "PTW" = "#4daf4a", "SMS" = "#377eb8", "WTP" = "#ff7f00"), 
    name = "Site") +
  labs(x = "", y = "Density") +
  theme_classic () +  
  theme(legend.position = "none", text = element_text(size = 20)) +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1, by = 0.5)) +
  scale_x_continuous(limits = c(3, 9)) +
  coord_flip() 


pEmpty2023 <- ggplot(data = catch2023[!is.na(catch2023$logCPUE),], aes(x = Date, y = logCPUE, color = Site)) +
  geom_point(aes(x = NA, y = NA), size = 4) +  # Only used to trigger the legend
  scale_color_manual(
    values = c("COR" = "#e41a1c", "MST" = "#984ea3", "PTW" = "#4daf4a", "SMS" = "#377eb8", "WTP" = "#ff7f00"),
    name = "Site") +
  theme_void() +
  theme(text = element_text(size = 20)) 


legend2023 <- get_legend(pEmpty2023)


# ---- make Map ----

#get coast shapefile
coast <-st_read(dsn="data/coast_shapefile", layer = "GSHHS_f_L1")

#sites
# COR	48.4003220	-122.6251430
# FHL	48.545505	-123.012208
# GAD/GAR	47.420604	-123.130137
# LOF	47.815763	-122.655901
# MST	47.349217	-122.325128
# NBY	48.368748	-124.613309
# PTW	48.1357600	-122.7607920
# SMS	48.991144	-122.7713
# SQR	48.037076	-123.019722
# WTP	46.910525	-124.111444

#create map 
map <- ggplot() + 
  geom_sf(data = coast) +
  annotate(geom = "text", x = -123.212208, y = 48.545505, label = "FHL", 
           color = "grey22", size = 6, fontface = "bold") +
  annotate(geom = "point", x = -123.012208, y = 48.545505, 
           color = "grey22", size = 6) +
  annotate(geom = "text", x = -124.813309, y = 48.368748, label = "NHB", 
           color = "grey22", size = 6, fontface = "bold") +
  annotate(geom = "point", x = -124.613309, y = 48.368748, 
           color = "grey22", size = 6) +
  annotate(geom = "text", x = -123.219722, y = 48.037076, label = "SQR", 
           color = "grey22", size = 6, fontface = "bold") +
  annotate(geom = "point", x = -123.019722, y = 48.037076, 
           color = "grey22", size = 6) +
  annotate(geom = "text", x = -122.3, y = 48.400, label = "COR", 
           color = "#e41a1c", size = 7, fontface = "bold") +
  annotate(geom = "point", x = -122.625, y = 48.400, 
           color = "#e41a1c", size = 7) +
  annotate(geom = "text", x = -122.03, y = 47.3492, label = "MST", 
           color = "#984ea3", size = 7, fontface = "bold") +
  annotate(geom = "point", x = -122.325, y = 47.3492, 
           color = "#984ea3", size = 7) +
  annotate(geom = "text", x = -122.97, y = 48.22, label = "PTW", 
           color = "#4daf4a", size = 7, fontface = "bold") +
  annotate(geom = "point", x = -122.760, y = 48.135, 
           color = "#4daf4a", size = 7) +
  annotate(geom = "text", x = -122.46, y = 48.991, label = "SMS", 
           color = "#377eb8", size = 7, fontface = "bold") +
  annotate(geom = "point", x = -122.771, y = 48.991, 
           color = "#377eb8", size = 7) +
  annotate(geom = "text", x = -124.40, y = 46.910, label = "WTP", 
           color = "#ff7f00", size = 7, fontface = "bold") +
  annotate(geom = "point", x = -124.111, y = 46.910, 
           color = "#ff7f00", size = 7) +
  annotate(geom = "text", x = -124.75, y = 47.3, label = "Pacific\nOcean", 
           color = "grey22", size = 7) +
  annotate(geom = "text", x = -124, y = 48.33, angle = -18, label = "Strait of Juan de Fuca", 
           color = "grey22", size = 7) +
  annotate(geom = "text", x = -123.5, y = 49.16, angle = -39, label = "Strait of Georgia", 
           color = "grey22", size = 7) +
  annotate(geom = "text", x = -122.45, y = 47.7, angle = -85, label = "Puget Sound", 
           color = "grey22", size = 7) +
  annotate(geom = "point", x = -122.335, y = 47.608, colour = "grey22") + 
  annotate(geom = "text", x = -122.00, y = 47.608, label = "Seattle", 
           color = "grey22", size = 7) +
  annotate(geom = "point", x = -123.116, y = 49.246, colour = "grey22") + 
  annotate(geom = "text", x = -122.69, y = 49.215, label = "Vancouver", 
           color = "grey22", size = 7) +
  annotate(geom = "segment", x = -121.84, xend = -122.5,y= 46.65, yend = 46.65, 
           color = "grey22", size = 1.5)+
  annotate(geom = "text", x = -122.17, y = 46.73, label = "50km", color = "grey22",
           size = 7) +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(breaks = seq(-125, -121, by = 1)) +
  theme_minimal() +
  theme(legend.position="none", text = element_text(size = 20)) +
  coord_sf(xlim = c(-125, -121.5),ylim = c(46.5,49.5))

#create inset map showing region
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id
world.df <- world.points[,c("long","lat","group", "region")]

#box to highlight study area in inset map
box <- data.frame(xmin=-125,xmax=-121.5 ,ymin=47 ,ymax=49.5)

#plot inset
worldmap <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(50, -122, 0)) +
  geom_rect(data = box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 1, linetype=1) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  theme(panel.grid.major = element_line(color = "gray",linewidth = 0.5))

#add inset to main map
map2 <- map + annotation_custom(grob = ggplotGrob(worldmap), xmin = -122.5, ymin = 48.8, xmax = Inf, ymax = Inf)

combined_grob <- arrangeGrob(
  map2,          # 1
  pTop2023,     # 2
  legend2023,   # 3
  pMain2023,    # 4
  pRight2023,   # 5
  
  layout_matrix = rbind(
    c(1, 2, 3),   # top row
    c(1, 4, 5)    # bottom row
  ),
  widths  = c(3, 2, 1),   # adjust as needed
  heights = c(2, 3)
)

# Overlay the grob and add labels
combined <- ggdraw(combined_grob) +
  draw_plot_label(
    label = c("A", "B", "C", "D"),
    x     = c(0.01, 0.5, 0.5, 0.83),  
    y     = c(0.99, 0.99, 0.59, 0.59),
    size  = 20
  )

# ---- Save combined figure ----
dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("results/figures/Figure_size_CPUE_map.pdf", combined, width = 20, height = 12)
