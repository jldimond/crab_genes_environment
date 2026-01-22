# ============================================================
# 02_genotype_likelihood_PCA_admixture.R
# Purpose: Conduct PCA and admixture analysis from ANGSD
#          covariance matrix and admixture Q files
# ============================================================

library(tidyverse)
library(viridis)
library(cowplot)
library(lubridate)
library(stringr)
library(ggrepel)
library(scatterpie)

# ---- Read covariance and sample metadata ----
pcangsd_cov <- as.matrix(read.table("data/2023_larvae3.cov"), header = F)
pop <- read.table("data/2023_larvae3")
sample <- str_extract(pop$V1, "(?<=trimmed-).*?(?=_S)")
samples <- data.frame(
  path = pop$V1,
  sample = sample,
  pop = substr(sample, 1, 3),
  lane = substr(pop$V1, regexpr("L00", pop$V1), regexpr("L00", pop$V1) + 3)
)
samples$pop <- gsub("GAR", "GAD", samples$pop)

# ---- Merge with specimen data ----
crab_data <- read.csv("results/clean/crab_data_clean.csv")
#change to proper date
crab_data$Date <- as.Date(crab_data$Date)

specimens2023 <- crab_data %>%
  filter(format(Date, "%Y") == "2023") %>%
  mutate(
    sample = paste0(Site, "23_", str_pad(Vial, width = 3, side = "left", pad = "0"))
  ) %>%
  distinct(sample, .keep_all = TRUE)

merged2023 <- merge(samples, specimens2023, by = "sample", all.x = TRUE)

# ---- PCA ----
#perform the pca using the eigen function. 
pcangsd_pca <- eigen(pcangsd_cov) 

pcangsd_eigenvectors <- pcangsd_pca$vectors #extract eigenvectors 
pca.vectors <- as_tibble(cbind(merged2023, data.frame(pcangsd_eigenvectors))) #combine with our population assignments

#Levels reorder from outside to inside salish sea
group_order <- c("WTP", "NHB", "SQR", "FHL", "PTW", "COR", "LOF", "SMS", "APS", "GAD", "MST")

# Convert Group.1 to a factor with the desired order
pca.vectors$pop <- factor(pca.vectors$pop, levels = group_order)

# Filter for specific sites
filtered_data <- pca.vectors %>%
  filter(pop %in% c("COR", "MST", "NHB", "PTW", "SMS", "SQR", "WTP", "FHL"))

pca_plot <- ggplot(data = filtered_data, 
                   aes(x = X1, y = X2, color=pop, label = pop)) + 
  geom_text(key_glyph = "point") +
  scale_color_viridis(discrete=TRUE) +
  stat_ellipse() +
  labs(x = "PC1 (0.6%)", y = "PC2 (0.2%)", color = "Location") +
  theme_classic() +
  theme(
    axis.line.y.right = element_line(),
    axis.line.x.top = element_line(),
    axis.ticks = element_blank(),         
    axis.ticks.length = unit(0, "pt") 
  ) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  theme(text = element_text(size = 14))

#screeplot
variance <- pcangsd_pca$values/sum(pcangsd_pca$values)
component <- seq(1:length(pcangsd_pca$values))
scree <- cbind(component,variance)
screeplot <- ggplot(data=scree[1:50,], aes(x=component, y=variance)) +
  geom_bar(stat="identity") +
  theme_classic() +
  theme(
    axis.line.y.right = element_line(),
    axis.line.x.top = element_line(),
    axis.ticks = element_blank(),         
    axis.ticks.length = unit(0, "pt") 
  ) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = dup_axis(name = NULL, labels = NULL)) +
  theme(legend.position = "none", text = element_text(size = 12),
        plot.background = element_blank()) 


full_pca_plot <- ggdraw() +
  draw_plot(pca_plot) +  
  draw_plot(screeplot, x = 0.66, y = 0.73, width = 0.23, height = 0.25)  

# ---- Admixture ----
q <- read.table("data/2023_larvae3.admix.2.Q")
crab_q <- cbind(samples$sample, samples$pop, q)
colnames(crab_q) <- c("sample", "site", "k1", "k2")
#reorder sites from west to east
crab_q$site <- factor(crab_q$site, levels = c("WTP", "NHB", "SQR", "FHL", "PTW", 
                                              "COR", "SMS", "LOF", "APS",
                                              "GAD", "MST"))

crab_admix_long <- crab_q %>%
  pivot_longer(cols = c(k1, k2), names_to = "K", values_to = "prop")

crab_admix_long_date <- merge(crab_admix_long,specimens2023, by.x = "sample", by.y = "sample")

filtered_data <- crab_admix_long_date %>%
  filter(site %in% c("COR", "MST", "NHB", "PTW", "SMS", "SQR", "WTP", "FHL")) %>%
  arrange(site, Date, sample) %>%
  mutate(
    sample = factor(sample, levels = unique(sample)),
    month = floor_date(Date, "month")
  )

#One sample per month per site for x-axis tick marks
monthly_ticks <- filtered_data %>%
  group_by(site, month) %>%
  slice_min(Date, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    sample = factor(sample, levels = levels(filtered_data$sample)),
    month_label = format(month, "%b")  # Abbreviated month (e.g. Jun, Jul)
  )

#K=2 admixture barplot with tick marks and month labels
k2plot <- ggplot(filtered_data, aes(x = sample, y = prop, fill = K)) +
  geom_col(color = "gray", linewidth = 0.1) +
  geom_segment(
    data = monthly_ticks,
    aes(x = sample, xend = sample, y = -0.02, yend = 0),
    inherit.aes = FALSE,
    color = "black") +
  facet_grid(~site, scales = "free_x", space = "free_x") +
  coord_cartesian(ylim = c(-0.1, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 1)) +
  
  labs(x = "Individuals", y = "Ancestry") +
  theme_minimal(base_size = 14) +
  theme(
    strip.text.x = element_text(angle = 0),
    panel.spacing.x = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 10, 30, 10)  # extra space for month labels
  )

#Make the Map

#get coast shapefile
coast <-st_read(dsn="data/coast_shapefile", layer = "GSHHS_f_L1")

# Filter for specific sites
filtered_data <- crab_q %>%
  filter(site %in% c("WTP", "NHB", "SQR", "PTW", "COR", "FHL", "SMS", "MST"))

#get cluster means by site in order to plot pies
crab_q_ag <- aggregate(filtered_data, by = list(filtered_data$site), FUN = mean)

#add in site latitude and longitude info 
#read in station data
stations <- read.csv("data/stations.csv")
crab_q_ag <- crab_q_ag %>%
  left_join(stations %>% select(Name, Lat, Lon), by = c("Group.1" = "Name"))
crab_q_ag$radius <- rep(0.08,8)


#create map 
map <- ggplot() + 
  geom_sf(data = coast)+
  geom_scatterpie(aes(x=Lon, y=Lat, r=radius), data=crab_q_ag, cols = c("k1", "k2")) +
  geom_text(data= crab_q_ag,aes(x=Lon, y=Lat, label=Group.1), check_overlap = TRUE, 
            nudge_x = 0.00, nudge_y = -0.12, color = "grey22", size = 4, fontface = "bold")+
  annotate(geom = "segment", x = -121.84, xend = -122.5,y= 46.6, yend = 46.6, 
           color = "grey22", size = 1.5)+
  annotate(geom = "text", x = -122.17, y = 46.73, label = "50km", color = "grey22",
           size = 4) +
  labs(x = "Longitude", y = "Latitude", color = "Collection Site") +
  scale_x_continuous(breaks = seq(-125, -121, by = 1)) +
  scale_y_continuous(breaks = c(47,48,49)) +
  theme_minimal() +
  theme(legend.position="none", text = element_text(size = 14)) +
  coord_sf(xlim = c(-125, -121.5),ylim = c(46.5,49.5))

# PLot all together
# First assemble the full layout using grid.arrange()
combined_grob <- arrangeGrob(
  full_pca_plot,     # 1
  k2plot,   # 2 
  map,     # 3
  
  layout_matrix = rbind(
    c(1, 3),  # top row
    c(2, 2)   # bottom row
  ),
  widths = c(1.5, 1),
  heights = c(2, 1)
)

# Then overlay the grob and add labels
p_combined <- ggdraw(combined_grob) +
  draw_plot_label(
    label = c("A", "B", "C"),
    x = c(0.01, 0.6, 0.01),       # Top row
    y = c(0.99, 0.99, 0.35),      # Bottom row
    size = 14
  )

# ---- Combine and save ----
dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("results/figures/Figure_PCA_Admixture.pdf", p_combined, width = 16, height = 10)
saveRDS(pca.vectors, "results/outputs/pca_vectors.rds")

