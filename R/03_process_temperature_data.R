# ============================================================
# 03_process_temperature_data.R
# Purpose: Import and process NOAA + ECCC + Orca data into a 
# standardized daily mean SST list (daily_means_list)
# ============================================================

library(tidyverse)
library(lubridate)
library(ggplot2)

# ---- Process NOAA data ----

# File paths for NOAA data
files <- c("data/Westport_46211h2023.txt", "data/Tacoma_tcnw1h2023.txt", 
           "data/FridayHarbor_frdw1h2023.txt", "data/HeinBank_46088h2023.txt", 
           "data/NeahBay_46087h2023.txt")

# Define a function to process each file
process_file <- function(file) {
  # Read the data
  data <- read.table(file, header = FALSE, na.strings = c("999.0", "99.0"), stringsAsFactors = FALSE)
  
  # Select relevant columns
  data <- data %>%
    dplyr::select(V1, V2, V3, V4, V5, V15)
  
  # Create a datetime column
  data <- data %>%
    mutate(datetime = make_datetime(V1, V2, V3, V4, V5)) %>%
    select(datetime, V15)
  
  return(data)
}

# Process files
data_list <- lapply(files, process_file)

# Calculate daily means for each dataset separately
daily_means_list <- lapply(data_list, function(data) {
  data %>%
    filter(!is.na(V15)) %>%
    mutate(day = floor_date(datetime, "day")) %>%
    group_by(day) %>%
    summarize(mean_V15 = mean(V15, na.rm = TRUE))
})

# name the results by file names for clarity
names(daily_means_list) <- files

# ---- Add Georgia Strait and Hoodsport data ----

#Georgia Strait
GStrait <- read.csv("data/Georgia_Strait_ECCC_MSC_BUOYS_db7f_989c_896a.csv", header = TRUE, stringsAsFactors = FALSE)
# Select relevant columns
GStrait <- GStrait %>%
  dplyr::select(time, avg_sea_sfc_temp_pst10mts)
GStrait <- GStrait[-1,]
# Convert to POSIXct
GStrait$time <- as.POSIXct(GStrait$time, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
# convert temp to numeric
GStrait$avg_sea_sfc_temp_pst10mts <- as.numeric(GStrait$avg_sea_sfc_temp_pst10mts)

# Compute the mean temperature by day
GStrait_daily_mean_temp <- GStrait %>%
  mutate(date = as.Date(time)) %>%        
  group_by(date) %>%                      
  summarize(daily_mean_temp = mean(avg_sea_sfc_temp_pst10mts, na.rm = TRUE))  

#reformat columns like other datasets
colnames(GStrait_daily_mean_temp) <- c("day", "mean_V15")

#add to list
daily_means_list$GStrait_daily_mean_temp <- GStrait_daily_mean_temp %>%
  filter(day >= as.Date("2023-01-01") & day < as.Date("2023-12-31")) %>%
  mutate(day = as.POSIXct(day, tz = "UTC"))

names(daily_means_list) <- c("Westport","Tacoma","Friday Harbor","Hein Bank","Neah Bay","Georgia Strait")
                             
#Now add Hoodsport
# compute daily means for prior 5 years (2018-2022) at Hoodsport to fill in gaps in 2023 dataset

file_path <- "data/orca2_L3_depthgridded_025_d59e_6630_1bc2.tsv"
data <- read_tsv(file_path)

data <- data %>%
  mutate(datetime = as.POSIXct(`UTC`, format = "%Y-%m-%d %H:%M:%S"))

# Filter data for 3.0 m depth
data_3m <- data %>% filter(m == 3.0)

# Extract date from datetime and calculate mean daily temperatures
daily_mean_temps <- data_3m %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarize(mean_temp = mean(degree_C, na.rm = TRUE))

# Add columns for month and day from the date
daily_mean_temps <- daily_mean_temps %>%
  mutate(month = month(date), day = day(date))

# Group by month and day to calculate the mean temperature across all years
calendar_mean_temps <- daily_mean_temps %>%
  group_by(month, day) %>%
  summarize(mean_temp = mean(mean_temp, na.rm = TRUE), .groups = "drop")

# drop leap year day
Hoodsport_calendar_mean_temps <- calendar_mean_temps[-60,]

# Add column for 2023 dates and format as other datasets
Hoodsport_calendar_mean_temps$day <- daily_means_list$Westport$day
Hoodsport_calendar_mean_temps <- Hoodsport_calendar_mean_temps[,-1]
colnames(Hoodsport_calendar_mean_temps) <- c("day","mean_V15")

#add to list
daily_means_list$Hoodsport_calendar_mean_temps <- Hoodsport_calendar_mean_temps

names(daily_means_list) <- c("Westport","Tacoma","Friday Harbor","Hein Bank","Neah Bay","Georgia Strait", "Hoodsport mean")

sst_plot_data <- bind_rows(
  lapply(names(daily_means_list), function(name) {
    daily_means_list[[name]] %>% mutate(source = name)
  })
)

ggplot(sst_plot_data, aes(x = day, y = mean_V15, color = source)) +
  geom_line() +
  labs(x = "Date", y = "Mean SST (°C)", color = "Dataset") +
  theme_minimal() +
  theme(legend.position = "right")

saveRDS(sst_plot_data, "results/outputs/sst_plot_data.rds")
saveRDS(daily_means_list, "results/outputs/daily_means_list.rds")

