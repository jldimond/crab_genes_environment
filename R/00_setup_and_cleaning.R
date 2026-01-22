# ============================================================
# 00_setup_and_cleaning.R
# Purpose: Import raw field data, clean and
#          filter entries, remove outliers, and save clean data
# ============================================================

# ---- Libraries ----
library(tidyverse)
library(lubridate)

# ---- Read data ----
crab_data <- read.csv("data/Larvae_Field_Data_ALL.csv")

# ---- Remove bad notes ----
crab_data <- crab_data %>%
  filter(!grepl("instar|broken|incomplete|skipped|missing", Notes, ignore.case = TRUE))

# ---- Format columns ----
crab_data <- crab_data %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    Vial = sprintf("%03d", Vial),
    Site = if_else(Site == "GAR", "GAD", Site)
  )

colnames(crab_data) <- c("Site", "Vial", "Date", "CW", "CL", "TL", "Notes")

# ---- Outlier removal ----
ratios <- crab_data[, 4:6]
z_scores <- as.data.frame(sapply(ratios, function(x)
  abs(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)))
crab_data$outliers <- rowSums(z_scores)
crab_data_no_outliers <- crab_data[crab_data$outliers <= 6, ]

#remove rows with only NAs
crab_data_no_outliers <- crab_data_no_outliers[rowSums(is.na(crab_data_no_outliers)) != ncol(crab_data_no_outliers), ]

# ---- Save clean data ----
dir.create("results/clean", showWarnings = FALSE, recursive = TRUE)
write.csv(crab_data_no_outliers, "results/clean/crab_data_clean.csv", row.names = FALSE)
