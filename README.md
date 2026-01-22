Genes vs. Environment: Body Size Variation Among Recruitment Cohorts of Dungeness Crab
# Analysis Directory

This repository contains the full workflow, data, and code used to analyze environmental drivers, genetic structure, and phenotype–environment relationships in Dungeness crab megalopae. The project is organized to support reproducible R-based analysis.

---

## Repository Structure

crab_genes_environment/
├── crab_genes_environment.Rproj
├── data/
├── R/
├── renv/
├── results/
└── README.md   (this file)

---

## 1. Project Overview

This project integrates:
- Field observations of larval abundance, settlement, size, and temperature conditions
- Genetic data from low-coverage whole-genome sequencing
- Genotype likelihood PCA and admixture analyses
- GWAS on larval size
- Temperature data
- Supplemental environmental and prey-related analyses

All analyses can be reproduced using the scripts in R/ and the packaged R environment in renv/.

---

## 2. Data Directory (data/)

The data/ folder contains all raw and processed datasets used in this project. Contents include:

### Genomic data analysis files (ANGSD)
- 2023_larvae3 – List of .bam alignment files for ANGSD genotype likelihood analyses (to be uploaded to NCBI)
- 2023_larvae3.admix.2.Q – Admixture proportions (K = 2)
- 2023_larvae3.cov, 2023_larvae3.covMat – PCA covariance matrices
- megalopae_clusters2023.cov – Covariance matrix from targeted genomic analysis of large and small megalopae
- gwas.lrt0.gz – GWAS results

### Larval abundance, size, and environmental data
- 20250410_LT_Counts_2022-2023.csv – Larval trap counts
- 20250410_LT_StationMetadata_2022-2023.csv – Station metadata
- Larvae_Field_Data_ALL.csv – Larval size dataset

### Sea surface temperature time series
- FridayHarbor_frdw1h2023.txt
- HeinBank_46088h2023.txt
- NeahBay_46087h2023.txt
- Tacoma_tcnw1h2023.txt
- Westport_46211h2023.txt
- Georgia_Strait_ECCC_MSC_BUOYS_*.csv
- orca2_L3_depthgridded_025_*.tsv

### Spatial data
- coast_shapefile/ – GSHHS shoreline shapefile components (.shp, .dbf, .prj, .shx)

### Station metadata
- stations.csv – Locations of light trap stations and SST sensors

### Data used for supplementary information
- SJF001_EIMContinuousDepthSeriesData_*.csv – CTD profiles for station SJF001 (Strait of Juan de Fuca)
- GRG002_EIMContinuousDepthSeriesData_*.csv – CTD profiles for station GRG002 (Strait of Georgia)
- seasonal_average_simple_means.csv – Seasonal mean cancrid larvae and zooplankton biomass (Puget Sound Zooplankton Monitoring Program)
- size_temperature.txt – Size–temperature relationships from earlier laboratory studies

---

## 3. Analysis Scripts (R/)

The analysis is organized into numbered scripts that run sequentially or independently.

### Main analysis pipeline
1. 00_setup_and_cleaning.R  
   Loads packages and imports and cleans raw data.
2. 01_size_and_CPUE_plots.R  
   Plots larval size, CPUE, and seasonal patterns.
3. 02_genotype_likelihood_PCA_admixture.R  
   PCA on genotype likelihood covariance matrices; admixture visualization.
4. 03_process_temperature_data.R  
   Imports, cleans, and aggregates temperature time series.
5. 04_sst_size_relationship.R  
   Quantifies relationships between SST and mean larval size.
6. 05_genes_vs_env_modeling.R  
   Linear mixed models testing the role of genetics and temperature in larval size.
7. 06_GWAS.R  
   GWAS on larval size using an ANGSD genotype likelihood approach.
8. run_all.R  
   Master script to run the full workflow end-to-end.

### Supplemental analyses (R/supplemental/)
- 07_CTD_profiles.R – CTD temperature–depth profiles for the Straits of Juan de Fuca and Georgia
- 08_size_temp_studies.R – Temperature–size relationships from earlier laboratory studies
- 09_prey.R – Cancrid larvae and zooplankton biomass from the Puget Sound Zooplankton Monitoring Program

---

## 4. Reproducible Environment (renv/)

This project uses {renv} to lock package versions and ensure full reproducibility.

To restore the environment:
renv::restore()

---

## 5. Running the Analysis

1. Open crab_genes_environment.Rproj in RStudio
2. Run:
renv::restore()
source("R/run_all.R")

Individual analyses can be run by sourcing specific scripts, although some depend on output from earlier steps.

Output figures and derived data are written to results/.