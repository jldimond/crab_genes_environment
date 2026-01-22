# ============================================================
# run_all.R
# Purpose: Run entire workflow in order
# ============================================================

source("R/00_setup_and_cleaning.R")
source("R/01_size_and_CPUE_plots.R")
source("R/02_genotype_likelihood_PCA_admixture.R")
source("R/03_process_temperature_data.R")
source("R/04_sst_size_relationship.R")
source("R/05_genes_vs_env_modeling.R")
source("R/06_GWAS.R")
source("R/supplemental/07_CTD_profiles.R")
source("R/supplemental/08_size_temp_studies.R")
source("R/supplemental/09_prey.R")
