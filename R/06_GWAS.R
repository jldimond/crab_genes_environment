# ============================================================
# 06_GWAS.R
# Purpose: Genome-wide association study of larval crab carapace length
# ============================================================

#Load the covariance matrix
cov <- as.matrix(read.table("data/2023_larvae3.covMat"), header = F)

#Add a column with population assingments
pop <- read.table("data/2023_larvae3")
#extract sample names
sample <- str_extract(pop$V1, "(?<=trimmed-).*?(?=_S)")
pop_id <- substr(sample, 1, 3)
lane <- as.data.frame(substr(pop$V1, regexpr("L00", pop$V1), regexpr("L00", pop$V1) + 3))
samples <- cbind(pop, sample, pop_id, lane)
colnames(samples) <- c("path", "sample", "pop", "lane")
#change GAR tp GAD
samples$sample <- gsub("GAR", "GAD", samples$sample)
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

#perform the pca using the eigen function. 
mme.pca <- eigen(cov) 
#extract eigenvectors 
eigenvectors = mme.pca$vectors 

#combine with our population assignments
pca.vectors = as_tibble(cbind(merged2023, data.frame(eigenvectors))) 
pca.vectors$pop <- as.factor(pca.vectors$pop)

#data with 28 day prior SST
crab_data2023 <- readRDS("results/outputs/crab_data2023_sst.rds")

#add in sample column
#create three-digit sample numbers
crab_data2023 <- crab_data2023 %>%
  mutate(samples = paste0(Site, "23_", sprintf("%03d", Vial)))

#now merge samples and specimens df
gwas_merged <- merge(pca.vectors, crab_data2023, by.x = "sample", by.y = "samples")
gwas_merged2 <- gwas_merged %>%
  dplyr::select(path, Site.x, sample, CL.x, X1, Date.x, Mean_28_Day_V15)

filtered_data <- gwas_merged2 %>%
  filter(Site.x %in% c("COR", "PTW", "WTP", "NHB", "MST", "FHL", "SQR"), !is.na(CL.x))

filtered_data$Date <- as.numeric(filtered_data$Date)

#check size for normality
hist(filtered_data$CL)
#create Q-Q plot 
qqnorm(filtered_data$CL)
qqline(filtered_data$CL)

#write bam list
write.table(filtered_data$path, file = "results/outputs/gwas.bamlist", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

#write phenotype list
write.table(filtered_data$CL.x, file = "results/outputs/gwas.phenotypes", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

#write covariates list
write.table(filtered_data[,c(5:7)], file = "results/outputs/gwas.covariates", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

###Results

#import results files
gwas_results <- read.table(gzfile("data/gwas.lrt0.gz"), header = TRUE)   

#bonferroni correction = alpha/#tests
sig <- 0.05 / nrow(gwas_results)
#1.38914e-07 is p value threshold
sig_p <- gwas_results[gwas_results$P < sig,]
#no significant SNPs

#column for plotting index
gwas_results$Index <- seq_len(nrow(gwas_results))

# Manhattan plot
gwas <- ggplot(gwas_results, aes(x = Index, y = -log10(P))) +
  geom_point(size = 1.4, color = "black") +
  geom_hline(yintercept = -log10(sig), color = "red", linetype = "dashed") +
  labs(x = "Variant Index", y = expression(-log[10](P))) +
  theme_minimal() +
  theme(legend.position = "right", text = element_text(size = 18))

ggsave("results/figures/gwas.pdf", gwas, width = 15, height = 15)
