# ============================================================
# 05_genes_vs_env_modeling.R
# Purpose: Linear mixed-effects modeling of temperature and 
# genomic relationships with carapace length
# ============================================================

library(tidyverse)
library(lme4)
library(ggplot2)
library(ggeffects)
library(cowplot)

# ---- Load datasets ----

crab_data2023 <- readRDS("results/outputs/crab_data2023_sst.rds")
pca.vectors <- readRDS("results/outputs/pca_vectors.rds")

# ---- Merge datasets ----

crab_data2023 <- crab_data2023 %>%
  mutate(
    sample = paste0(Site, "23_", str_pad(Vial, width = 3, side = "left", pad = "0"))
  ) %>%
  distinct(sample, .keep_all = TRUE)

pca.gen <- pca.vectors %>%
  select(sample, starts_with("X"), pop)

crab_pca_temp <- crab_data2023 %>%
  left_join(pca.gen, by = "sample")

filtered_data <- crab_pca_temp %>%
  filter(pop %in% c("COR", "MST", "NHB", "PTW", "SMS", "SQR", "WTP", "FHL"))

#complete dataset with no missing data
data <- filtered_data %>%
  filter(!is.na(X1), !is.na(Mean_28_Day_V15), !is.na(CL))

# ---- Models ----
m_env  <- lmer(CL ~ scale(Mean_28_Day_V15) + (1|Site),
               data = data, REML = FALSE)

m_gen  <- lmer(CL ~ scale(X1) + (1|Site),
               data = data, REML = FALSE)

m_both <- lmer(CL ~ scale(Mean_28_Day_V15) + scale(X1) + (1|Site),
               data = data, REML = TRUE)

# ---- Comparisons ----
anova(m_gen, m_both)   
anova(m_env, m_both)   
summary(m_both)

# ---- Predicted effects ----
effect_temp <- as_tibble(ggpredict(m_both, terms = "Mean_28_Day_V15 [all]"))
effect_pc1  <- as_tibble(ggpredict(m_both, terms = "X1 [all]"))

# ---- Plotting ----

y_min <- min(c(effect_temp$conf.low, effect_pc1$conf.low))
y_max <- max(c(effect_temp$conf.high, effect_pc1$conf.high))

#plot temp effects
p_temp <- ggplot(effect_temp, aes(x = x, y = predicted)) +
  geom_line(color = "#E69F00", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#E69F00", alpha = 0.2) +
  labs(x = "Mean 28-Day Prior SST (°C)", y = "Carapace Length (mm)") +
  ylim(y_min, y_max) +
  theme_classic() +
  theme(text = element_text(size = 20))

#Plot PC1 effects
p_pc1 <- ggplot(effect_pc1, aes(x = x, y = predicted)) +
  geom_line(color = "#56B4E9", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#56B4E9", alpha = 0.2) +
  labs(x = "Genomic PC1", y = "Carapace Length (mm)") +
  ylim(y_min, y_max) +
  theme_classic() +
  theme(text = element_text(size = 20))

#combine plots
effects_plot <- plot_grid(p_temp, p_pc1, ncol = 2, align = "h", labels = "AUTO", label_size = 16)

ggsave("results/figures/Figure_effects.pdf", effects_plot, width = 20, height = 10)
