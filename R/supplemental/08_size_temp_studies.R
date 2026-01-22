# ============================================================
# 08_size_temp_studies.R
# Purpose: Plot size-temperature trends from older lab rearing
# studies
# ============================================================

library(ggplot2)

#data manually extracted from
# Shirley SM, Shirley TC, Rice SD (1987) Latitudinal variation in the Dungeness crab, 
# Cancer magister: zoeal morphology explained by incubation temperature. Marine Biology 95:371–376

# Sulkin SD, McKeen G (1994) Influence of temperature on larval development of 
# four co-occurring species of the brachyuran genus Cancer. Mar Biol 118:593–600. https://doi.org/10.1007/BF00347506

# Sulkin SD, Mojica E, McKeen GL (1996) Elevated summer temperature effects on megalopal 
# and early juvenile development in the Dungeness crab,Cancer magister. Can J Fish Aquat Sci 53:2076–2079. https://doi.org/10.1139/f96-137

data <- read.delim("data/size_temperature.txt", header = TRUE)

size_temp <- ggplot(data, aes(x = Temperature, y = Size, color = Study)) +
  geom_point() +
  geom_line() +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(x = "Temperature (°C)",y = "Size") +
  theme(panel.border = element_rect(linetype = "solid", linewidth = 0.75, fill = NA),
        text = element_text(size = 18)) +
  facet_wrap(~ Study, scales = "free", 
             labeller = labeller(Study = label_wrap_gen(width = 20)))

ggsave("results/figures/size_temp.pdf", size_temp, width = 20, height = 15)
