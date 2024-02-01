library(tidyverse)
library(forcats)

bct <- read.csv("data/processed/BCT_fundiversity/BCT_fundiv_w_form.csv")
bench <- read.csv("data/processed/FD/fundiversity/fundiv_bench_with_form.csv") %>% 
  rename(vegetation_formation = "VegetationFormation")

#species Richness
#calculate benchmarks
summary_stats <- bench %>%
  group_by(vegetation_formation) %>%
  summarise(
    n = n(),
    Mean = mean(sp_richness, na.rm = TRUE),
    SD = sd(sp_richness, na.rm = TRUE),
    Min = min(sp_richness, na.rm = TRUE),
    Percentile_10 = quantile(sp_richness, 0.1, na.rm = TRUE),
    Percentile_20 = quantile(sp_richness, 0.2, na.rm = TRUE),
    Median = quantile(sp_richness, 0.5, na.rm = TRUE),
    Percentile_80 = quantile(sp_richness, 0.8, na.rm = TRUE),
    Percentile_90 = quantile(sp_richness, 0.9, na.rm = TRUE),
    Max = max(sp_richness, na.rm = TRUE)
  ) %>% 
  mutate(plot_type = "benchmark")

#going to get the order of most sp rich veg forms to force orders across final plots
x <- summary_stats %>% 
  arrange(Mean) %>% 
  select(vegetation_formation)

summary_stats_bct <- bct %>%
  group_by(vegetation_formation) %>%
  summarise(
    n = n(),
    Mean = mean(sp_richness, na.rm = TRUE), 
    SD = sd(sp_richness, na.rm = TRUE),
    Min = min(sp_richness, na.rm = TRUE),
    Percentile_10 = quantile(sp_richness, 0.1, na.rm = TRUE),
    Percentile_20 = quantile(sp_richness, 0.2, na.rm = TRUE),
    Median = quantile(sp_richness, 0.5, na.rm = TRUE),
    Percentile_80 = quantile(sp_richness, 0.8, na.rm = TRUE),
    Percentile_90 = quantile(sp_richness, 0.9, na.rm = TRUE),
    Max = max(sp_richness, na.rm = TRUE)
  ) %>% 
  mutate(plot_type = "bct") %>% 
  filter(vegetation_formation != "")

#combine these
sp_rich <- bind_rows(summary_stats, summary_stats_bct)

#make linegraph 
  plot_sr <- sp_rich %>% 
    mutate(vegetation_formation = factor(vegetation_formation, levels = unique(.$vegetation_formation)))  %>%  
  ggplot(aes(x = vegetation_formation, y = Mean, colour = as.factor(plot_type))) +
  geom_pointrange(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    position = position_dodge(0.5),
    size = 1
  ) +
  labs(
    title = "Species Richness of BCT Monitoring vs Benchmarks",
    x = "Vegetation Formation",
    y = "Species Richness"
  ) +
  coord_flip() +
  theme_bw()+
    theme(legend.title = element_blank())
print(plot_sr)


#########
#Functional Richness
#calculate benchmarks
summary_stats <- bench %>%
  group_by(vegetation_formation) %>%
  summarise(
    n = n(),
    Mean = mean(FRic, na.rm = TRUE),
    SD = sd(FRic, na.rm = TRUE),
    Min = min(FRic, na.rm = TRUE),
    Percentile_10 = quantile(FRic, 0.1, na.rm = TRUE),
    Percentile_20 = quantile(FRic, 0.2, na.rm = TRUE),
    Median = quantile(FRic, 0.5, na.rm = TRUE),
    Percentile_80 = quantile(FRic, 0.8, na.rm = TRUE),
    Percentile_90 = quantile(FRic, 0.9, na.rm = TRUE),
    Max = max(FRic, na.rm = TRUE)
  ) %>% 
  mutate(plot_type = "benchmark")

summary_stats_bct <- bct %>%
  group_by(vegetation_formation) %>%
  summarise(
    n = n(),
    Mean = mean(FRic, na.rm = TRUE),
    SD = sd(FRic, na.rm = TRUE),
    Min = min(FRic, na.rm = TRUE),
    Percentile_10 = quantile(FRic, 0.1, na.rm = TRUE),
    Percentile_20 = quantile(FRic, 0.2, na.rm = TRUE),
    Median = quantile(FRic, 0.5, na.rm = TRUE),
    Percentile_80 = quantile(FRic, 0.8, na.rm = TRUE),
    Percentile_90 = quantile(FRic, 0.9, na.rm = TRUE),
    Max = max(FRic, na.rm = TRUE)
  ) %>% 
  mutate(plot_type = "bct") %>% 
  filter(vegetation_formation != "")

#combine these
fric_rich <- bind_rows(summary_stats, summary_stats_bct)

#make linegraph 
plot_fric <- fric_rich %>% 
  # mutate(ordering = -as.numeric(plot_type) + Mean,
  #        vegetation_formation = fct_reorder(vegetation_formation, ordering, .desc = T)) %>%  
  ggplot(aes(x = vegetation_formation, y = Mean, colour = as.factor(plot_type))) +
  geom_pointrange(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    position = position_dodge(0.5),
    size = 1
  ) +
  labs(
    title = "Functional Richness of BCT Monitoring vs Benchmarks",
    x = "Vegetation Formation",
    y = "Functional Richness"
  ) +
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank())
print(plot_fric)

#########
#Functional evenness
#calculate benchmarks
summary_stats <- bench %>%
  group_by(vegetation_formation) %>%
  summarise(
    n = n(),
    Mean = mean(FEve, na.rm = TRUE),  
    SD = sd(FEve, na.rm = TRUE),
    Min = min(FEve, na.rm = TRUE),
    Percentile_10 = quantile(FEve, 0.1, na.rm = TRUE),
    Percentile_20 = quantile(FEve, 0.2, na.rm = TRUE),
    Median = quantile(FEve, 0.5, na.rm = TRUE),
    Percentile_80 = quantile(FEve, 0.8, na.rm = TRUE),
    Percentile_90 = quantile(FEve, 0.9, na.rm = TRUE),
    Max = max(FEve, na.rm = TRUE)
  ) %>% 
  mutate(plot_type = "benchmark")

summary_stats_bct <- bct %>%
  group_by(vegetation_formation) %>%
  summarise(
    n = n(),
    Mean = mean(FEve, na.rm = TRUE),
    SD = sd(FEve, na.rm = TRUE),
    Min = min(FEve, na.rm = TRUE),
    Percentile_10 = quantile(FEve, 0.1, na.rm = TRUE),
    Percentile_20 = quantile(FEve, 0.2, na.rm = TRUE),
    Median = quantile(FEve, 0.5, na.rm = TRUE),
    Percentile_80 = quantile(FEve, 0.8, na.rm = TRUE),
    Percentile_90 = quantile(FEve, 0.9, na.rm = TRUE),
    Max = max(FEve, na.rm = TRUE)
  ) %>% 
  mutate(plot_type = "bct") %>% 
  filter(vegetation_formation != "")

#combine these
feve <- bind_rows(summary_stats, summary_stats_bct)

#make linegraph 
plot_feve <- feve %>% 
  # mutate(ordering = -as.numeric(plot_type) + Mean,
  #        vegetation_formation = fct_reorder(vegetation_formation, ordering, .desc = T)) %>%  
  ggplot(aes(x = vegetation_formation, y = Mean, colour = as.factor(plot_type))) +
  geom_pointrange(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    position = position_dodge(0.5),
    size = 1
  ) +
  labs(
    title = "Functional Evenness of BCT Monitoring vs Benchmarks",
    x = "Vegetation Formation",
    y = "Functional Evenness"
  ) +
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank())
print(plot_feve)


#########
#Functional dis
#calculate benchmarks
summary_stats <- bench %>%
  group_by(vegetation_formation) %>%
  summarise(
    n = n(),
    Mean = mean(FDis, na.rm = TRUE),
    SD = sd(FDis, na.rm = TRUE),
    Min = min(FDis, na.rm = TRUE),
    Percentile_10 = quantile(FDis, 0.1, na.rm = TRUE),
    Percentile_20 = quantile(FDis, 0.2, na.rm = TRUE),
    Median = quantile(FDis, 0.5, na.rm = TRUE),
    Percentile_80 = quantile(FDis, 0.8, na.rm = TRUE),
    Percentile_90 = quantile(FDis, 0.9, na.rm = TRUE),
    Max = max(FDis, na.rm = TRUE)
  ) %>% 
  mutate(plot_type = "benchmark")

summary_stats_bct <- bct %>%
  group_by(vegetation_formation) %>%
  summarise(
    n = n(),
    Mean = mean(FDis, na.rm = TRUE),
    SD = sd(FDis, na.rm = TRUE),
    Min = min(FDis, na.rm = TRUE),
    Percentile_10 = quantile(FDis, 0.1, na.rm = TRUE),
    Percentile_20 = quantile(FDis, 0.2, na.rm = TRUE),
    Median = quantile(FDis, 0.5, na.rm = TRUE),
    Percentile_80 = quantile(FDis, 0.8, na.rm = TRUE),
    Percentile_90 = quantile(FDis, 0.9, na.rm = TRUE),
    Max = max(FDis, na.rm = TRUE)
  ) %>% 
  mutate(plot_type = "bct") %>% 
  filter(vegetation_formation != "")

#combine these
fdis <- bind_rows(summary_stats, summary_stats_bct)

#make linegraph 
plot_fdis <- fdis %>% 
  # mutate(ordering = -as.numeric(plot_type) + Mean,
  #        vegetation_formation = fct_reorder(vegetation_formation, ordering, .desc = T)) %>%  
  ggplot(aes(x = vegetation_formation, y = Mean, colour = as.factor(plot_type))) +
  geom_pointrange(
    aes(ymin = Mean - SD, ymax = Mean + SD),
    position = position_dodge(0.5),
    size = 1
  ) +
  labs(
    title = "Functional Dispersion of BCT Monitoring vs Benchmarks",
    x = "Vegetation Formation",
    y = "Functional Dispersion"
  ) +
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank())
print(plot_fdis)

library(patchwork)

plot_sr + plot_fric +plot_feve + plot_fdis
