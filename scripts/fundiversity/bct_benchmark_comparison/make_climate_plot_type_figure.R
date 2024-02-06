library(tidyverse)
library(patchwork)
library(ggblend)

#benchmark data
fd_benchmark <- read.csv("data/processed/FD/fundiversity/fundiv_metrics_benchmarks.csv") %>% 
  select(-X)
fd_random <- read.csv("data/processed/FD/fundiversity/fundiv_metrics_random.csv") %>% 
  select(-X) %>% 
  mutate(scenario = "random")
fd_extinction <- read.csv("data/processed/FD/fundiversity/fundiv_metrics_extinction.csv") %>% 
  select(-X) %>% 
  mutate(scenario = "extinction")
rich <- read.csv("data/processed/FD/fundiversity/richness.csv") %>% 
  select(-X)

suffix1 <- "_current"
new_column_names <- paste0(names(fd_benchmark), suffix1)
colnames(fd_benchmark) <- new_column_names


fd_dfs_fundiv <- rbind(fd_extinction, fd_random) %>% 
  left_join(fd_benchmark, by = c("site" = "site_current")) %>% 
  left_join(rich) %>% 
  mutate(nbsp_change = nbsp_current - nbsp) %>% 
  mutate(FRic_change = FRic_current - FRic) %>% 
  mutate(FEve_change = FEve_current - FEve) %>% 
  mutate(FDis_change = FDis_current - FDis) %>% 
  mutate(FDiv_change = FDiv_current - FDiv) %>% 
  mutate(Q_change = Q_current - Q) %>% 
  mutate(nbsp_prop = nbsp/nbsp_current) %>% 
  mutate(FRic_prop_change = 1- (FRic/FRic_current)) %>% 
  mutate(nbsp_prop_change = 1-(nbsp/nbsp_current)) %>% 
  mutate(FDis_prop_change = 1- (FDis/FDis_current)) %>% 
  mutate(FEve_prop_change = 1- (FEve/FEve_current)) %>% 
  mutate(FDiv_prop_change = 1- (FDiv/FDiv_current)) %>% 
  mutate(Q_prop_change = 1- (Q/Q_current)) %>% 
  mutate(source = "benchmark")

#bct data
fd_current_bct <- read.csv("data/processed/FD/fundiversity/fundiv_metrics_current_BCT.csv") %>% 
  select(-X)
fd_extinction_bct <- read.csv("data/processed/FD/fundiversity/fundiv_metrics_extinction_bct.csv") %>% 
  select(-X) %>% 
  mutate(scenario = "extinction")
extinction_richness <- fd_extinction_bct %>% 
  select(site, sp_richness)
fd_random_bct <- read.csv("data/processed/FD/fundiversity/fundiv_metrics_random_bct.csv") %>% 
  select(-X) %>% 
  mutate(scenario = "random") %>% 
  left_join(extinction_richness)
rich <- read.csv("data/processed/FD/fundiversity/richness.csv") %>% 
  select(-X)

suffix1 <- "_current"
new_column_names <- paste0(names(fd_current_bct), suffix1)
colnames(fd_current_bct) <- new_column_names


fd_dfs_fundiv_bct <- rbind(fd_extinction_bct, fd_random_bct) %>% 
  left_join(fd_current_bct, by = c("site" = "site_current")) %>% 
  rename(nbsp = sp_richness,
         nbsp_current = sp_richness_current) %>% 
  mutate(nbsp_change = nbsp_current - nbsp) %>% 
  mutate(FRic_change = FRic_current - FRic) %>% 
  mutate(FEve_change = FEve_current - FEve) %>% 
  mutate(FDis_change = FDis_current - FDis) %>% 
  mutate(FDiv_change = FDiv_current - FDiv) %>% 
  mutate(Q_change = Q_current - Q) %>% 
  mutate(nbsp_prop = nbsp/nbsp_current) %>% 
  mutate(FRic_prop_change = 1- (FRic/FRic_current)) %>% 
  mutate(nbsp_prop_change = 1-(nbsp/nbsp_current)) %>% 
  mutate(FDis_prop_change = 1- (FDis/FDis_current)) %>% 
  mutate(FEve_prop_change = 1- (FEve/FEve_current)) %>% 
  mutate(FDiv_prop_change = 1- (FDiv/FDiv_current)) %>% 
  mutate(Q_prop_change = 1- (Q/Q_current)) %>% 
  mutate(source = "bct")

##########

both <- bind_rows(fd_dfs_fundiv_bct, fd_dfs_fundiv)

df <- fd_dfs_fundiv_bct %>% group_by(site) %>% mutate(Difference = c(NA, diff(FRic)))
ggplot(df, aes(nbsp_prop_change, Difference)) + 
  geom_point()

df1 <- both %>% group_by(site) %>% 
  mutate(Difference = c(NA, diff(FRic))) %>% 
  arrange(desc(source))


ggplot(df1, aes(nbsp_prop_change, Difference, colour = source)) + 
  geom_point()+ 
  ylim(-0.2, 0.2)+
  theme_bw() +
  ylab("Difference in Functional Richness between random and climate species loss") +
  xlab("Change in proportion of species")
  
#get veg_form involved

bct <- read.csv("data/processed/BCT_fundiversity/BCT_fundiv_w_form.csv") %>% 
  select(site = GlobalID, vegetation_formation)
bench <- read.csv("data/processed/FD/fundiversity/fundiv_bench_with_form.csv") %>% 
  select(site, vegetation_formation = "VegetationFormation") %>% 
  bind_rows(bct)

df1 <- left_join(df1, bench, by = "site")

ggplot(data=subset(df1, !is.na(vegetation_formation)), aes(vegetation_formation, Difference, colour = source)) + 
  geom_boxplot()+
  ylab("Difference in Functional Richness between random and climate species loss") +
  xlab("")+ 
  coord_flip()+
  theme_bw()

