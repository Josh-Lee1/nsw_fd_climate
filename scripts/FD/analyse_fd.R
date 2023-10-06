library(tidyverse)

#read fd results
extinctions <- read.csv("data/processed/FD/fd_after_extinctions.csv") %>% 
  select(-X) %>% 
  mutate(scenario = "extinction")
random <- read.csv("data/processed/FD/fd_random_drop.csv") %>% 
  select(-X) %>% 
  mutate(scenario = "random")
current <- read.csv("data/processed/FD/bnfs_benchmark_plots_native.csv") %>% 
  select(-X)
# Add the suffix to all column names
suffix1 <- "_current"
new_column_names <- paste0(names(current), suffix1)
colnames(current) <- new_column_names

#merge all the dfs

fd_dfs <- rbind(extinctions, random) %>% 
  left_join(current, by = c("site" = "site_current")) %>% 
  mutate(nbsp_change = nbsp_current - nbsp) %>% 
  mutate(FRic_change = FRic_current - FRic) %>% 
  mutate(FEve_change = FEve_current - FEve) %>% 
  mutate(FDis_change = FDis_current - FDis)

ggplot(fd_dfs, aes(nbsp_change, FRic_change, colour = scenario)) +
  geom_point()  +
  geom_smooth(method = "loess") +
  xlab("species richness loss") +
  ylab("functional richness loss") +
  theme_bw()

#cwm trait fig
site_list <- extinctions$site
current <- read.csv("data/processed/FD/bnfs_benchmark_plots_native.csv") %>% 
  select(-X)
cwm_traits <- current %>% 
  mutate(scenario = "current")%>% 
  filter(site %in% site_list) %>% 
  rbind(extinctions) %>% 
  pivot_longer(cols = CWM.log10_plant_height:CWM.log10_leaf_N_per_dry_mass,
               names_to = "trait")
ggplot(cwm_traits, aes(trait, value, fill = scenario)) +
  geom_boxplot()+
  theme_bw()




# split by rain fall plpots
# proportion of species
#proportion of fr as well



