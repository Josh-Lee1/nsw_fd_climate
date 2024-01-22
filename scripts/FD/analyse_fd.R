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
# random_hie <- read.csv("data/processed/FD/fd_random_drop_HIE.csv") %>% 
#   select(-X) %>% 
#   mutate(scenario = "random")

#merge all the dfs

fd_dfs <- rbind(extinctions, random) %>% 
  left_join(current, by = c("site" = "site_current")) %>% 
  mutate(nbsp_change = nbsp_current - nbsp) %>% 
  mutate(FRic_change = FRic_current - FRic) %>% 
  mutate(FEve_change = FEve_current - FEve) %>% 
  mutate(FDis_change = FDis_current - FDis) %>% 
  mutate(nbsp_prop = nbsp/nbsp_current) %>% 
  mutate(FRic_prop_change = 1- (FRic/FRic_current)) %>% 
  mutate(nbsp_prop_change = 1-(nbsp/nbsp_current)) %>% 
  mutate(FDis_prop_change = 1- (FDis/FDis_current)) %>% 
  mutate(FEve_prop_change = 1- (FEve/FEve_current)) 

ggplot(fd_dfs, aes(nbsp_prop_change, FDis_change, colour = scenario)) +
  geom_point()  +
  geom_smooth(method = "lm") +
  xlab("% species richness loss") +
  ylab("difference in functional dispersion") +
  theme_bw()

#going to try change in metric 
fd_dfs %>% 
  group_by(site) %>% 
  
  
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

cwm_traits_with_info <- fd_with_info %>% 
  select(site, Elevation..m.:rain_percentile) %>% 
  distinct() %>% 
  left_join(cwm_traits, by = "site")
ggplot(cwm_traits_with_info, aes(trait, value, fill = scenario)) +
  geom_boxplot()+
  theme_bw() +
  facet_wrap(~rain_percentile)
  

#have a look by veg type or rainfall
#bring back plot info
bnfs_raw <- read.csv("data/raw/bionet_flora_survey/vegetation-condition-benchmarks-cover-and-richness-raw-data-v1-2.csv") %>% 
  mutate(CensusID = as.character(CensusID))
plot_info <- read.csv("data/raw/bionet_flora_survey/CensusList.csv") %>% 
  mutate(CensusID = as.character(Census.DB.ID)) %>% 
  right_join(bnfs_raw, by = "CensusID") %>% 
  select(site = Census.Key,
         Elevation..m.:Bioregion,
         IBRA7:Class.IBRA,
         Rain12) %>% 
  distinct() %>% 
  mutate(rain_percentile = cut(Rain12, 
                              quantile(Rain12, probs = seq(0, 1, 0.1), na.rm = TRUE), 
                              labels = seq(10, 100, 10), 
                              include.lowest = TRUE))
fd_with_info <- fd_dfs %>% 
  full_join(plot_info, by = "site")

#veg type

ggplot(fd_with_info, aes(nbsp_prop_change, FRic_prop_change, colour = scenario)) +
  geom_point()  +
  geom_smooth(method = "lm") +
  xlab("species richness loss") +
  ylab("functional richness loss") +
  theme_bw() +
  facet_wrap(~VegetationFormation)

#Rainfall
ggplot(fd_with_info, aes(nbsp_prop_change, FRic_prop_change, colour = scenario)) +
  geom_point()  +
  geom_smooth(method = "lm") +
  xlab("species richness loss") +
  ylab("functional richness loss") +
  theme_bw() +
  facet_wrap(~rain_percentile)

#compare fd with fundiversity 
fundiv_fric <- read.csv("data/processed/FD/fundiversity/fundiversity_frich.csv") %>% 
  select(-X) %>% 
  left_join(current, by = "site")

ggplot(fundiv_fric, aes(FRic.y, FRic.x)) +
  geom_point()

