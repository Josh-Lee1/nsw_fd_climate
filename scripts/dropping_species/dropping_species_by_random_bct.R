library(tidyverse)

#get the extinction results and compare #species lost
fd_extinction <- read.csv("data/processed/FD/fd_after_extinctions.csv") %>% 
  select(-c(X)) %>% 
  mutate(scenario = "future_loss")
fd_original <- read.csv("data/processed/FD/bnfs_benchmark_plots_native.csv") %>% 
  select(-c(X)) %>% 
  mutate(scenario = "current")

srich_change <- fd_extinction %>% 
  select(site, nbsp_ex = nbsp) %>% 
  left_join(fd_original) %>% 
  mutate(srich_change = nbsp-nbsp_ex) %>% 
  select(CensusKey = site, nbsp_ex, srich_change)

#get original bnfs data and drop species randomly 
original_plot_data <- read.csv("data/processed/cleaned_benchmark_data.csv") %>% 
  select(-c(X))  %>% 
  filter(native_anywhere_in_aus == "considered native to Australia by APC" |
           native_anywhere_in_aus == "Native") %>%
  group_by(CensusKey, apc_name) %>%
  summarise(cover = sum(CoverScore)) %>%
  ungroup() %>% 
  left_join(srich_change) %>% 
  filter(!is.na(srich_change))%>% 
  group_by(CensusKey) %>% 
  mutate(sp_before_sample = n_distinct(apc_name)) %>% 
  ungroup()

# writing a function for group specific sample sizes. there might be a better way to do this...
n_target_sp <- original_plot_data %>% 
  select(CensusKey, nbsp_ex) %>% 
  distinct()

nested_plot_data <- original_plot_data %>%
  group_by(CensusKey) %>%   # prep for work by Species
  nest() %>%              # --> one row per Species
  ungroup() %>% # add sample sizes
  left_join(n_target_sp)

sampled_plot_data <- nested_plot_data %>%
  mutate(samp = map2(data, nbsp_ex, sample_n))

plot_data_random_drop <- sampled_plot_data %>% 
  select(-data, -nbsp_ex) %>%
  unnest(samp) %>% 
  group_by(CensusKey) %>% 
  mutate(sp_after_sample = n_distinct(apc_name)) %>% 
  ungroup()

#prepare for FD calculations
library(FD)

#get traits
traits <- read.csv("data/raw/BHPMF_diaz6_v2.csv") %>% 
  select(-c(X)) %>% 
  mutate(apc_name = str_replace(species, "_", " ")) 

#wrangle traits for FD package
traits <- plot_data_random_drop %>% 
  left_join(traits, by = "apc_name") %>% 
  select(species = apc_name,
         log10_plant_height,
         log10_wood_density,
         log10_leaf_area,
         log10_leaf_mass_per_area,
         log10_seed_dry_mass,
         log10_leaf_N_per_dry_mass) %>% 
  distinct() %>% 
  drop_na() %>% 
  arrange(species) %>%
  column_to_rownames(var = "species")

target_list <- traits %>%
  rownames_to_column(var = "species") %>% 
  select(species) %>%
  distinct()
targ <- target_list$species

#wrangle sites for FD package

sites <- plot_data_random_drop %>%
  select(CensusKey,
         species = apc_name,
         cover)  %>%
  drop_na() %>%
  distinct() %>%
  group_by(CensusKey, species) %>%
  summarise(cover = sum(cover)) %>%
  ungroup() %>%
  filter(species %in% targ) %>%
  arrange(species) %>%
  filter(cover != 0) %>% 
  pivot_wider(names_from = species,
              values_from = cover,
              values_fill = 0) %>%
  column_to_rownames(var = "CensusKey")

#calculate fd
fd <- dbFD(traits, sites, w.abun = TRUE)
FD <- fd %>%
  as.data.frame() %>%
  rownames_to_column(var = "site")
write.csv(FD, "data/processed/FD/fd_random_drop.csv")




