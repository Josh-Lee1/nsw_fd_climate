library(tidyverse)
library(fundiversity)

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
  arrange(species)%>%
  mutate(cover = ifelse(cover != 0, 1, cover)) %>%
  filter(cover != 0) %>% 
  pivot_wider(names_from = species,
              values_from = cover,
              values_fill = 0) %>%
  column_to_rownames(var = "CensusKey")


#make into sparse matrix for faster computing
sites_matrix<- as.matrix(sites)
sparse_site_sp <- Matrix::Matrix(sites_matrix, sparse = TRUE)


#calculate fd
#may need this to run the code
options(future.globals.maxSize= 1417*1024^2)

fric<- fd_fric(traits, sparse_site_sp)
feve<- fd_feve(traits, sparse_site_sp)
fdis<- fd_fdis(traits, sparse_site_sp)
fdiv<- fd_fdiv(traits, sparse_site_sp)
frao<- fd_raoq(traits, sparse_site_sp)

data_frames <- list(fric, feve, fdis, fdiv, frao)  # Create a list of data frames
result <- Reduce(function(x, y) left_join(x, y, by = "site"), data_frames)


write.csv(result, "data/processed/FD/fundiversity/fundiv_metrics_random.csv")



