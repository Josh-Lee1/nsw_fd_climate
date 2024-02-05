library(tidyverse)
library(fundiversity)

#get the extinction results and compare #species lost
fd_extinction <- read.csv("data/processed/FD/fundiversity/fundiv_metrics_extinction_bct.csv") %>% 
  select(-c(X)) %>% 
  mutate(scenario = "future_loss")
fd_original <- read.csv("data/processed/FD/fundiversity/fundiv_metrics_current_bct.csv") %>% 
  select(-c(X)) %>% 
  mutate(scenario = "current")

srich_change <- fd_extinction %>% 
  select(site, sprich_ex = sp_richness) %>% 
  left_join(fd_original) %>% 
  mutate(sprich_change = sp_richness-sprich_ex) %>% 
  select(ParentGlobalID = site, sprich_ex, sprich_change)

#get original data and drop species randomly 
original_plot_data <- readRDS("data/processed/BCT_fundiversity/native_bct_with_margins.rds") %>% 
  select(-c(geometry)) %>% 
  group_by(ParentGlobalID, species) %>%
  summarise(cover = sum(Cover..)) %>%
  ungroup() %>% 
  left_join(srich_change) %>% 
  group_by(ParentGlobalID) %>% 
  mutate(sp_before_sample = n_distinct(species)) %>% 
  ungroup()

# get group specific sample sizes
n_target_sp <- original_plot_data %>% 
  select(ParentGlobalID, sprich_ex) %>% 
  distinct()

nested_plot_data <- original_plot_data %>%
  group_by(ParentGlobalID) %>%   # prep for work by Species
  nest() %>%              # --> one row per Species
  ungroup() %>% # add sample sizes
  left_join(n_target_sp) %>% 
  filter(!is.na(sprich_ex))

sampled_plot_data <- nested_plot_data %>%
  mutate(samp = map2(data, sprich_ex, sample_n))

plot_data_random_drop <- sampled_plot_data %>% 
  select(-data, -sprich_ex) %>%
  unnest(samp) %>% 
  group_by(ParentGlobalID) %>% 
  mutate(sp_after_sample = n_distinct(species)) %>% 
  ungroup()

#prepare for FD calculations
library(FD)

#get traits
traits <- read.csv("data/raw/BHPMF_diaz6_v2.csv") %>% 
  select(-c(X)) %>% 
  mutate(species = str_replace(species, "_", " ")) 

#wrangle traits for FD package
traits <- plot_data_random_drop %>% 
  left_join(traits, by = "species") %>% 
  select(species,
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
  select(ParentGlobalID,
         species,
         cover)  %>%
  drop_na() %>%
  distinct() %>%
  group_by(ParentGlobalID, species) %>%
  summarise(cover = sum(cover)) %>%
  ungroup() %>%
  filter(species %in% targ) %>%
  arrange(species)%>%
  mutate(cover = ifelse(cover != 0, 1, cover)) %>%
  filter(cover != 0) %>% 
  pivot_wider(names_from = species,
              values_from = cover,
              values_fill = 0) %>%
  column_to_rownames(var = "ParentGlobalID")


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


write.csv(result, "data/processed/FD/fundiversity/fundiv_metrics_random_bct.csv")



