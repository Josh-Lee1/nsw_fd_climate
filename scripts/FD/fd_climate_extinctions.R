library(tidyverse)
library(FD)

#get the plot data with removed 
obs <- read.csv("data/processed/FD/future_plot_data_for_fd.csv") 

#wrangle traits for FD package
traits <- obs %>% 
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

sites <- obs %>%
  select(CensusKey,
         species,
         CoverScore)  %>%
  drop_na() %>%
  distinct() %>%
  group_by(CensusKey, species) %>%
  summarise(cover = sum(CoverScore)) %>%
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
# write.csv(FD, "data/processed/FD/fd_after_extinctions.csv")