library(tidyverse)
library(FD)

#get the cleaned floristics data
obs <- read.csv("data/processed/cleaned_benchmark_data.csv") %>% 
  filter(native_anywhere_in_aus == "considered native to Australia by APC" |
           native_anywhere_in_aus == "Native")

##get a species list for nativeness and trait filtering 
sp_list <- obs %>% 
  select(species = apc_name) %>% 
  distinct() %>% 
  drop_na()

#wrangle traits for FD package
gap_filled_diaz_traits <- read.csv("data/raw/BHPMF_diaz6_v2.csv") %>% 
  select(species,
         log10_plant_height,
         log10_wood_density,
         log10_leaf_area,
         log10_leaf_mass_per_area,
         log10_seed_dry_mass,
         log10_leaf_N_per_dry_mass)%>% 
  mutate(species = str_replace(species, "_", " ")) %>% 
  right_join(sp_list) %>% 
  drop_na() %>% 
  arrange(species) %>%
  column_to_rownames(var = "species")

target_list <- gap_filled_diaz_traits %>%
  rownames_to_column(var = "species") %>% 
  select(species) %>%
  distinct()
targ <- target_list$species

#write.csv(target_list, "data/processed/native_list.csv", row.names = FALSE)

#wrangle sites for FD package

sites <- obs %>%
  select(CensusKey,
         species = apc_name,
         CoverScore)  %>%
  drop_na() %>%
  distinct() %>%
  group_by(CensusKey, species) %>%
  summarise(cover = sum(CoverScore)) %>%
  ungroup() %>%
  filter(species %in% targ) %>%
  arrange(species) %>%
  pivot_wider(names_from = species,
              values_from = cover,
              values_fill = 0) %>%
  column_to_rownames(var = "CensusKey")

#calculate fd
fd <- dbFD(gap_filled_diaz_traits, sites, w.abun = TRUE)
FD <- fd %>%
  as.data.frame() %>%
  rownames_to_column(var = "site")
# write.csv(FD, "data/processed/FD/bnfs_benchmark_plots_native.csv")