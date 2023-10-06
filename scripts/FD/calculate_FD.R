# Calculate some fd for all
# Name match 
# Connect to gap filled diaz 6
# Assimilate subsp to sp
# Remove introduced
# Clean/wrangle

library(tidyverse)
library(APCalign)
library(FD)

#get species x plot data (I have removed obs without cover ~10%, and subsp. ~1000taxa)
benchmark_subset <- read.csv("data/raw/bionet_flora_survey/condition_benchmarks_withspecies.csv") %>% 
  select(-c(X)) %>% 
  rename(original_name = "ScientificName") %>% 
  filter(!is.na(CoverScore)) %>% 
  filter(!grepl('spp.', original_name)) %>% 
  filter(!grepl("sp. . ", original_name)) %>% 
  filter(!grepl(" sp. .", original_name)) %>% 
  filter(!grepl("species", original_name)) %>% 
  filter(!grepl("\\d+", original_name)) %>% 
  filter(!grepl("indeterminate", original_name))

#use ausflora to align taxonomy and identify non-natives
##tax
resources <- APCalign::load_taxonomic_resources()
#lookup start 11:52am ... fin 12:01
lookup <- APCalign::create_taxonomic_update_lookup(benchmark_subset$original_name,
                                         resources = resources)
benchmark_subset_withapc <- dplyr::left_join(benchmark_subset, lookup)


##get a species list for nativeness and trait filtering 
sp_list <- benchmark_subset_withapc %>% 
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

##nativeness

good_names <- target_list %>% 
  filter(!grepl(".-.", species))
no_dash_names <- good_names$species
native_or_not<- native_anywhere_in_australia(no_dash_names)

#fix no dashes(-) names, 24 with I can score manually. see below
# bad_names <- target_list %>% 
#   filter(str_count(species, '\\w+') >2)
#write 
dashed_names_scored <- read.csv("data/processed/names_to_score_native.csv") %>% 
  rename(native_anywhere_in_aus = status) %>% 
  select(-c(X)) 

#note that here there are 5 species that could not find nativeness could score mannually
nativeness_with_all_else <- native_or_not %>% 
  bind_rows(dashed_names_scored) %>% 
  rename(apc_name = species) %>% 
#  left_join(lookup) %>% 
  right_join(benchmark_subset_withapc)


######



#wrangle sites for FD package

sites <- benchmark_subset_withapc %>%
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

#this script should run now with natives and non natives. Going to new script to remove natives
#calculate fd
fd <- dbFD(gap_filled_diaz_traits, sites, w.abun = TRUE)
FD <- fd %>%
  as.data.frame() %>%
  rownames_to_column(var = "site")
write.csv(FD, "data/processed/FD/______.csv")
