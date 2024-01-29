######### copied code from other project but shouldn't need major edits. 
#calculate fd using fundiversity on both BCT and benchmark to remake those boxplots
#will also then have a platform to simulate extinctions from
library(tidyverse)
library(fundiversity)
library(ausflora)

bct<- read.csv("data/raw/BCT_BAM_Beta1.csv") %>% 
  select(siteID = GlobalID) 

#name match
sp_list <- read.csv("data/raw/speciesobs.csv") %>% 
  select(siteID = ParentGlobalID, 
         original_name = displayname,
         cover = Cover..) %>% 
  left_join(bct, by = "siteID") %>% 
  drop_na() %>% 
  distinct() %>% 
  group_by(siteID, original_name) %>% 
  summarise(cover = sum(cover)) %>% 
  ungroup() %>% 
  arrange(original_name) %>% 
  filter(original_name != "")

resources <- load_taxonomic_resources()

lookup <- create_taxonomic_update_lookup(sp_list$original_name,
                                         resources = resources)
sp_list_withapc<- dplyr::left_join(sp_list, lookup)

#organise data for FD calculation

sp_list <- sp_list_withapc %>% 
  select(species = apc_name) %>% 
  distinct() %>% 
  drop_na()

native_list <- ausflora::native_anywhere_in_australia(sp_list$species, resources = resources)

sp_list_native_only <- sp_list %>% 
  left_join(native_list) %>% 
  filter(native_anywhere_in_aus == "TRUE") %>% 
  select(species)

gap_filled_diaz_traits <- read.csv("data/raw/BHPMF_diaz6_v2.csv") %>% 
  select(species,
         log10_plant_height,
         log10_wood_density,
         log10_leaf_area,
         log10_leaf_mass_per_area,
         log10_seed_dry_mass,
         log10_leaf_N_per_dry_mass)%>% 
  mutate(species = str_replace(species, "_", " ")) %>% 
  right_join(sp_list_native_only) %>% 
  drop_na() %>% 
  arrange(species) %>%
  column_to_rownames(var = "species")

target_list <- gap_filled_diaz_traits %>% 
  rownames_to_column(var = "species") %>% 
  select(species) %>% 
  distinct()
targ <- target_list$species
sites <- sp_list_withapc %>% 
  select(siteID, 
         species = apc_name,
         cover)  %>% 
  drop_na() %>% 
  distinct() %>% 
  group_by(siteID, species) %>% 
  summarise(cover = sum(cover)) %>% 
  ungroup() %>% 
  filter(species %in% targ) %>% 
  arrange(species) %>% 
  pivot_wider(names_from = species,
              values_from = cover,
              values_fill = 0) %>% 
  column_to_rownames(var = "siteID")

#get species richness to join on the end
sp_list_withapc %>% 
  group_by(siteID) %>% 
  summarise(sp_richness = n_distinct(apc_name)) %>% 
  rename(site = "siteID") -> sp_rich

#calculate fd

#make into sparse matrix for faster computing
sites_matrix<- as.matrix(sites)
sparse_site_sp <- Matrix::Matrix(sites_matrix, sparse = TRUE)


#calculate fd
#may need this to run the code
options(future.globals.maxSize= 1417*1024^2)

fric<- fd_fric(gap_filled_diaz_traits, sparse_site_sp)
feve<- fd_feve(gap_filled_diaz_traits, sparse_site_sp)
fdis<- fd_fdis(gap_filled_diaz_traits, sparse_site_sp)
fdiv<- fd_fdiv(gap_filled_diaz_traits, sparse_site_sp)
frao<- fd_raoq(gap_filled_diaz_traits, sparse_site_sp)

data_frames <- list(fric, feve, fdis, fdiv, frao)  # Create a list of data frames
result <- Reduce(function(x, y) left_join(x, y, by = "site"), data_frames) %>% 
  left_join(sp_rich)


write.csv(result, "data/processed/BCT_fundiversity/fundiv_metrics_bct_mon_sites.csv")

