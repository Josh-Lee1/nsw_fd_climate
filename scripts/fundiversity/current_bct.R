library(tidyverse)
library(fundiversity)

#get the cleaned floristics data
obs <- readRDS("data/processed/BCT_fundiversity/native_bct_with_margins.rds")


##get a species list for nativeness and trait filtering 
sp_list <- obs %>% 
  select(species) %>% 
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
  select(ParentGlobalID,
         species,
         Cover..)  %>%
  drop_na() %>%
  distinct() %>%
  group_by(ParentGlobalID, species) %>%
  summarise(cover = sum(Cover..)) %>%
  ungroup() %>%
  filter(species %in% targ) %>%
  arrange(species)%>%
  mutate(cover = ifelse(cover != 0, 1, cover)) %>%
  pivot_wider(names_from = species,
              values_from = cover,
              values_fill = 0) %>%
  column_to_rownames(var = "ParentGlobalID")

#get species richness to join on the end
obs %>% 
  group_by(ParentGlobalID) %>% 
  summarise(sp_richness = n_distinct(species)) %>% 
  rename(site = "ParentGlobalID") -> sp_rich

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
result <- Reduce(function(x, y) left_join(x, y, by = "site"), data_frames)


write.csv(result, "data/processed/FD/fundiversity/fundiv_metrics_current_BCT.csv")
