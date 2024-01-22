library(tidyverse)
library(fundiversity)

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

future::plan(future::multisession, workers = 4)
fric<- fd_fric(traits, sparse_site_sp)
feve<- fd_feve(traits, sparse_site_sp)
fdis<- fd_fdis(traits, sparse_site_sp)
fdiv<- fd_fdiv(traits, sparse_site_sp)
frao<- fd_raoq(traits, sparse_site_sp)

data_frames <- list(fric, feve, fdis, fdiv, frao)  # Create a list of data frames
result <- Reduce(function(x, y) left_join(x, y, by = "site"), data_frames)


write.csv(result, "data/processed/FD/fundiversity/fundiv_metrics_extinction.csv")

#test parallelisation 
microbenchmark::microbenchmark(
  seq = { 
    future::plan(future::sequential) 
    fd_fric(traits, sparse_site_sp) 
  },
  multisession = { 
    future::plan(future::multisession, workers = 4)
    fd_fric(traits, sparse_site_sp) 
  },
  multicore = { 
    future::plan(future::multicore, workers = 4) 
    fd_fric(traits, sparse_site_sp) 
  }, times = 20
)

# Unit: seconds
# expr        min        lq      mean    median        uq       max neval
# seq   9.494626  13.10664  30.92582  21.75846  42.52860 118.93702    20
# multisession 123.027865 131.92013 184.34219 162.74028 221.95686 339.68907    20
# multicore   8.443992  11.68605  17.78569  16.92989  20.09958  36.24547    20
