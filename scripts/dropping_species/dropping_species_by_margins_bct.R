library(tidyverse)
library(sf)
library(ausflora)

#margins
clim_margins <- read_rds("data/processed/climate_margins_raw_names.rds") %>% 
  select(-c(geometry))
#plot climate
plots_climate <- read.csv("data/processed/bct_plots_with_current_future_climate.csv") %>% 
  rename(ParentGlobalID = GlobalID)

#get traits
traits <- read.csv("data/raw/BHPMF_diaz6_v2.csv") %>% 
  select(-c(X)) %>% 
  mutate(species = str_replace(species, "_", " ")) 

#plot data and join to site and species climate info
##quickly match to APC first
bct_species_list <- read.csv("data/raw/bct_plot_metadata/speciesobs.csv") %>% 
  select(original_name = displayname)
  
lookup <- create_taxonomic_update_lookup(bct_species_list$original_name)
sp_list_withapc<- dplyr::left_join(bct_species_list, lookup)


plot_data <- sp_list_withapc %>%
  left_join(plots_climate) %>% 
  rename(species = apc_name) %>% 
  left_join(clim_margins) %>% 
  group_by(ParentGlobalID) %>% 
  mutate(original_sp_richness = n_distinct(species)) %>% 
  ungroup()

#plot data with extinction (rain and temp)
future_plot_data <- plot_data %>% 
  filter(!(tas_2075_4.5 >= p98_tas_historical |
             tas_2075_4.5 <= p02_tas_historical
            |
            precip_2075_4.5 >= p98_precip_historical | 
            precip_2075_4.5 <= p02_precip_historical
  )
  ) %>% 
  group_by(ParentGlobalID) %>% 
  mutate(future_sp_richness = n_distinct(species)) %>% 
  ungroup() %>% 
  mutate(richness_loss = original_sp_richness - future_sp_richness)

#drop non-natives and join to traits as previous for FD calculation
native_or_not <- native_anywhere_in_australia(future_plot_data$species, resources = load_taxonomic_resources()) %>% 
  distinct()

future_plot_data_for_fd <- future_plot_data %>% 
  left_join(native_or_not, by = "species") %>% 
  filter(native_anywhere_in_aus == "TRUE") %>% 
  left_join(traits)

saveRDS(future_plot_data_for_fd, "data/processed/BCT_fundiversity/future_tas_only_plot_data_for_fd_bct.rds")
# write.csv(future_plot_data_for_fd, "data/processed/FD/future_plot_data_for_fd_bct.csv", row.names = FALSE)
