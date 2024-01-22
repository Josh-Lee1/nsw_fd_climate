library(tidyverse)
library(sf)

#margins
clim_margins <- read_rds("data/processed/climate_margins_raw_names.rds") %>% 
  select(-c(geometry))
#plot climate
plots_climate <- read.csv("data/processed/bnfs_plots_with_current_future_climate.csv")

#get traits
traits <- read.csv("data/raw/BHPMF_diaz6_v2.csv") %>% 
  select(-c(X)) %>% 
  mutate(species = str_replace(species, "_", " ")) 

#plot data and join to site and species climate info
plot_data <- read.csv("data/processed/cleaned_benchmark_data.csv") %>% 
  select(-c(X)) %>% 
  left_join(plots_climate) %>% 
  rename(species = apc_name) %>% 
  left_join(clim_margins) %>% 
  group_by(CensusKey) %>% 
  mutate(original_sp_richness = n_distinct(species)) %>% 
  ungroup()

#plot data with extinction (rain and temp)
future_plot_data <- plot_data %>% 
  filter(!(tas_2075_4.5 >= p98_tas_historical |
           tas_2075_4.5 <= p02_tas_historical
           # |
           # precip_2075_4.5 >= p98_precip_historical | 
           # precip_2075_4.5 <= p02_precip_historical
           )
         ) %>% 
  group_by(CensusKey) %>% 
  mutate(future_sp_richness = n_distinct(species)) %>% 
  ungroup() %>% 
  mutate(richness_loss = original_sp_richness - future_sp_richness)

#drop non-natives and join to traits as previous for FD calculation
future_plot_data_for_fd <- future_plot_data %>% 
  filter(native_anywhere_in_aus == "considered native to Australia by APC" |
           native_anywhere_in_aus == "Native") %>% 
  left_join(traits)

saveRDS(future_plot_data_for_fd, "data/processed/FD/future_tas_only_plot_data_for_fd.rds")
#write.csv(future_plot_data_for_fd, "data/processed/FD/future_plot_data_for_fd.csv", row.names = FALSE)
