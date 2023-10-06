library(tidyverse)
library(galah)
library(janitor)
library(arrow)
library(here)
library(APCalign)
library(skimr)


#configure Galah
# galah_config(email = "josh.lee2415@gmail.com")

# #get ALA data
# plants <- 
#   galah_call() |> 
#   galah_identify("Plantae") |> 
#   galah_apply_profile("CSDM") |> 
#   galah_filter(decimalLatitude != "", 
#                species != "") |> 
#   galah_select(recordID, species, genus,family,
#                decimalLatitude,decimalLongitude,coordinateUncertaintyInMeters,
#                eventDate,datasetName,basisOfRecord,
#                references,institutionCode,recordedBy) |> 
#    atlas_occurrences() 
# 
# #write.csv(plants, "data/processed/ala_all.csv", row.names = FALSE)
# write_parquet(plants, paste0("data/processed/ala_all_", Sys.Date(), ".parquet"))

#now read in the parquet 
#ala_all <- open_dataset(sources = here("data/processed/2023-08-23"), format = "parquet") 
#ala_all <- read.csv("data/processed/ala_all.csv")
ala_parquet <- read_parquet("data/processed/2023-08-23.parquet")



#get project species list
sp_list <- read.csv("data/processed/native_list.csv") %>% 
  left_join(ala_parquet)

#41 species with mismatched names. Look for them online and score manually
sp_list %>% 
  filter(is.na(recordID)) %>% 
  select(species) %>% 
  distinct() %>% 
  arrange() -> mismatched_names
write.csv(mismatched_names, "data/processed/mismatched_names.csv", row.names = FALSE)

#read in the new names and merge back to get occurrences

new_names <- read.csv("data/processed/matched_names.csv") %>% 
  rename(apc_names = species) %>% 
  mutate(species = gsub("<a0>", " ", new_names$ala_names)) %>% 
  select(-c(X, ala_names)) 

extracting_obs_mismatched_names <- new_names %>% 
  right_join(ala_parquet, by = "species") %>% 
  filter(!is.na(apc_names))

#append to the names that were fine
obs_for_climate <- sp_list %>% 
  filter(!is.na(recordID)) %>% 
  bind_rows(extracting_obs_mismatched_names) %>% 
  mutate(apc_names = coalesce(apc_names, species))

#I need to get an Aus shapefile to clip here because I've got coord all over the place

write_parquet(obs_for_climate, paste0("data/processed/obs_for_climate_calc_", Sys.Date(), ".parquet"))





# #map a species to see if it makes sense (Boronia serrulata had an outlier so look at that)
# library(sf)
# bose <- sp_list %>% 
#   filter(species == "Boronia serrulata") %>% 
#   st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
#            crs = st_crs(nsw))
# nsw <-  read_sf("data/raw/nsw_state_polygon_shp_gda2020/nsw_state_polygon_shp_gda2020.shp")
# 
# ggplot()+
#   geom_sf(data = nsw) +
#   geom_sf(data = bose) +
#   xlim(150, 152) +
#   ylim(35, 32)

