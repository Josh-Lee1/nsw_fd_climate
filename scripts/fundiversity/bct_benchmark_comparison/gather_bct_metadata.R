library(tidyverse)
library(foreign)
library(sf)

#get monpoint data, ibra shape, and bct points and combine all data

mon <- read.dbf("data/raw/bct_plot_metadata/MonitoringPoint_280423.dbf") %>% 
  rename(MP.ID = "mon_point_")
ibra<- read_sf("data/raw/bct_plot_metadata/IBRA_NSW_v41/IBRA_NSW_v41.shp")
bct_sites<- read.csv("data/raw/BCT_BAM_Beta1.csv") %>% 
  st_as_sf(coords = c("x", "y"), crs = st_crs(ibra))
pct <- read.csv("data/raw/bct_plot_metadata/bionet-plant-community-type-data.csv")

# #get ibra info for bct points 
# test <- st_intersection(ibra, bct_sites)
# 
# bct_with_ibra <- test %>% 
#   rename(IBRA = CONT_NAME) %>% 
#   mutate(IBRA = case_when(
#     IBRA == "NSW SouthWestern Slopes" ~ "NSW South Western Slopes",
#     IBRA == "New England Tableland" ~ "New England Tablelands",
#     IBRA == "Murray-Darling Depression" ~ "Murray Darling Depression",
#     IBRA == "South Brigalow" ~ "Brigalow Belt South",
#     TRUE ~ IBRA  # Keep the original value if none of the above conditions match
#   )) %>%  
#   left_join(mon, by = "MP.ID") %>% 
# select(IBRA, veg_class, veg_form, tenure, MP.ID)

#try and just do formation first then try above if needed...

bct_with_form <- bct_sites %>% 
  left_join(mon, by = "MP.ID") %>% 
  select(MP.ID, bionet_censuskey, veg_form, viewfilter.y, tenure, geometry) 
#some name cleaning
fix_abreviations  <- read.csv("data/raw/bct_plot_metadata/formation_abbreviations.csv") %>% 
  left_join(bct_with_form, by = "veg_form") %>% 
  left_join(mon, by = "MP.ID") %>% 
  rename(site = "GlobalID") %>% 
  select(MP.ID, vegetation_formation) %>% 
  distinct()

#get bct fd
bct_fd <- read.csv("data/processed/BCT_fundiversity/fundiv_metrics_bct_mon_sites.csv") %>% 
  rename(GlobalID = "site") %>% 
  select(-c(X)) %>% 
  left_join(bct_sites, by = "GlobalID") %>% 
  left_join(fix_abreviations, by = "MP.ID") %>% 
  select(GlobalID:sp_richness, MP.ID, vegetation_formation, everything())
  
# write.csv(bct_fd, "data/processed/BCT_fundiversity/BCT_fundiv_w_form.csv", row.names = FALSE)

