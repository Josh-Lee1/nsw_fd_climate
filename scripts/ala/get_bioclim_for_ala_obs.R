library(tidyverse)
library(raster)
library(arrow)
library(terra)
library(hexbin)
library(sf)

#get the climate data
# Stacked raster of climate predictors, cropped to Australia
vars <- c("_1.tif", "_12.tif")

stacked_rasters <- 
  paste0("data/raw/fine_res_climate/wc2.1_30s_bio", vars) %>%
  terra::rast()


#ala observations
ala_obs <- read_parquet("data/processed/obs_for_climate_calc_2023-08-24.parquet")

#crop to Aus mainland
aus_shape<- st_read("data/raw/AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp")
ala_sf <- st_as_sf(ala_obs, coords = c("decimalLongitude", "decimalLatitude"), crs = crs(aus_shape))

cropped_sf <- st_intersection(ala_sf, aus_shape)

#make a vector for extraction
obs_vector <- 
  terra::vect(
    ala_obs, 
    geom = c("decimalLongitude","decimalLatitude")
  )



#extract raster values for each plot
ala_clim <- 
  terra::extract(x = stacked_rasters,
                 y = obs_vector,
                 method = "simple",
                 bind = TRUE,
                 xy = FALSE,
                 cells = FALSE,
                 ID = FALSE) 


ala_with_clim <- ala_clim %>% 
  as.data.frame() %>% 
  dplyr::select(recordID,
         Annual_precip = "wc2.1_30s_bio_12",
         Annual_temp = "wc2.1_30s_bio_1") %>% 
  distinct() %>% 
  left_join(ala_obs)

#test look at climate of herbarium vs all
ala_reduced <- ala_with_clim %>% 
  filter(basisOfRecord == "PRESERVED_SPECIMEN")

ggplot() +
  geom_hex(data = ala_with_clim,
           mapping = aes(x = Annual_precip,
                         y = Annual_temp),
           bins = 30,
           fill = "gray", 
           show.legend = NA) +
  geom_hex(data = ala_reduced,
           mapping = aes(x = Annual_precip,
                         y = Annual_temp),
           bins = 30,
           show.legend = TRUE) +
  # scale_x_continuous(expand = c(0, 0), limits = c(0, 2750)) +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 27)) +
  ggtitle("Occurrence of all plant species relative to climate space") + 
  theme_bw() +
  theme(legend.position = "none")



