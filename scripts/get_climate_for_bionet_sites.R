#packages
library(tidyverse)
library(terra)
library(sf)
library(spatstat)


# library(raster)
# library(tidyterra)

#load plot data
#bionet <- read.csv("data/raw/bionet_flora_survey/CensusList.csv")
benchmark_subset <- read.csv("data/raw/bionet_flora_survey/vegetation-condition-benchmarks-cover-and-richness-raw-data-v1-2.csv") %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

plot_vec <- 
  terra::vect(
    benchmark_subset, 
    geom = c("Longitude","Latitude")
  )

#make a map of plots in NSW
nsw <-  read_sf("data/raw/nsw_state_polygon_shp_gda2020/nsw_state_polygon_shp_gda2020.shp")
bench_sites <- benchmark_subset %>% 
  dplyr::filter(!is.na(Latitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))
benchmap <- ggplot() +
  geom_sf(data = nsw, fill = "white") +
  geom_sf(data = bench_sites, size = 1) + 
  theme_bw() +
  xlim(141, 154)
ggsave(benchmap, file = "figs/benchmark_locations.png")


#load climate rasters (MAT and MAP)
vars <- c("_1.tif", "_12.tif")

# Stacked raster of climate predictors, cropped to nsw
stacked_rasters <- 
  paste0("data/raw/bioclim/wc2.1_30s_bio", vars) %>%
  terra::rast() %>%
  terra::crop(plot_vec) 

#extract raster values for each plot
plots_clim <- 
  terra::extract(x = stacked_rasters,
                 y = plot_vec,
                 method = "simple",
                 bind = TRUE,
                 xy = FALSE,
                 cells = FALSE,
                 ID = FALSE) 
plots_with_clim <- plots_clim %>% 
  as.data.frame() %>% 
  select(CensusID,
         Annual_precip = "wc2.1_30s_bio_12",
         Annual_temp = "wc2.1_30s_bio_1") %>% 
  distinct() %>% 
  left_join(benchmark_subset)
write.csv(plots_with_clim, "data/processed/plots_with_climate.csv")


