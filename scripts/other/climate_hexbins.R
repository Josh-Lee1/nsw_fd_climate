library(tidyverse)
library(hexbin)

# #get raster values across nsw
# nsw <- terra::vect("data/raw/nsw_state_polygon_shp_gda2020/NSW_STATE_POLYGON_shp_GDA2020.shp")
# #load climate rasters (MAT and MAP)
# vars <- c("_1.tif", "_12.tif")
# # Stacked raster of climate predictors, cropped to nsw
# stacked_rasters <- 
#   paste0("data/raw/bioclim/wc2.1_30s_bio", vars) %>%
#   terra::rast() %>%
#   terra::crop(plot_vec) 
# nsw_climate_data <- terra::extract(x= stacked_rasters,
#                                    y = nsw)
#already done:
nsw_climate_data <- read.csv("data/processed/all_nsw_climdata.csv") %>% 
  rename(Annual_temp = "wc2.1_30s_bio_1",
         Annual_precip = "wc2.1_30s_bio_12") %>% 
  select(-c(X))

#and plot info
plot_climate_data <- read.csv("data/processed/plots_with_climate.csv") %>% 
  select(-c(X)) %>% 
  distinct()

ggplot() +
  geom_hex(data = nsw_climate_data,
           mapping = aes(x = Annual_precip,
                         y = Annual_temp),
           bins = 30,
           fill = "grey",
           show.legend = NA) +
  geom_hex(data = plot_climate_data,
                     mapping = aes(x = Annual_precip,
                                   y = Annual_temp),
                     bins = 30,
                     show.legend = TRUE) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2250)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 22.5)) +
  ggtitle("Climate of Plots relative to NSW Climate") + 
  theme_bw() +
  theme(legend.position = "none")

ggplot() +
  geom_hex(data = plot_climate_data,
           mapping = aes(x = Annual_precip,
                         y = Annual_temp),
           bins = 30,
           show.legend = TRUE) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2250)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 22.5)) +
  ggtitle("Climate of Plots") + 
  theme_bw()
#I thinnk the legend is weird because the scale is including both dfs so the plot data that I'm interested
# in is shrinking in colour to the huge scale of the all_nsw df. 
