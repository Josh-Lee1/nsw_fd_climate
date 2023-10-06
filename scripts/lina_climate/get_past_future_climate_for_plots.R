# Load required libraries
library(raster)
library(sf)
library(tidyverse)

# Function to extract data from a raster file for points
extract_raster_data <- function(raster_file, points) {
  extracted_data <- extract(raster_file, points)
  return(extracted_data)
}

# Load your raster files
raster_files <- list(
  hist_tas <- raster("data/processed/processed_climate_data/MAT_current.tiff"),
  hist_prec <- raster("data/processed/processed_climate_data/MAP_current.tiff"),
  future_tas <- raster("data/processed/processed_climate_data/tas4.5_2075.tiff"),
  future_prec <- raster("data/processed/processed_climate_data/prec4.5_2075.tiff")
)

# Load your set of points as a SpatialPointsDataFrame
bnfs_plots <- read.csv("data/processed/cleaned_benchmark_data.csv") %>% 
  dplyr::select(CensusKey,
                Longitude,
                Latitude) %>% 
  dplyr::distinct() %>% 
  st_as_sf(coords = c("Longitude", "Latitude"))

# Initialize an empty data frame to store extracted data
extracted_data <- data.frame()

# Extract data from all raster files for the points using lapply
extracted_data_list <- lapply(raster_files, extract_raster_data, bnfs_plots)


# Combine the extracted data with the original point data
points_with_raster_data <- cbind(bnfs_plots, do.call(cbind, extracted_data_list)) %>% 
  dplyr::rename(tas_historical = X1,
                precip_historical = X2,
                tas_2075_4.5 = X3,
                precip_2075_4.5 = X4) %>% 
  dplyr::mutate(tas_change = tas_2075_4.5 - tas_historical) %>% 
  dplyr::mutate(precip_change = precip_2075_4.5 - precip_historical) %>% 
  st_drop_geometry()
  
write.csv(points_with_raster_data, "data/processed/bnfs_plots_with_current_future_climate.csv", row.names = FALSE)

ggplot2::ggplot(points_with_raster_data, aes(tas_change)) +
  geom_histogram() +
  xlab("change in MAT") +
  theme_bw()
ggplot2::ggplot(points_with_raster_data, aes(precip_change)) +
  geom_histogram()+
  xlab("change in MAP") +
  theme_bw()
