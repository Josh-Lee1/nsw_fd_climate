library(tidyverse)
library(arrow)
library(raster)
library(sf)

# #read and filter ala data
# ala_all_bionet<- read_parquet("data/processed/obs_for_climate_calc_2023-08-24.parquet") %>% 
#   filter(basisOfRecord == "PRESERVED_SPECIMEN") %>% 
#   st_as_sf(coords = c("decimalLongitude", "decimalLatitude"))

#reading the csv with all ala not just the bionet sp
ala_all<- read.csv("data/processed/ala_all.csv") %>% 
  filter(basisOfRecord == "PRESERVED_SPECIMEN") %>% 
  filter(!is.na(decimalLongitude)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"))

# Function to extract data from a raster file for points
extract_raster_data <- function(raster_file, points) {
  extracted_data <- raster::extract(raster_file, points)
  return(extracted_data)
}

#read in historic climate data
raster_files <- list(
hist_tas <- raster("data/processed/processed_climate_data/MAT_current.tiff"),
hist_prec <- raster("data/processed/processed_climate_data/MAP_current.tiff")
)

# Initialize an empty data frame to store extracted data
extracted_data <- data.frame()

# Extract data from all raster files for the points using lapply
extracted_data_list <- lapply(raster_files, extract_raster_data, ala_all)

# Combine the extracted data with the original point data
points_with_raster_data <- cbind(ala_all, do.call(cbind, extracted_data_list)) %>% 
  dplyr::rename(tas_historical = X1,
                precip_historical = X2) %>% 
  filter(!is.na(precip_historical)) %>% 
  filter(!is.na(tas_historical))

climate_margins <- points_with_raster_data %>% 
  group_by(species) %>% 
  summarise(
    max_tas_historical = max(tas_historical, na.rm = TRUE),
    min_tas_historical = min(tas_historical, na.rm = TRUE),
    p98_tas_historical = quantile(tas_historical, probs = 0.98, na.rm = TRUE),
    p95_tas_historical = quantile(tas_historical, probs = 0.95, na.rm = TRUE),
    p90_tas_historical = quantile(tas_historical, probs = 0.90, na.rm = TRUE),
    p02_tas_historical = quantile(tas_historical, probs = 0.02, na.rm = TRUE),
    p05_tas_historical = quantile(tas_historical, probs = 0.05, na.rm = TRUE),
    p10_tas_historical = quantile(tas_historical, probs = 0.10, na.rm = TRUE),
    mean_tas_historical = mean(tas_historical, na.rm = TRUE),
    max_precip_historical = max(precip_historical, na.rm = TRUE),
    min_precip_historical = min(precip_historical, na.rm = TRUE),
    p98_precip_historical = quantile(precip_historical, probs = 0.98, na.rm = TRUE),
    p95_precip_historical = quantile(precip_historical, probs = 0.95, na.rm = TRUE),
    p90_precip_historical = quantile(precip_historical, probs = 0.90, na.rm = TRUE),
    p02_precip_historical = quantile(precip_historical, probs = 0.02, na.rm = TRUE),
    p05_precip_historical = quantile(precip_historical, probs = 0.05, na.rm = TRUE),
    p10_precip_historical = quantile(precip_historical, probs = 0.10, na.rm = TRUE),
    mean_precip_historical = mean(precip_historical, na.rm = TRUE)
  )

write_rds(climate_margins, "data/processed/climate_margins_raw_names.rds")

read_rds("data/processed/climate_margins_raw_names.rds")
