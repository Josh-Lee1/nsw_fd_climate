library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(sf)
library(sp)
library(tidyverse)
library(terra)

#read historic temp files 
agcd_max <- nc_open("data/raw/lina_climate/AGCD/Oz/agcd_v1_tmax_mean_monthly_1971-2000_Oz.nc")
agcd_min <- nc_open("data/raw/lina_climate/AGCD/Oz/agcd_v1_tmin_mean_monthly_1971-2000_Oz.nc")

#this data is max and min monthly temp 1971-2000, to get historic MAT I need to get average temp for each year (add 12 months and /12)
#do this for max and min, extract values for both then average these. Precip is easier. just one file but will have to do MAP calc too

#organise tmax
lon <- ncvar_get(agcd_max, "lon")
lat <- ncvar_get(agcd_max, "lat", verbose = F)
t <- ncvar_get(agcd_max, "time")
val <- ncvar_get(agcd_max, "tmax")
tmax.array <- ncvar_get(agcd_max, "tmax")
#dim(prec.array) shows us that we have the same lat and lon and times as defined above.
dim(tmax.array) 
#what is the fill value
fillvalue <- ncatt_get(agcd_max, "tmax", "_FillValue")
fillvalue
#make fill value NA
tmax.array[tmax.array == fillvalue$value] <- NA

# #make a slice to have a look, use first time slice 
##this all looks fine so commented out
# prec.slice <- prec.array[, , 1]
# r <- raster(t(prec.slice),
#             xmn=min(lon),
#             xmx=max(lon),
#             ymn=min(lat),
#             ymx=max(lat),
#             crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# r <- flip(r, direction='y')
# plot(r)
aus <-  read_sf("data/raw/AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp")
aus <- aus["AUS_NAME21"]
# 
# slice_df <- rasterToPoints(r) %>% 
#   as.data.frame()
# ggplot() +
#   geom_raster(data = slice_df, aes(x = x, y = y, fill = layer)) +
#   geom_sf(data = aus, fill = NA)

#create raster based on means for all time slices

# Initialize an empty raster stack
raster_stack <- stack()
# Loop through each time step
for (i in 1:length(t)) {
  # Read the data for a specific time step (adjust variable names accordingly)
  data <- ncvar_get(agcd_max, "tmax", start = c(1, 1, i), count = c(-1, -1, 1))
  # Create a raster layer from the data
  raster_layer <- raster(data)
  # Extract latitude and longitude information from the NetCDF file
  lat_data <- ncvar_get(agcd_max, "lon")
  lon_data <- ncvar_get(agcd_max, "lat")
  # Create a raster layer from the data and set the latitude and longitude attributes
  min_raster_layer <- raster(min_data)
  extent(raster_layer) <- extent(lon_data[1], lon_data[length(lon_data)], lat_data[1], lat_data[length(lat_data)])
  # Append the raster layer to the stack
  raster_stack <- stack(raster_stack, raster_layer)
}

# Calculate the mean of the raster stack along the time dimension
tmax_mean_raster <- mean(raster_stack, na.rm = TRUE)
plot(tmax_mean_raster)

#same for min
min_raster_stack <- stack()
# Loop through each time step
for (i in 1:length(t)) {
  # Read the data for a specific time step (adjust variable names accordingly)
  min_data <- ncvar_get(agcd_min, "tmin", start = c(1, 1, i), count = c(-1, -1, 1))
  # Create a raster layer from the data
  min_raster_layer <- raster(min_data)
  # Extract latitude and longitude information from the NetCDF file***this step is backwards because the nc file is reading lat for x and lon for y* there might be a better fix than this
  lat_data <- ncvar_get(agcd_min, "lon")
  lon_data <- ncvar_get(agcd_min, "lat")
  # Create a raster layer from the data and set the latitude and longitude attributes
  min_raster_layer <- raster(min_data)
  extent(min_raster_layer) <- extent(lon_data[1], lon_data[length(lon_data)], lat_data[1], lat_data[length(lat_data)])
  # Append the raster layer to the stack
  min_raster_stack <- stack(min_raster_stack, min_raster_layer)
}

# Calculate the mean of the raster stack along the time dimension
tmin_mean_raster <- mean(min_raster_stack, na.rm = TRUE)
plot(tmin_mean_raster)

#stack tmax and tmin, and then get MAT
MAT_stack <- stack(min_raster_stack, raster_stack)
MAT_raster <- mean(MAT_stack, na.rm = TRUE)
MAT_raster <- t(flip(MAT_raster, direction='x'))
plot(MAT_raster)

#Here I will mask just terrestrial
# Filter out empty geometries
aus_sf <- aus[!st_is_empty(aus), ]
# Convert the filtered sf object back to Spatial object
aus_shp <- as(aus_sf, "Spatial")
# Create a mask using the filtered shapefile
masked_MAT_raster <- mask(MAT_raster, aus_shp)
plot(masked_MAT_raster)
# Write the raster to a file
writeRaster(masked_MAT_raster, filename = "data/processed/processed_climate_data/MAT_current.tiff")

###This all again but for MAP #########################
agcd_prec <- nc_open("data/raw/lina_climate/AGCD/Oz/agcd_v1_precip_total_monthly_1971-2000_Oz.nc")
t <- ncvar_get(agcd_prec, "time")
prec <- ncvar_get(agcd_prec, "precip")

# Initialize an empty raster stack
raster_stack <- stack()
# Loop through each time step
for (i in 1:length(t)) {
  # Read the data for a specific time step (adjust variable names accordingly)
  rain_data <- ncvar_get(agcd_prec, "precip", start = c(1, 1, i), count = c(-1, -1, 1))
  # Create a raster layer from the data
  rain_raster_layer <- raster(rain_data)
  # Extract latitude and longitude information from the NetCDF file
  lat_data <- ncvar_get(agcd_prec, "lon")
  lon_data <- ncvar_get(agcd_prec, "lat")
  # Create a raster layer from the data and set the latitude and longitude attributes
  rain_raster_layer <- raster(rain_data)
  extent(rain_raster_layer) <- extent(lon_data[1], lon_data[length(lon_data)], lat_data[1], lat_data[length(lat_data)])
  # Append the raster layer to the stack
  raster_stack <- stack(raster_stack, rain_raster_layer)
}


# Calculate the mean of the raster stack along the time dimension
MAP_raster <- (sum(raster_stack, na.rm = TRUE)/30)
plot(MAP_raster)
MAP_raster <- t(flip(MAP_raster, direction='x'))
plot(MAP_raster)

# Filter out empty geometries
aus_sf <- aus[!st_is_empty(aus), ]

# Convert the filtered sf object back to Spatial object
aus_shp <- as(aus_sf, "Spatial")

# Create a mask using the filtered shapefile
masked_MAP_raster <- mask(MAP_raster, aus_shp)

# Write the raster to a file
writeRaster(masked_MAP_raster, filename = "data/processed/processed_climate_data/MAP_current.tiff")



