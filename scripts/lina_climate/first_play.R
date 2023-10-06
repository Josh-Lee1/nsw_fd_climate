library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(sf)
library(tidyverse)
library(terra)

#read file
nc_data <- nc_open("data/raw/lina_climate/pr-fl_narclimi_cccma-canesm2_historical_r1i1p1_unsw-wrf360j_v1_mon_unit_JL.nc")

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

prec.array <- ncvar_get(nc_data, "pr-fl")
#dim(prec.array) shows us that we have the same lat and lon and times as defined above.
dim(prec.array) 

#what is the fill value
fillvalue <- ncatt_get(nc_data, "pr-fl", "_FillValue")
fillvalue

#make fill value NA
prec.array[prec.array == fillvalue$value] <- NA

prec.slice <- prec.array[, , 1]

r <- raster(t(prec.slice),
            xmn=min(lon),
            xmx=max(lon),
            ymn=min(lat),
            ymx=max(lat),
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r <- flip(r, direction='y')

plot(r)

aus <-  read_sf("data/raw/AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp")
aus <- aus["AUS_NAME21"]


