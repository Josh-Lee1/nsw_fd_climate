library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)
library(cmsafops)

#load data
##nsw mask
nsw<- read_sf("data/raw/nsw_state_polygon_shp_gda2020/NSW_STATE_POLYGON_shp_GDA2020.shp")

##use this function to read in monthly time series, and write into yearly
###only needs to be run once
 # yearsum(var = "pr-fl", infile = "data/raw/lina_climate/BC/pr-fl/pr-fl_narclimi_csiro-bom-access1-3_rcp45_r1i1p1_unsw-wrf360j_v1_mon.nc",
 #          outfile = "data/raw/lina_climate/time_test/annual_precip.nc")
# yearmean(var = "tas", infile = "data/raw/lina_climate/BC/tas/tas_narclimi_csiro-bom-access1-3_rcp45_r1i1p1_unsw-wrf360j_v1_mon.nc",
#          outfile = "data/raw/lina_climate/time_test/annual_temp.nc")

precip_45 <- nc_open("data/raw/lina_climate/time_test/annual_precip.nc")
tas_45 <- nc_open("data/raw/lina_climate/time_test/annual_temp.nc")

rain.array <- ncvar_get(precip_45, "pr-fl")
lon <- ncvar_get(precip_45, "lon")
lat <- ncvar_get(precip_45, "lat", verbose = F)
#dim(prec.array) shows us that we have the same lat and lon and times as defined above.
dim(rain.array) 
#what is the fill value
fillvalue <- ncatt_get(precip_45, "pr-fl", "_FillValue")
fillvalue
#make fill value NA
rain.array[rain.array == fillvalue$value] <- NA
#make a time slice to have a look at each year
prec.slice <- rain.array[, , 80]
r <- raster(t(prec.slice),
            xmn=min(lon),
            xmx=max(lon),
            ymn=min(lat),
            ymx=max(lat),
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
prec_45_2075 <- flip(r, direction='y')
plot(prec_45_2075)

writeRaster(temp_45_2075, filename = "data/processed/processed_climate_data/tas4.5_2075.tiff")
writeRaster(prec_45_2075, filename = "data/processed/processed_climate_data/prec4.5_2075.tiff")


# #to mask nsw
# # Filter out empty geometries
# nsw_sf <- nsw[!st_is_empty(nsw), ]
# # Convert the filtered sf object back to Spatial object
# nsw_shp <- as(nsw_sf, "Spatial")
# # Create a mask using the filtered shapefile
# masked_tas2075_raster <- mask(temp_45_2075, nsw_shp)
# plot(masked_tas2075_raster)

