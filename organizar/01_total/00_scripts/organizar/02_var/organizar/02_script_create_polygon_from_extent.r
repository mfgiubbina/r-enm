### create polygon from extent ###

# mauricio vancine
# 2018-12-04

# memory
rm(list = ls())

# packages
library(raster)
library(rgdal)

# directory
setwd("")

# vector
ve <- rgdal::readOGR(dsn = "")
ve
plot(ve)

br <- raster::getData("")

# extention
ex <- raster::extent(x = li)
ex
plot(ex, add = TRUE)

# create vector from extent
ex.ve <- as(ex, "SpatialPolygons")
ex.ve
lines(ex.ve, col = "red")

# create vector with data
ex.ve <- sp::SpatialPolygonsDataFrame(ex.ve, data = data.frame(1))
ex.ve

# define projection
raster::crs(ex.ve) <- sp::CRS("+init=epsg:4326")
ex.ve

# export
rgdal::writeOGR(obj = ex.ve, 
                dsn = "bioma_am_ce_ca_norte_extent_gcs_wgs84.shp",
                layer = "bioma_am_ce_ca_norte_extent_gcs_wgs84.shp", 
                driver = "ESRI Shapefile")
                
###---------------------------------------------------------------------------###
