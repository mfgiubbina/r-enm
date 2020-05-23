### download bio-oracle ##

# mauricio vancine
# 07-07-2018

# memory
rm(list = ls())

# packages
if(!require(install.load)) install.packages("install.load", dep = TRUE)
install.load::install_load("sdmpredictors", "leaflet")

###----------------------------------------------------------------### 

# Explore datasets in the package 
list_datasets() 

# Download specific layers to the current directory 
bathy <- load_layers(c("BO_bathymin", "BO_bathymean", "BO_bathymax")) 
bathy

# Check layer statistics 
layer_stats() 

# Check Pearson correlation coefficient between layers 
layers_correlation() 

###----------------------------------------------------------------### 

# Easy download of raster file (Maximum Temperature at the sea bottom) 
temp.max.bottom <- load_layers("BO2_tempmax_bdmax") 
temp.max.bottom

# Crop raster to fit the North Atlantic 
ne.atlantic.ext <- extent(-100, 45, 30.75, 72.5) 
temp.max.bottom.crop <- crop(temp.max.bottom, ne.atlantic.ext) 

# Generate a nice color ramp and plot the map 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(temp.max.bottom.crop,col=my.colors(1000),axes=FALSE, box=FALSE) 
title(cex.sub = 1.25, sub = "Maximum temperature at the sea bottom (ºC)") 

###----------------------------------------------------------------### 

# List layers avaialble in Bio-ORACLE v2 
layers.bio2 <- list_layers( datasets="Bio-ORACLE" ) 
layers.bio2 

# Download environmental data layers (Max. Temperature, Min. Salinity and Min. Nitrates at the sea bottom) 
environment.bottom <- load_layers( layercodes = c("BO2_tempmax_bdmean" , "BO2_salinitymin_bdmean", "BO2_nitratemin_bdmean") , equalarea=FALSE, rasterstack=TRUE) 

# Download bathymetry 
bathymetry <- load_layers("BO_bathymean") 

# Generate a data.frame with the sites of interest 
my.sites <- data.frame(Name=c("Faro, Portugal, NE Atlantic" , "Maspalomas, Spain, NE Atlantic" , "Guadeloupe, France, Caribbean Sea" , "Havana, Cuba, Caribbean Sea") , Lon=c(-7.873,-15.539,-61.208,-82.537) , Lat=c(37.047, 27.794,15.957,23.040 ) ) 
my.sites 

# Visualise sites of interest in google maps 
m <- leaflet() 
m <- addTiles(m) 
m <- addMarkers(m, lng=my.sites$Lon, lat=my.sites$Lat, popup=my.sites$Name) 
m 

# Extract environmental values from layers 
my.sites.environment <- data.frame(Name=my.sites$Name , depth=extract(bathymetry,my.sites[,2:3]) , extract(environment.bottom,my.sites[,2:3]) ) 
my.sites.environment 
