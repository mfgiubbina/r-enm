### script enm - dismo ###

## ensemble - frequency ##

# mauricio vancine
# 02-03-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(colorRamps)
library(raster)
library(tidyverse)

# import data -------------------------------------------------------------
# directory
setwd("/media/mauricio/data/gitlab/r-enm/data/enms")

# enms
enm <- raster::raster(dir(pattern = ".tif$")[1])
enm

# limit
li.br <- sf::read_sf("/media/mauricio/data/gitlab/r-enm/data/var/limit_brazil_gadm36_gcswgs84.shp")
li.br

# plot
ggplot() +
  geom_raster(data = raster::rasterToPoints(enm) %>% tibble::as_tibble(), 
              aes(x, y, fill = bioclim_haddadus_binotatus_01)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100), name = "Adequabilidade") +
  geom_sf(data = li.br, color = "black", fill = NA) +
  labs(x = "lon", y = "lat") + 
  theme_minimal()

# import evaluates
eva <- purrr::map_df(dir(pattern = ".csv$"), readr::read_csv)
eva

# frequency ensemble  -----------------------------------------------------
# ensemble raster
ens <- enm
ens[] <- 0
plot(ens)

# create directory
dir.create("00_ensemble_freq")
  
# ensemble
for(i in eva$species %>% unique){
  
  # information
  print(i)
  
  # select species
  eva.sp <- eva %>% dplyr::filter(species == i)
  
  for(j in seq(nrow(eva.sp))){
    
    # information
    eva.sp %>% dplyr::slice(j) %>% dplyr::select(file) %>% dplyr::pull() %>% print
    
    # import raster
    enm.j <- eva.sp %>% dplyr::slice(j) %>% dplyr::select(file) %>% dplyr::pull() %>% raster::raster()
    
    # select threshold
    thr.j <- eva.sp %>% dplyr::slice(j) %>% dplyr::select(threshold) %>% dplyr::pull()
    
    # cut
    enm.thr <- enm.j >= thr.j
    
    # frequency sum 
    ens <- sum(ens, enm.thr)
    
  }

  # ensemble frequency
  ens.f <- ens / nrow(eva.sp)
  ens.f  

  # directory
  setwd("00_ensemble_freq")
  
  # export
  raster::writeRaster(ens.f, paste0("ensemble_freq_", i, ".tif"), 
                      options = c("COMPRESS=DEFLATE"), overwrite = TRUE)
  
  # new ens
  ens[] <- 0
  
  # back directory
  setwd("..")
  
}

###----------------------------------------------------------------------------###

# directory
setwd("00_ensemble_freq")

# import
mo <- stack(dir(patt = ".tif$"))
mo

# map
ggplot() +
  geom_raster(data = raster::rasterToPoints(mo) %>% tibble::as_tibble(), 
              aes(x, y, fill = ensemble_freq_haddadus_binotatus)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like(100), name = "Adequabilidade") +
  geom_sf(data = li.br, color = "black", fill = NA) +
  theme_minimal() 

# map
ggplot() +
  geom_raster(data = raster::rasterToPoints(mo) %>% tibble::as_tibble(), 
              aes(x, y, fill = ensemble_freq_vitreorana_uranoscopa), alpha = .9) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100), name = "Adequabilidade") +
  geom_sf(data = li.br, color = "black", fill = NA) +
  theme_minimal() 

# end ---------------------------------------------------------------------
