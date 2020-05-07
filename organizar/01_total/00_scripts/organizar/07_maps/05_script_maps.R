### script maps - gymnophiona ###

# mauricio vancine
# 08-04-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(ggsn)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)
library(wesanderson)

# directory
path <- "/home/mauricio/Downloads/gymnophiona"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ <- readr::read_csv("00_occ_siphonops_clean.csv")
occ

# li
li <- sf::st_read("/media/mauricio/data1/006_orientacoes/lucas/03_data/var/00_limit/rnaturalearth_small_south_america.shp")
li

# sp
sp <- occ %>% 
  dplyr::select(species) %>% 
  dplyr::distinct() %>%
  dplyr::pull()
sp <- sp[c(1, 5)]
sp

# enms
enm.eta <- raster::stack(dir(pattern = "eta.tif$")) %>% 
  raster::brick()
enm.eta

enm.w14 <- raster::stack(dir(pattern = "w14.tif$")) %>% 
  raster::brick()
enm.w14

enm.w20 <- raster::stack(dir(pattern = "w20.tif$")) %>% 
  raster::brick()
enm.w20


# maps --------------------------------------------------------------------
for(i in sp){
  
  # information
  print(i)
  
  # occ
  occ.sp <- occ %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(longitude, latitude)
  
  # enm
  enm.eta.da <- enm.eta %>% 
    raster::subset(grep(i, names(enm.eta), value = TRUE)) %>% 
    raster::rasterToPoints() %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(lon = x, lat = y, enm = grep(i, names(enm.eta), value = TRUE))
  
  enm.w14.da <- enm.w14 %>% 
    raster::subset(grep(i, names(enm.w14), value = TRUE)) %>% 
    raster::rasterToPoints() %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(lon = x, lat = y, enm = grep(i, names(enm.w14), value = TRUE))
  
  enm.w20.da <- enm.w20 %>% 
    raster::subset(grep(i, names(enm.w20), value = TRUE)) %>% 
    raster::rasterToPoints() %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(lon = x, lat = y, enm = grep(i, names(enm.w20), value = TRUE))
  
  # map
  map.eta <- ggplot() + 
    geom_raster(data = enm.eta.da, aes(x = lon, y = lat, fill = enm)) +
    scale_fill_gradientn(colours = wes_palette("Zissou1", 10, type = "continuous")) +
    geom_sf(data = li, fill = NA, color = "black", alpha = .5) +
    geom_point(data = occ.sp, aes(x = longitude, y = latitude), size = 1.5, alpha = .4) +
    coord_sf() +
    theme_bw() +
    labs(x = "Longitude", y = "Latitude", fill = "Adequabilidade",
         title = paste0(stringr::str_to_title(i) %>% stringr::str_replace("_", " "), " - ETA")) +
    ggsn::north(data = li, symbol = 12, scale = .09) +
    ggsn::scalebar(data = li, dist = 500, dist_unit = "km", transform = TRUE, model = "WGS84",
                   height = .02, st.dist = .02, st.size = 3.5, border.size = .5) +
    theme(title = element_text(face = "italic", size = 15),
          axis.title = element_text(face = "plain", size = 10),
          legend.title = element_text(face = "bold", size = 10))
  
  map.w14 <- ggplot() + 
    geom_raster(data = enm.w14.da, aes(x = lon, y = lat, fill = enm)) +
    scale_fill_gradientn(colours = wes_palette("Zissou1", 10, type = "continuous")) +
    geom_sf(data = li, fill = NA, color = "black", alpha = .5) +
    geom_point(data = occ.sp, aes(x = longitude, y = latitude), size = 1.5, alpha = .4) +
    coord_sf() +
    theme_bw() +
    labs(x = "Longitude", y = "Latitude", fill = "Adequabilidade",
         title = paste0(stringr::str_to_title(i) %>% stringr::str_replace("_", " "), " - W14")) +
    ggsn::north(data = li, symbol = 12, scale = .09) +
    ggsn::scalebar(data = li, dist = 500, dist_unit = "km", transform = TRUE, model = "WGS84",
                   height = .02, st.dist = .02, st.size = 3.5, border.size = .5) +
    theme(title = element_text(face = "italic", size = 15),
          axis.title = element_text(face = "plain", size = 10),
          legend.title = element_text(face = "bold", size = 10))
  
  map.w20 <- ggplot() + 
    geom_raster(data = enm.w20.da, aes(x = lon, y = lat, fill = enm)) +
    scale_fill_gradientn(colours = wes_palette("Zissou1", 10, type = "continuous")) +
    geom_sf(data = li, fill = NA, color = "black", alpha = .5) +
    geom_point(data = occ.sp, aes(x = longitude, y = latitude), size = 1.5, alpha = .4) +
    coord_sf() +
    theme_bw() +
    labs(x = "Longitude", y = "Latitude", fill = "Adequabilidade",
         title = paste0(stringr::str_to_title(i) %>% stringr::str_replace("_", " "), " - W20")) +
    ggsn::north(data = li, symbol = 12, scale = .09) +
    ggsn::scalebar(data = li, dist = 500, dist_unit = "km", transform = TRUE, model = "WGS84",
                   height = .02, st.dist = .02, st.size = 3.5, border.size = .5) +
    theme(title = element_text(face = "italic", size = 15),
          axis.title = element_text(face = "plain", size = 10),
          legend.title = element_text(face = "bold", size = 10))
  
  
  # export
  ggsave(filename = paste0(i, "_eta.tiff"), plot = map.eta, wi = 20, he = 15, un = "cm", dpi = 300, comp = "lzw")
  ggsave(filename = paste0(i, "_w14.tiff"), plot = map.w14, wi = 20, he = 15, un = "cm", dpi = 300, comp = "lzw")
  ggsave(filename = paste0(i, "_w20.tiff"), plot = map.w20, wi = 20, he = 15, un = "cm", dpi = 300, comp = "lzw")
  
}


# end ---------------------------------------------------------------------