#' ---
#' title: download and extract values from bioclim and chelsa
#' authors: mauricio vancine
#' date: 2020-05-14
#' ---

# packages
library(raster)
library(rnaturalearth)
library(rvest)
library(sf)
library(spocc)
library(tidyverse)
library(tmap)

# points ------------------------------------------------------------------
# download points
occ_guara <- spocc::occ(query = "Chrysocyon brachyurus", from = "gbif") %>% 
  spocc::occ2df() %>% 
  tidyr::drop_na(longitude, latitude)
occ_guara

# vector
occ_guara_vector <- occ_guara %>% 
  dplyr::mutate(lon = longitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_guara_vector

# brazilzao
br <- rnaturalearth::ne_countries(country = "Brazil")
br

# map
tm_shape(br) +
  tm_polygons() +
  tm_shape(occ_guara_vector) +
  tm_dots(size = .3, shape = 20, col = "gray30") +
  tm_graticules(lines = FALSE) +
  tm_compass() +
  tm_scale_bar()

# variables ---------------------------------------------------------------
# bioclim
# download
downloader::download(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_bio.zip",
                     destfile = "wc2.1_10m_bio.zip", mode = "wb")

# unzip
unzip(zipfile = "wc2.1_10m_bio.zip")

# import
var_bioclim <- dir(pattern = "wc") %>%
  stringr::str_subset(".tif") %>% 
  raster::stack()
var_bioclim

# rename
names(var_bioclim)
names(var_bioclim) <- paste0("wc2_", c("bio01", paste0("bio", 10:19), paste0("bio0", 2:9)))
names(var_bioclim)
var_bioclim

# crop
var_bioclim_br <- raster::crop(var_bioclim, br)
var_bioclim_br

# map
tm_shape(var_bioclim_br$wc2_bio01) +
  tm_raster() +
  tm_shape(br) +
  tm_borders() +
  tm_shape(occ_guara_vector) +
  tm_dots(size = .3, shape = 20, col = "gray30") +
  tm_graticules(lines = FALSE) +
  tm_compass() +
  tm_scale_bar() +
  tm_layout(legend.position = c("right", "top"))

# chelsa
# download
chelsa_links <- xml2::read_html("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/") %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset(".tif") %>% 
  paste0("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/", .)
chelsa_links

chelsa_files <- xml2::read_html("https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/integer/") %>% 
  rvest::html_nodes("a") %>% 
  rvest::html_attr("href") %>% 
  stringr::str_subset(".tif")
chelsa_files

purrr::map2(chelsa_links, chelsa_files, download.file, mode = "wb") # demora horrores

# import
var_chelsa <- dir(pattern = "CHELSA") %>%
  raster::stack()
var_chelsa

# rename
names(var_chelsa)
names(var_chelsa) <- paste0("ch_", c(paste0("bio0", 1:9), paste0("bio", 10:19)))
names(var_chelsa)
var_chelsa

# crop
var_chelsa_br <- raster::crop(var_chelsa, br) # demora horrores
var_chelsa_br

# resample to same resolution from wolrclim
var_chelsa_br <- raster::resample(var_chelsa_br, var_bioclim_br)
var_chelsa_br

# map
tm_shape(var_chelsa_br$ch_bio01) +
  tm_raster() +
  tm_shape(br) +
  tm_borders() +
  tm_shape(occ_guara_vector) +
  tm_dots(size = .3, shape = 20, col = "gray30") +
  tm_graticules(lines = FALSE) +
  tm_compass() +
  tm_scale_bar() +
  tm_layout(legend.position = c("right", "top"))

# extract values ----------------------------------------------------------
# extract values
occ_guara_var <- raster::extract(x = raster::stack(var_bioclim_br, var_chelsa_br), 
                                 y = occ_guara_vector) %>% 
  tibble::as_tibble()
occ_guara_var

# combine values
occ_guara_var_values <- dplyr::bind_cols(occ_guara, occ_guara_var)
occ_guara_var_values

# export
readr::write_csv(occ_guara_var_values, "occ_guara_var_values.csv")

# map
occ_guara_vector_var_values <- dplyr::left_join(occ_guara_vector, occ_guara_var_values, by = "key")
occ_guara_vector_var_values

# map
tm_shape(br) +
  tm_polygons() +
  tm_shape(occ_guara_vector_var_values) +
  tm_dots(size = .3, shape = 21, col = "wc2_bio01", palette = "-Spectral") +
  tm_graticules(lines = FALSE) +
  tm_compass() +
  tm_scale_bar() +
  tm_layout(legend.position = c("left", "bottom"))

# end ---------------------------------------------------------------------