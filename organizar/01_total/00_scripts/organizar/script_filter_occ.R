#' ---
#' title: filter occurrences
#' authors: mauricio vancine
#' date: 2020-04-29
#' ---

# packages
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
occ_guara_vector <- sf::st_as_sf(occ_guara, 
                                 coords = c("longitude", "latitude"), crs = 4326)
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

# spatial filter ----------------------------------------------------------
# remove repeated coordinates
occ_guara_rep <- occ_guara %>% 
  tidyr::drop_na(longitude, latitude) %>% 
  dplyr::distinct(longitude, latitude, .keep_all = TRUE)
occ_guara_rep  

# desaggregation == rarefaction
occ_guara_rep_bias_spat <- occ_guara_rep %>% 
  dplyr::mutate(x = longitude, y = latitude) %>% 
  as.data.frame() %>% 
  ecospat::ecospat.occ.desaggregation(xy = ., min.dist = 0.0083, by = "name") %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-x, -y)
occ_guara_rep_bias_spat

# vector
occ_guara_rep_bias_spat_vector <- sf::st_as_sf(occ_guara_rep_bias_spat, 
                                               coords = c("longitude", "latitude"), crs = 4326)
occ_guara_rep_bias_spat_vector

# map
tm_shape(br) +
  tm_polygons() +
  tm_shape(occ_guara_vector) +
  tm_dots(size = .3, shape = 20, col = "gray30") +
  tm_shape(occ_guara_rep_bias_spat_vector) +
  tm_dots(size = .3, shape = 20, col = "red") +
  tm_graticules(lines = FALSE) +
  tm_compass() +
  tm_scale_bar()

# export
readr::write_csv(occ_guara_rep_vector_bias_spat, "occ_guara_rep_vector_bias_spat.csv")

# end ---------------------------------------------------------------------