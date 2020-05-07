# script description #
# script: occurences - convert utm to gcs
# author:   mauricio vancine
# date:     05-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(sf)
library(readxl)
library(tidyverse)

# raster options
raster::rasterOptions(maxmemory = 1e+10)
raster::rasterOptions(chunksize = 1e+10)

# directory
path <- "/media/mude/data/00_trabalho/00_empresas/aquaflora/05_distribuicao_especies/03_dados/00_ocorrencias/02_aquatico/00_continente/00_raw"
setwd(path)
dir()

# occ ---------------------------------------------------------------------
# import
occ <- readxl::read_xlsx("raw_aquatico_total_peixes.xlsx")
occ

# convert -----------------------------------------------------------------
# 24 k
occ_24k_gcs <- occ %>% 
  dplyr::filter(zona == "24K") %>% 
  dplyr::select(x, y) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 32724) %>% 
  sf::st_transform(crs = 4326) %>% 
  sf::st_coordinates() %>% 
  tibble::as_tibble() %>% 
  dplyr::rename(longitude = X, latitude = Y) %>% 
  dplyr::bind_cols(occ[occ$zona == "24K", 1])
occ_24k_gcs

# 23 k
occ_23k_gcs <- occ %>% 
  dplyr::filter(zona == "23K") %>% 
  dplyr::select(x, y) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 32723) %>% 
  sf::st_transform(crs = 4326) %>% 
  sf::st_coordinates() %>%
  tibble::as_tibble() %>% 
  dplyr::rename(longitude = X, latitude = Y) %>% 
  dplyr::bind_cols(occ[occ$zona == "23K", 1])
occ_23k_gcs

# bind
occ_gcs <- dplyr::bind_rows(occ_23k_gcs, occ_24k_gcs) %>% 
  dplyr::mutate(datum = "wgs84")
occ_gcs

# join data ---------------------------------------------------------------
occ <- dplyr::left_join(occ, occ_gcs, by = "Numero") %>% 
  dplyr::select(Numero:zona, longitude:datum, ÁREA:PG)
occ

# export ------------------------------------------------------------------
readr::write_csv(occ, "raw_aquatico_total_peixes_gcs.csv")

# end ---------------------------------------------------------------------