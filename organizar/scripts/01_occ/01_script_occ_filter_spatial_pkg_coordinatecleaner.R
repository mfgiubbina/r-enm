# script description #
# script: occurences - spatial filter
# package: CoordinateCleaner
# author:   mauricio vancine
# date:     05-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(CoordinateCleaner)
library(lubridate)
library(rnaturalearth)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ/filtered"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ_data <- readr::read_csv("occ_terretrial_animal_integrated_filter_gnr_dist.csv")
occ_data

# spatial data
br <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>% 
  filter(name == "Brazil")
br

# map
ggplot() +
  geom_sf(data = br) +
  geom_point(data = occ, aes(x = longitude, y = latitude, color = species)) +
  scale_color_viridis_d() +
  theme_bw()

# spatial filter occ ------------------------------------------------------
# remove na
occ_data_na <- occ_data %>% 
  tidyr::drop_na(longitude, latitude)
occ_data_na

# flag data
flags_spatial <- CoordinateCleaner::clean_coordinates(
  x = occ_data_na, 
  species = "species",
  lon = "longitude", 
  lat = "latitude",
  tests = c("capitals", # radius around capitals
            "centroids", # radius around country and province centroids
            "duplicates", # records from one species with identical coordinates
            "gbif", # radius around GBIF headquarters
            "institutions", # radius around biodiversity institutions
            # "iucn", # records outside the natural range, or any custom polygon
            # "outliers", # records far away from all other records of this species 
            "seas", # in the sea
            "urban", # within urban area
            "validity", # outside reference coordinate system
            "zeros", # plain zeros and lat = lon
            "equal", # equal coordinates
            "ranges"
  )
)

# results
summary(flags_spatial)

# plot
ggplot() +
  geom_sf(data = br) +
  geom_point(data = flags_spatial, aes(x = longitude, y = latitude, color = .summary), alpha = .7) +
  scale_color_viridis_d(name = "Flagged outlier") +
  theme_bw()

# exclude records flagged by any test
occ_data_filter_spatial <- occ_data_na[flags_spatial$.summary, ]
occ_data_filter_spatial

# resume data
occ_filter_spatial$species %>% table

# map
ggplot() +
  geom_sf(data = br) +
  geom_point(data = occ_filter_spatial, aes(x = longitude, y = latitude, color = species)) +
  scale_color_viridis_d() +
  theme_bw()

# date filter occ ---------------------------------------------------------
# verify
occ_data_filter_spatial$year %>% table

# year > 1960
occ_data_filter_spatial_date <- occ_data_filter_spatial %>% 
  dplyr::filter(year > 1960 | is.na(year) == TRUE)
occ_data_filter_spatial_date

# verify
occ_data_filter_spatial_date$year %>% table

# verify filters
occ_data$species %>% table
occ_data_filter_spatial$species %>% table
occ_data_filter_spatial_date$species %>% table

# export ------------------------------------------------------------------
readr::write_csv(occ_data_filter_spatial_date, 
                 paste0("occ_terretrial_animal_integrated_filter_gnr_dist_spatial_date.csv"))

# end ---------------------------------------------------------------------