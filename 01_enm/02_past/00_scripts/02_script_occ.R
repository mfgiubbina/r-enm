#' ---
#' title: occ - download and clean
#' author: mauricio vancine
#' date: 2019-01-15
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(CoordinateCleaner)
library(lubridate)
library(tidyverse)

# directory
path <- "/home/mude/data/gitlab/r-sdm/00_pragmatico/02_occ"
setwd(path)
dir()

# import data -------------------------------------------------------------
# species
sp <- "Haddadus binotatus"
sp
  
# bases for download
db <- c("gbif", "bison", "inat", "ebird", "ecoengine", "vertnet", "idigbio", "obis", "ala")
db

# download ----------------------------------------------------------------
# occ data
occ <- spocc::occ(query = sp, 
                  from = db, 
                  has_coords = TRUE, 
                  limit = 1e6)
occ

# get data ----------------------------------------------------------------
# data
occ_data <- spocc::occ2df(occ) %>% 
  dplyr::mutate(species = sp,
                longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = lubridate::year(date),
                base = prov) %>% 
  dplyr::select(species, name, longitude, latitude, year, base)
occ_data

readr::write_csv(occ_data, paste0("occ_spocc_raw_", sp, lubridate::today(), ".csv"))

# spatial filter ----------------------------------------------------------
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
            "equal", # equal coordinates
            "gbif", # radius around GBIF headquarters
            "institutions", # radius around biodiversity institutions
            "outliers", # records far away from all other records of this species
            "seas", # in the sea
            "urban", # within urban area
            "validity", # outside reference coordinate system
            "zeros" # plain zeros and lat = lon 
  )
)

# results
#' TRUE = clean coordinate entry 
#' FALSE = potentially problematic coordinate entries
flags_spatial %>% head
summary(flags_spatial)

# exclude records flagged by any test
occ_data_tax_date_spa <- occ_data_na %>% 
  dplyr::filter(flags_spatial$.summary == TRUE)
occ_data_tax_date_spa

# resume data
occ_data_na$species %>% table
occ_data_tax_date_spa$species %>% table

# map
ggplot() +
  geom_sf(data = li) +
  geom_sf(data = br, fill = NA) +
  geom_point(data = occ_data_na, aes(x = longitude, y = latitude)) +
  geom_point(data = occ_data_tax_date_spa, aes(x = longitude, y = latitude), col = "blue") +
  theme_bw() +
  theme(legend.position = "none")

# oppc --------------------------------------------------------------------
# import raster id
var_id <- raster::raster("/home/mude/data/gitlab/r-sdm/01_data/01_var/01_mask/01_present/wc14_5km_bio02.tif")
var_id

var_id[!is.na(var_id)] <- raster::cellFromXY(var_id, raster::rasterToPoints(var_id)[, 1:2])
plot(var_id)

# oppc
occ_data_tax_date_spa_oppc <- occ_data_tax_date_spa %>% 
  dplyr::mutate(oppc = raster::extract(var_id, dplyr::select(., longitude, latitude))) %>% 
  dplyr::distinct(species, oppc, .keep_all = TRUE) %>% 
  dplyr::filter(!is.na(oppc)) %>% 
  dplyr::add_count(species) %>% 
  dplyr::arrange(species)
occ_data_tax_date_spa_oppc

# verify
table(occ_data_tax_date_spa$species)
table(occ_data_tax_date_spa_oppc$species)

# map
ggplot() +
  geom_sf(data = br) +
  geom_sf(data = li, fill = NA) +
  geom_point(data = occ_data_tax_date_spa_oppc, aes(x = longitude, y = latitude, color = species)) +
  theme_bw()

# export ------------------------------------------------------------------
# export
readr::write_csv(occ_data_tax_date_spa_oppc, paste0("/home/mude/data/gitlab/r-sdm/01_data/00_occ/hb_clean.csv"))

# end ---------------------------------------------------------------------