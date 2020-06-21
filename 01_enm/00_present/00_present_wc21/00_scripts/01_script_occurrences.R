#' ---
#' title: occ - download and clean
#' author: mauricio vancine
#' date: 2019-06-19
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(CoordinateCleaner)
library(ecospat)
library(lubridate)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(tmap)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present/00_present_wc21"
setwd(path)
dir.create("01_occurrences"); setwd("01_occurrences")
path <- getwd()
path

# import data -------------------------------------------------------------
# data
li <- rnaturalearth::countries110 %>% 
  sf::st_as_sf()
li

# species list
sp_list <- readr::read_csv("00_species_list/00_species_list.csv") %>% 
  dplyr::arrange(species) %>% 
  dplyr::pull()
sp_list

# bases for download
db <- c("gbif", "bison", "inat", "ebird", "ecoengine", "vertnet", "idigbio", "obis", "ala")
db

# download ----------------------------------------------------------------
# directory
setwd(path); dir.create("01_raw"); setwd("01_raw")

# occ
for(i in sp_list){
  
  # information
  print(i)
  
  # download spocc ----
  # download
  occ_spocc <- spocc::occ(query = i, 
                          from = db,
                          has_coords = TRUE, 
                          limit = 1e6)
  
  # conditional without data
  if(occ_spocc %>% spocc::occ2df() %>% nrow == 0){
    
    # conditional without year  
  } else if(!"date" %in% c(occ_spocc %>% spocc::occ2df() %>% colnames)){
    
    # get data
    occ_data_spocc <- spocc::occ2df(occ_spocc) %>% 
      dplyr::mutate(species_search = i,
                    longitude = as.numeric(longitude),
                    latitude = as.numeric(latitude),
                    year = NA,
                    base = prov %>% stringr::str_to_lower(),
                    r_package = "spocc") %>% 
      dplyr::select(name, species_search, longitude, latitude, year, base, r_package)
    
    # conditional with data and year
  } else{
    
    # get data
    occ_data_spocc <- spocc::occ2df(occ_spocc) %>% 
      dplyr::mutate(species_search = i,
                    longitude = as.numeric(longitude),
                    latitude = as.numeric(latitude),
                    year = lubridate::year(date),
                    base = prov %>% stringr::str_to_lower(),
                    r_package = "spocc") %>% 
      dplyr::select(name, species_search, longitude, latitude, year, base, r_package)
    
  }
  
  # download bien ----
  occ_bien <- BIEN::BIEN_occurrence_species(species = i) %>% 
    tibble::as_tibble()
  
  # conditional without data
  if(nrow(occ_bien) == 0){
    
    # get data
    occ_data_bien <- tibble::tibble(name = i,
                                    species_search = i,
                                    longitude = NA,
                                    latitude = NA,
                                    year = NA,
                                    base = NA,
                                    r_package = "bien")
    
  } else{
    
    # get data
    occ_data_bien <- occ_bien %>% 
      dplyr::mutate(name = scrubbed_species_binomial,
                    species_search = i,
                    year = lubridate::year(occ_bien$date_collected),
                    base = datasource %>% stringr::str_to_lower(),
                    r_package = "bien") %>% 
      dplyr::select(name, species_search, longitude, latitude, year, base, r_package)
  }
  
  # combine data ----
  occ_data <- dplyr::bind_rows(occ_data_spocc, occ_data_bien)
  
  # export
  readr::write_csv(occ_data, 
                   paste0("occ_spocc_bien_", i %>% 
                            stringr::str_to_lower() %>% 
                            stringr::str_replace(" ", "_"), "_", 
                          lubridate::today(), ".csv"))
  
}

# specieslink -------------------------------------------------------------
# directory
setwd(path); setwd("01_raw")

# unzip
purrr::map(dir(patt = ".zip"), unzip)

# impor data
for(i in dir(patt = ".xlsx$")){
  
  # information
  print(i)
  
  # import data
  occ_specieslink <- readxl::read_xlsx(i)
  
  # adjust data
  occ_specieslink_data <- occ_specieslink %>%
    dplyr::mutate(name = scientificname,
                  species_search = paste(stringr::str_split(string = scientificname, pattern = " ", simplify = TRUE)[, 1],
                                         stringr::str_split(string = scientificname, pattern = " ", simplify = TRUE)[, 2]),
                  longitude = longitude %>% as.numeric, 
                  latitude = latitude %>% as.numeric, 
                  year = yearcollected %>% as.numeric, 
                  base = "specieslink") %>%
    dplyr::select(name, species_search, longitude, latitude, year, base)
  
  # export
  readr::write_csv(occ_specieslink_data, 
                   paste0("occ_specieslink_", 
                          occ_specieslink_data$species_search[1] %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                          "_", lubridate::today(), ".csv"))
  
}

# integrated --------------------------------------------------------------
# directory
setwd(path); setwd("01_raw")

# import
occ_data <- purrr::map_dfr(dir(pattern = ".csv"), readr::read_csv)
occ_data

# directory
setwd(path); dir.create("02_integrated"); setwd("02_integrated")

# export
readr::write_csv(occ_data, paste0("occ_integrated_", lubridate::today(), ".csv"))

# map
occ_data_vector <- occ_data %>% 
  tidyr::drop_na(longitude, latitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

tm_shape(li, bbox = occ_data_vector) +
  tm_polygons() +
  tm_shape(occ_data_vector) +
  tm_dots(size = .2, shape = 21, col = "species_search",  
          palette = "Set1", title = "Species") +
  tm_graticules(lines = FALSE) +
  tm_layout(legend.text.fontface = "italic")

# filter ------------------------------------------------------------------
# directory
setwd(path); dir.create("03_clean"); setwd("03_clean")

# taxa filter
# adjust names
gnr_taxa <- NULL
gnr_total <- NULL

for(i in sp_list){
  
  # info
  print(i)
  
  # gnr names
  gnr <- taxize::gnr_resolve(i)
  
  # combine
  gnr_total <- rbind(gnr_total, gnr)
  
  # adjust names
  gnr_taxa_i <- gnr %>% 
    dplyr::mutate(species = i %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_")) %>% 
    dplyr::select(species, matched_name) %>%
    dplyr::bind_rows(tibble::tibble(species = i %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"),
                                    matched_name = c(i, 
                                                     i %>% stringr::str_to_title(),
                                                     i %>% stringr::str_to_lower(),
                                                     i %>% stringr::str_to_upper()))) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(matched_name)
  
  # combine
  gnr_taxa <- rbind(gnr_taxa, gnr_taxa_i)
  
}

# confer
gnr_taxa
gnr_total

# export gnr total
readr::write_csv(gnr_total, paste0("table_gnr_total_", lubridate::today(), ".csv"))

# confer data
occ_data %>%
  dplyr::count(name)

# taxa filter
occ_data_taxa <- dplyr::inner_join(occ_data, gnr_taxa, c(name = "matched_name")) %>% 
  dplyr::arrange(name) %>% 
  dplyr::select(name, species, everything())
occ_data_taxa

# confer
occ_data %>% dplyr::count(name)
occ_data_taxa %>% dplyr::count(name)

# map
occ_data_taxa_vector <- occ_data_taxa %>% 
  tidyr::drop_na(longitude, latitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

tm_shape(li, bbox = occ_data_taxa_vector) +
  tm_polygons() +
  tm_shape(occ_data_taxa_vector) +
  tm_dots(size = .2, shape = 21, col = "species_search",  
          palette = "Set1", title = "Species") +
  tm_graticules(lines = FALSE) +
  tm_layout(legend.text.fontface = "italic")

# date filter -------------------------------------------------------------
# verify
occ_data_taxa$year %>% table(useNA = "always")

# year > 1970 and < 2020
occ_data_taxa_date <- occ_data_taxa %>% 
  dplyr::filter(is.na(year) == FALSE,
                (year > 1970 & year <= 2020)) %>% 
  dplyr::arrange(year)
occ_data_taxa_date

# verify
occ_data_taxa$year %>% table(useNA = "always")
occ_data_taxa_date$year %>% table(useNA = "always")

# histogram
occ_data_taxa_date %>% 
  ggplot() + 
  aes(x = year, fill = species_search) +
  geom_histogram(color = "black") +
  scale_y_continuous(trans = "log10") +
  scale_fill_brewer(palette = "Set1") +
  facet_grid("species_search") +
  labs(x = "Year", y = "Frequency (log10)", fill = "Species") +
  theme_bw() +
  theme(legend.text = element_text(face = "italic"),
        legend.position = "none",
        strip.text = element_text(size = 10, face = "italic"))
ggsave(filename = "occ_plot_date.png", wi = 20, he = 15, un = "cm", dpi = 300)

# map
occ_data_taxa_date_vector <- occ_data_taxa_date %>% 
  tidyr::drop_na(longitude, latitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

tm_shape(li, bbox = occ_data_taxa_date_vector) +
  tm_polygons() +
  tm_shape(occ_data_taxa_date_vector) +
  tm_dots(size = .2, shape = 21, col = "species_search",  
          palette = "Set1", title = "Species") +
  tm_graticules(lines = FALSE) +
  tm_layout(legend.text.fontface = "italic")

# bias filter -------------------------------------------------------------
# remove na
occ_data_na <- occ_data_taxa_date %>% 
  tidyr::drop_na(longitude, latitude)
occ_data_na

# flag data
flags_bias <- CoordinateCleaner::clean_coordinates(
  x = occ_data_na, 
  species = "species",
  lon = "longitude", 
  lat = "latitude",
  outliers_mtp = 2,
  tests = c("capitals", # radius around capitals
            "centroids", # radius around country and province centroids
            "duplicates", # records from one species with identical coordinates
            "equal", # equal coordinates
            "gbif", # radius around GBIF headquarters
            "institutions", # radius around biodiversity institutions
            "seas", # in the sea
            "urban", # within urban area
            "validity", # outside reference coordinate system
            "zeros" # plain zeros and lat = lon 
  )
)

# results
#' TRUE = clean coordinate entry 
#' FALSE = potentially problematic coordinate entries
flags_bias %>% head
flags_bias %>% summary

# exclude records flagged by any test
occ_data_taxa_date_bias <- occ_data_na %>% 
  dplyr::filter(flags_bias$.summary == TRUE)
occ_data_taxa_date_bias

# resume data
occ_data_na %>% dplyr::count(species)
occ_data_taxa_date_bias %>% dplyr::count(species)

# map
occ_data_taxa_date_bias_vector <- occ_data_taxa_date_bias %>% 
  tidyr::drop_na(longitude, latitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

tm_shape(li, bbox = occ_data_taxa_date_bias_vector) +
  tm_polygons() +
  tm_shape(occ_data_taxa_date_bias_vector) +
  tm_dots(size = .2, shape = 21, col = "species_search",  
          palette = "Set1", title = "Species") +
  tm_graticules(lines = FALSE) +
  tm_layout(legend.text.fontface = "italic")

# spatial -----------------------------------------------------------------
# spatial occ
occ_data_taxa_date_bias_vector <- occ_data_taxa_date_bias %>%
  dplyr::mutate(x = longitude, lon = longitude, y = latitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_data_taxa_date_bias_vector

# extent
li_sa <- rnaturalearth::ne_countries(scale = 110, continent = "South America", returnclass = "sf")
li_sa %>% tm_shape() + tm_polygons() + tm_graticules(lines = FALSE)

# crop to limit
occ_data_taxa_date_bias_limit <- sf::st_crop(occ_data_taxa_date_bias_vector, li_sa) 
occ_data_taxa_date_bias_limit

# map
tm_shape(li, bbox = li_sa) +
  tm_polygons() +
  tm_shape(li_sa) +
  tm_borders(col = "black") +
  tm_shape(occ_data_taxa_date_bias_limit) +
  tm_dots(size = .2, shape = 21, col = "species_search",  
          palette = "Set1", title = "Species") +
  tm_graticules(lines = FALSE) +
  tm_layout(legend.text.fontface = "italic")

# prepare
occ_data_taxa_date_bias_limit <- occ_data_taxa_date_bias_limit %>%
  sf::st_drop_geometry() %>% 
  as.data.frame
occ_data_taxa_date_bias_limit

# desaggregation
occ_data_taxa_date_bias_limit_spatial <- ecospat::ecospat.occ.desaggregation(
  xy = occ_data_taxa_date_bias_limit,
  min.dist = .5, 
  by = "species") %>%
  tibble::as_tibble() %>% 
  dplyr::select(-x, -y)
occ_data_taxa_date_bias_limit_spatial

# verify
occ_data_taxa_date_bias %>% dplyr::count(species)
occ_data_taxa_date_bias_limit_spatial %>% dplyr::count(species)

# map
occ_data_taxa_date_bias_limit_spatial_vector <- occ_data_taxa_date_bias_limit_spatial %>%
  dplyr::mutate(x = longitude, lon = longitude, y = latitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_data_taxa_date_bias_limit_spatial_vector

tm_shape(li, bbox = li_sa) +
  tm_polygons() +
  tm_shape(occ_data_taxa_date_bias_limit_spatial_vector) +
  tm_dots(size = .2, shape = 21, col = "species_search",  
          palette = "Set1", title = "Species") +
  tm_graticules(lines = FALSE) +
  tm_layout(legend.text.fontface = "italic")

# verify filters ----------------------------------------------------------
# summary
occ_data %>% dplyr::count(species_search)
occ_data_taxa %>% dplyr::count(species)
occ_data_taxa_date %>% dplyr::count(species)
occ_data_taxa_date_bias %>% dplyr::count(species)
occ_data_taxa_date_bias_limit %>% dplyr::count(species)
occ_data_taxa_date_bias_limit_spatial %>% dplyr::count(species)

# summary total
sp_list_ <- sp_list %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_")
sp_list_

occ_filter_total <- occ_data %>% 
  dplyr::mutate(species = name %>% 
                  stringr::str_to_lower() %>% 
                  stringr::str_replace(" ", "_") %>% 
                  stringr::str_split(" ", simplify = TRUE) %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(1) %>% 
                  dplyr::pull()) %>%
  dplyr::filter(species %in% sp_list_) %>% 
  dplyr::count(species, base, name = "total")
occ_filter_total

occ_filter_taxa <- occ_data_taxa %>% 
  dplyr::count(species, base, name = "taxa")
occ_filter_taxa

occ_filter_taxa_date <- occ_data_taxa_date %>% 
  dplyr::count(species, base, name = "date") %>% 
  tidyr::drop_na()
occ_filter_taxa_date

occ_filter_taxa_date_spatial <- occ_data_taxa_date_bias_limit_spatial %>% 
  dplyr::count(species, base, name = "spatial")
occ_filter_taxa_date_spatial

occ_filter <- occ_filter_total %>% 
  dplyr::left_join(occ_filter_taxa, by = c("species", "base")) %>% 
  dplyr::left_join(occ_filter_taxa_date, by = c("species", "base")) %>% 
  dplyr::left_join(occ_filter_taxa_date_spatial, by = c("species", "base")) %>% 
  tidyr::drop_na(base) %>% 
  dplyr::mutate_all(~replace_na(., 0))
occ_filter

# export ------------------------------------------------------------------
# export
readr::write_csv(occ_data_taxa_date_bias_limit_spatial, 
                 paste0("occ_clean_taxa_date_bias_limit_spatial.csv"))

readr::write_csv(occ_filter, 
                 paste0("occ_table_filter_summary.csv"))

# -------------------------------------------------------------------------

#' -------
#' draft
#' -------

# oppc --------------------------------------------------------------------
# # import raster id
# var_id <- raster::raster("/home/mude/data/gitlab/r-sdm/00_pragmatico/00_present/01_variables/03_var/bio02.tif")
# var_id
# 
# var_id[!is.na(var_id)] <- raster::cellFromXY(var_id, raster::rasterToPoints(var_id)[, 1:2])
# plot(var_id)
# 
# # oppc
# occ_data_tax_date_spa_oppc <- occ_data_tax_date_spa %>% 
#   dplyr::mutate(oppc = raster::extract(var_id, dplyr::select(., longitude, latitude))) %>% 
#   dplyr::distinct(species, oppc, .keep_all = TRUE) %>% 
#   dplyr::filter(!is.na(oppc)) %>% 
#   dplyr::add_count(species) %>% 
#   dplyr::arrange(species)
# occ_data_tax_date_spa_oppc
# 
# # verify
# table(occ_data_tax_date_spa$species)
# table(occ_data_tax_date_spa_oppc$species)
# 
# # map
# ggplot() +
#   geom_sf(data = li %>% sf::st_crop(c(xmin = min(occ_data$longitude), ymin = min(occ_data$latitude), 
#                                       xmax = max(occ_data$longitude), ymax = max(occ_data$latitude))),
#           fill = "gray", alpha = .5) +
#   geom_point(data = occ_data_tax_date_spa_oppc, aes(x = longitude, y = latitude, color = species)) +
#   theme_bw()
# 
# # sample bias -------------------------------------------------------------
# # haddadus binotatus
# # sampbias
# sampbias_hb <- occ_data_tax_date_spa %>%
#   dplyr::filter(species == "haddadus_binotatus") %>% 
#   dplyr::select(species, longitude, latitude) %>% 
#   dplyr::rename(decimallongitude = longitude,
#                 decimallatitude = latitude) %>% 
#   sampbias::SamplingBias(x = ., 
#                          res = .1666667,
#                          buffer = .1666667*10, 
#                          biasdist = seq(0, 1e6, 2e4))
# sampbias_hb
# 
# # maps
# sampbias_hb %>% plot
# 
# # summary
# sampbias_hb_table <- sampbias_hb$biastable %>% 
#   dplyr::mutate(factors = rownames(sampbias_hb$biastable)) %>% 
#   tidyr::pivot_longer(-factors, names_to = "bias_dist", values_to = "bias_effect") %>% 
#   dplyr::mutate(bias_dist = as.numeric(bias_dist))
# sampbias_hb_table
# 
# ggplot(data = sampbias_hb_table) +
#   aes(x = factors, y = bias_effect) +
#   geom_boxplot(fill = viridis::viridis(4)) +
#   geom_jitter(width = .2, size = .1) +
#   coord_flip() +
#   theme_bw()
# 
# ggplot(data = ) +
#   aes(x = bias_dist/1e3, y = bias_effect, color = factors) +
#   geom_point() +
#   scale_color_viridis_d() +
#   scale_x_continuous(breaks = c(0, 250, 500, 750, 1000)) +
#   theme_bw()
# 
# # brachycephalus ephippium
# # sampbias
# sampbias_be <- occ_data_tax_date_spa %>%
#   dplyr::filter(species == "brachycephalus_ephippium") %>% 
#   dplyr::select(species, longitude, latitude) %>% 
#   dplyr::rename(decimallongitude = longitude,
#                 decimallatitude = latitude) %>% 
#   sampbias::SamplingBias(x = ., 
#                          res = 1, 
#                          biasdist = c(0, 5000, 10000))
# sampbias_be$biastable %>% row.names()
# 
# # summary
# summary(sampbias_be)
# sampbias_be %>% plot
# sampbias_be$biastable

# end ---------------------------------------------------------------------