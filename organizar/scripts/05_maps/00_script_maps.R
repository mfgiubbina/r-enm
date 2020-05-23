### script enm - dismo ###

## maps ##

# mauricio vancine
# 27-03-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(tidyverse)
library(wesanderson)

# directory
setwd("/home/mauricio/Downloads/enm/enms/00_ensemble_freq")


# import data -------------------------------------------------------------
# enm
enm <- stack(dir(patt = ".tif$"))
enm

# occurrences
occ <- readr::read_csv("/home/mauricio/Downloads/enm/occ_an_filtered.csv")
occ

# map ---------------------------------------------------------------------
ggplot() +
  geom_raster(data = raster::rasterToPoints(enm) %>% tibble::as_tibble(), 
              aes(x, y, fill = ensemble_freq_haddadus_binotatus)) +
  scale_fill_gradientn(colours = wes_palette("Zissou1", 10, type = "continuous")) +
  geom_point(data = occ %>% dplyr::filter(species == "haddadus_binotatus"), 
             aes(x = longitude, y = latitude)) +
  coord_equal() +
  theme_bw() +
  labs(x = "longitude", y = "latitude", fill = "adeq", 
       title = "Haddadus bibotatus - bio04, bio08, bio14, bio16") +
  theme(plot.title = element_text(face = "bold.italic", size = 20),
        legend.title = element_text(face = "bold", size = 15),
        axis.title = element_text(size = 17),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  ggsn::north(data = sf::st_as_sf(occ, coords = c("longitude", "latitude")), symbol = 12) +
  ggsn::scalebar(data = sf::st_as_sf(occ, coords = c("longitude", "latitude")), 
                 location = "bottomleft",
                 dist = 500, dist_unit = "km", transform = TRUE, model = "WGS84")

# end ---------------------------------------------------------------------