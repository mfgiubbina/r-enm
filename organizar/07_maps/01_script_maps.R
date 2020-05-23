# script description #
# aim:    maps
# author: mauricio vancine
# date:   03-06-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(ggspatial)
library(raster)
library(rgdal)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(wesanderson)

# import data -------------------------------------------------------------
# limit
br <- rnaturalearth::ne_countries(scale = "large", country = "Brazil", returnclass = "sf")
br

# raster
ra <- raster::getData("worldclim", var = "bio", res = 10) %>% 
  raster::crop(br) %>% 
  raster::mask(br)
ra

# fig ---------------------------------------------------------------------
ggplot() +
  geom_raster(data = raster::rasterToPoints(ra$bio1) %>% tibble::as_tibble(),
              aes(x, y, fill = bio1)) +
  geom_sf(data = br, fill = NA) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", n = 100, type = "continuous")) +
  labs(x = "Longitude", y = "Latitude", fill = "bio01", title = "BIO01") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 20, face = "plain"),
        legend.position = c(.75, .17)) +
  annotation_scale(location = "br", width_hint = .3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(1.5, "cm"), pad_y = unit(.8, "cm"),
                         style = north_arrow_fancy_orienteering)


# end ---------------------------------------------------------------------



