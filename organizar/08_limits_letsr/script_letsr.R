# package
library(letsR)
library(sf)
library(tidyverse)

# data
# download.file("https://s3.amazonaws.com/hp3-shapefiles/Mammals_Terrestrial.zip",
#               destfile = "Mammals_Terrestrial.zip")
# 
# unzip("Mammals_Terrestrial.zip")
# unlink("Mammals_Terrestrial.zip")

mammal_shapes <- sf::st_read("Mammals_Terrestrial") %>% 
  sf::as_Spatial()
mammal_shapes

# spatial distribution polygons of south american -------------------------
mammal_spatial <- letsR::lets.presab(shapes = mammal_shapes, 
                                    xmn = -93, 
                                    xmx = -29,
                                    ymn = -57, 
                                    ymx = 15)
mammal_spatial

summary(mammal_spatial)

# species richness map
plot(mammal_spatial, xlab = "Longitude", ylab = "Latitude")


# end ---------------------------------------------------------------------
