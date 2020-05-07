### script download of data bases for enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 01/06/2017

###-----------------------------------------------------------------------------###

# clean and increase memory limite
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(maps, maptools, raster, rgdal)


###-----------------------------------------------------------------------------###
###                                 world cities
###-----------------------------------------------------------------------------###

# diretorio
setwd("D:/environmental_data/vector/base_maps")

# points
data(world.cities)
wc <- world.cities
wc
str(wc)

## exportar mundo
# tabela
write.table(wc, "world_cities.txt", sep = "\t", row.names = F, quote = F)
write.table(wc, "world_cities.csv", sep = ",", row.names = F, quote = F)

# shape
sh <- shapefile("world_cities_gcs_wgs84.shp")
sh

plot(sh, axes = T, pch = 20)

# paises
co <- unique(sh@data$country_et)
co

# shapes dos pontos
for(i in 1:length(co)){
  sh.s <- sh[sh@data$country_et == co[i], ]
  writeOGR(sh.s, "paises", paste0(co[i]), driver = "ESRI Shapefile")}


###-----------------------------------------------------------------------------###


# limits world
data(wrld_simpl)
ws <- wrld_simpl
ws
str(ws)

plot(ws, col = "lightgrey", border = "darkgrey", axes = T)
points(wc$long, wc$lat, pch = 20, cex = 0.1)

###-----------------------------------------------------------------------------###
