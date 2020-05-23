### script amphibians vector map ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, ggplot2, data.table, dismo, maps, viridis, colorRamps, RCurl)

## functions
eval(parse(text = getURL("https://gist.githubusercontent.com/mauriciovancine/840428ae5511e78b5681af6f995e6348/raw/12228ca55408ba1cb06357a28ed86be6933a4d25/script_function_scalebar_north_arrow.R", 
                         ssl.verifypeer = F)))

## fonts
# https://nrelscience.org/2013/05/30/this-is-how-i-did-it-mapping-in-r-with-ggplot2/

# https://github.com/atredennick/Plot_geographical_data

# http://sharpsightlabs.com/blog/new-amazon-hq/

###---------------------------------------------------------------------###

## data
# directory
setwd("E:/github/enmR/data")

# points
po <- fread("Bromelia_balansae.txt")
po

# limit
li <- shapefile("south_america_gcs_wgs84.shp")
li

# map
ggplot() +
  
  geom_polygon(data = li, aes(x = long, y = lat, group = group),
              color = "white", fill = "gray80", size = .01) +
  
  geom_point(data = po, aes(x = long, y = lat, fill = sp),
             shape = 20, size = 3.5, alpha = .6) +
  
  coord_map() +
  theme_minimal() +
  
  scaleBar(lon = -110, lat = -50, distanceLon = 1000, distanceLat = 150,
           distanceLegend = -200, dist.unit = "km", legend.size = 3,
           arrow.length = 450, arrow.distance = 350, arrow.North.size = 5) +
  
  labs(fill = "Species") +
  
  theme(legend.position = c(.3, .5),
        legend.background = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))

  
ggsave("map.tiff", wi = 15, he = 15, un = "cm", dpi = 100)



###---------------------------------------------------------------------###
