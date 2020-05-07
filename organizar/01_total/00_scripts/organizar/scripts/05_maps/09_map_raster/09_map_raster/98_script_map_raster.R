### script raster map ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, spocc, ggplot2, data.table, dismo, maps, viridis, colorRamps, RCurl,
               stringr, lettercase, dplyr)

## functions
eval(parse(text = getURL("https://gist.githubusercontent.com/mauriciovancine/840428ae5511e78b5681af6f995e6348/raw/12228ca55408ba1cb06357a28ed86be6933a4d25/script_function_scalebar_north_arrow.R", 
                         ssl.verifypeer = F)))

## fonts
# https://nrelscience.org/2013/05/30/this-is-how-i-did-it-mapping-in-r-with-ggplot2/

# https://github.com/atredennick/Plot_geographical_data

# http://rmhogervorst.nl/cleancode/blog/2017/01/06/plotting-a-map-with-ggplot2.html

###---------------------------------------------------------------------###

## data
# points
po <- distinct(occ2df(occ(query = "Haddadus binotatus", 
                          from = c("gbif", "idigbio", "inat", "obis", "ala"), 
                          has_coords = T))[, 1:3])

po$longitude <- as.numeric(po$longitude)
po$latitude <- as.numeric(po$latitude)

plot(po$longitude, po$latitude, pch = 20)

# vector
br <- getData("GADM", country = "BRA", level = 0)
br

# raster
en <- crop(mask(aggregate(getData(name = "worldclim", var = "bio", res = 10, download = T), fact = 6, fun = "mean", expand = T), br), br)[[1]]
en

plot(en, col = viridis(100))

# convert raster to points for plotting
dt <- data.table(rasterToPoints(en))
dt

# column names
colnames(dt) <- c("lon", "lat", "bio01")
dt

# Now make the map
ggplot(data = dt, aes(y = lat, x = lon)) +
  
  geom_polygon(data = br, aes(x = long, y = lat, group = group),
               color = "black", fill = adjustcolor("white", 1), size = .01) + 
  
  geom_raster(aes(fill = bio01), alpha = .7) +
  
  geom_point(data = po, aes(x = longitude, y = latitude), shape = 21, 
            size = 1, fill = adjustcolor("black", .9)) +
  
  scale_fill_gradientn("bio01", colours = matlab.like(100)) + 
  #scale_fill_gradientn("Suitability", colours = viridis(100)) + 
  
  coord_equal() +
  
  theme_minimal() +
  
  scaleBar(lon = -48, lat = -33, distanceLon = 500, distanceLat = 100,
           distanceLegend = -100, dist.unit = "km", legend.size = 3,
           arrow.length = 350, arrow.distance = 200, arrow.North.size = 5) +
  
  theme(legend.position = c(.2, .3),
        legend.background = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_blank()) +
  
  annotate("text", -67, -15, label= "Bio 01", size = 5) +
  
  labs(title = bquote("" ~ italic(.(unique(po$name)[1]))))


ggsave("map.tiff", wi = 15, he = 15, un = "cm", dpi = 100)

###---------------------------------------------------------------------###
