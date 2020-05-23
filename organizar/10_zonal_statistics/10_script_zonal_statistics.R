### paper - barbeiros no parana ###

# andrade et al. 2017

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# 10/01/2017

### analise  ###

###-----------------------------------------------------------------------------------------###

# limpar o workspace e aumentar a memoria para o r
rm(list = ls())
gc() 
memory.limit(size = 17500000000) 

# instalar e carregar pacotes
library(raster)

###-----------------------------------------------------------------------------------------###

# diretorio
setwd("D:/_________parcerias/luciano_andreia/dados_espaciais/enm/output")

###-----------------------------------------------------------------------------------------###


# import data
enm.clim <- raster("climate_all_0k.asc")
plot(enm.clim)

enm.clim.40 <- enm.clim / 40
enm.clim.40
plot(enm.clim.40)

# layer
pr <- shapefile("municipios_parana_media_clima_paisagem_join_gcs_wgs84.shp")
plot(pr)
head(pr@data$FID_1)

pr.raster <- rasterize(pr, enm.clim.40, field = "FID_1")
pr.raster
plot(pr.raster)

# zonal
zonal.pr.clim <- zonal(enm.clim.40, pr.raster, "mean")
zonal.pr.clim
colnames(zonal.pr.clim) <- c("FID_1", "mean_clim")
head(zonal.pr.clim)

pr.merge.clim <- pr

pr.merge.clim@data <- merge(pr@data, zonal.pr.clim, by = "FID_1")

pr.raster.clim <- rasterize(pr.merge.clim, enm.clim, field = "mean_clim")
plot(pr.raster.clim)

writeRaster(pr.raster.clim, "zonal_clim.asc", format = "ascii")


###-----------------------------------------------------------------------------------------###

# import data
enm.lands <- raster("landscape_all_0k.asc")
plot(enm.lands)

enm.lands.40 <- enm.lands / 40
enm.lands.40
plot(enm.lands.40)

zonal.pr.lands <- zonal(enm.lands.40, pr.raster, "mean")
zonal.pr.lands
colnames(zonal.pr.lands) <- c("FID_1", "mean_lands")
zonal.pr.lands

pr.merge.lands <- pr

pr.merge.lands@data <- merge(pr@data, zonal.pr.lands, by = "FID_1")

pr.raster.lands <- rasterize(pr.merge.lands, enm.lands, field = "mean_lands")
plot(pr.raster.lands)

writeRaster(pr.raster.lands, "zonal_lands.asc", format = "ascii", over = T)


###-----------------------------------------------------------------------------------------###

# import data
enm.clim.lands <- raster("climate_landscape_0_1.asc")
plot(enm.clim.lands)

zonal.pr.clim.lands <- zonal(enm.clim.lands, pr.raster, "mean")
zonal.pr.clim.lands
colnames(zonal.pr.clim.lands) <- c("FID_1", "mean")
zonal.pr.clim.lands

pr.merge.clim.lands <- pr

pr.merge.clim.lands@data <- merge(pr@data, zonal.pr.clim.lands, by = "FID_1")

pr.raster.clim.lands <- rasterize(pr.merge.clim.lands, enm.clim.lands, field = "mean")
plot(pr.raster.clim.lands)

writeRaster(pr.raster.clim.lands, "zonal_clim_lands.asc", format = "ascii")


###-----------------------------------------------------------------------------------------###

### shapefile com as classes

pr.merge <- pr

pr.merge@data <- merge(pr.merge@data, zonal.pr.clim, by = "FID_1")

pr.merge@data <- merge(pr.merge@data, zonal.pr.lands, by = "FID_1")

head(pr.merge@data)

pr.merge@data$col <- NA

pr.merge@data$col[pr.merge@data$mean_clim >= .75 & pr.merge@data$mean_lands >= .75] <- "red"
pr.merge@data$col

pr.merge@data$col[pr.merge@data$mean_clim >= .75 & 
			pr.merge@data$mean_lands >= .25 &
			pr.merge@data$mean_lands < .75] <- "orange"
pr.merge@data$col

pr.merge@data$col[pr.merge@data$mean_clim >= .25 & 
			pr.merge@data$mean_clim < .75 &
			pr.merge@data$mean_lands >= .75] <- "orange"
pr.merge@data$col

pr.merge@data$col[pr.merge@data$mean_clim >= .25 & 
			pr.merge@data$mean_clim < .75 &
			pr.merge@data$mean_lands >= .25 &
			pr.merge@data$mean_lands < .75] <- "green"
pr.merge@data$col

pr.merge@data$col[pr.merge@data$mean_clim < .25 & pr.merge@data$mean_lands >= .75] <- "yellow"
pr.merge@data$col

pr.merge@data$col[pr.merge@data$mean_clim >= .75 & pr.merge@data$mean_lands < .25] <- "yellow"
pr.merge@data$col

pr.merge@data$col[pr.merge@data$mean_clim < .25 & 
			pr.merge@data$mean_lands >= .25 &
			pr.merge@data$mean_lands < .75] <- "lightblue"
pr.merge@data$col

pr.merge@data$col[pr.merge@data$mean_clim >= .25 & 
			pr.merge@data$mean_clim <= .75 &
			pr.merge@data$mean_lands < .25] <- "lightblue"
pr.merge@data$col

pr.merge@data$col[pr.merge@data$mean_clim < .25 & pr.merge@data$mean_lands < .25] <- "blue"
pr.merge@data$col

pr.merge@data$col

plot(pr.merge, col = pr.merge@data$col)

shapefile(pr.merge, "municipios_parana_media_clima_paisagem_join_suit_gcs_wgs84", over = T)

###-----------------------------------------------------------------------------------------###

# graficos
enm.clim.z <- raster("zonal_clim.asc") 
enm.clim.v <- enm.clim.z[]
length(enm.clim.v)

enm.lands.z <- raster("zonal_lands.asc") 
enm.lands.v <- enm.lands.z[]
length(enm.lands.v)

da <- data.frame(clim = enm.clim.v, lands = enm.lands.v)
da.na <- na.omit(da)
da.na

tiff('lands.tif', width = 18, height = 18, units = 'cm', res = 300, compression = 'lzw')
plot(da.na$clim, da.na$lands, type = "n",
	ylim = c(0 ,1), 
	xlab = "Climate Suitability", 
	ylab = "",
	col.axis = 'grey30',					
	cex.lab = 1.6,												
	cex.axis = 1.2, 
	las = 1)

h <- da.na[da.na$clim >= 0.75 & da.na$lands >= 0.75, ]
points(h$clim, h$lands, col = "red", pch = 20)

h.c_m.l <- da.na[da.na$clim >= 0.75 & da.na$lands >= 0.25 & da.na$lands < 0.75, ]
points(h.c_m.l$clim, h.c_m.l$lands, col = "orange", pch = 20)

m.c_h.l <- da.na[da.na$clim >= 0.25 & da.na$clim < 0.75 & da.na$lands >= 0.75, ]
points(m.c_h.l$clim, m.c_h.l$lands, col = "orange", pch = 20)

m.c_m.l <- da.na[da.na$clim >= 0.25 & da.na$clim < 0.75 & da.na$lands >= 0.25 & da.na$lands < 0.75, ]
points(m.c_m.l$clim, m.c_m.l$lands, col = "green", pch = 20)

l.c_h.l <- da.na[da.na$clim < 0.25 & da.na$lands >= 0.75, ]
points(l.c_h.l$clim, l.c_h.l$lands, col = "yellow", pch = 20)

h.c_l.l <- da.na[da.na$clim >= 0.75 & da.na$lands < 0.25, ]
points(h.c_l.l$clim, h.c_l.l$lands, col = "yellow", pch = 20)

l.c_m.l <- da.na[da.na$clim < 0.25 & da.na$lands >= 0.25 & da.na$lands < 0.75, ]
points(l.c_m.l$clim, l.c_m.l$lands, col = "lightblue", pch = 20)

m.c_l.l <- da.na[da.na$clim >= 0.25 & da.na$clim <= 0.75 & da.na$lands < 0.25, ]
points(m.c_l.l$clim, m.c_l.l$lands, col = "lightblue", pch = 20)

l <- da.na[da.na$clim < 0.25 & da.na$lands < 0.25, ]
points(l$clim, l$lands, col = "blue", pch = 20)

abline(h = 0.25, v = 0.25, col = "gray", lty = 2)
abline(h = 0.75, v = 0.75, col = "gray", lty = 2)

mtext("Landscape Suitability", 2, 2.7, cex = 1.5)	
dev.off()


###-----------------------------------------------------------------------------------------###




