### script enm ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 28/07/2017

### ecoland ###

###---------------------------------------------------------------------------###

## memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

##packages
# install and load
if(!require(pacman)) install.packages("pacman")
pacman::p_load(raster, rgdal, data.table, colorRamps)

# verify packages
search()

###---------------------------------------------------------------------------###

## directory
setwd("E:/github/enmR/ouput/ecoland")

###---------------------------------------------------------------------------###

## data
# limite
br <- getData("GADM", country = "BRA", level = 0)
br

# enm climate
re <- raster(res = 1, xmn = -75, xmx = -33, ymn = -35, ymx = 6)
re[] <- rbeta(ncell(re), 2, 2)
re.b <- mask(re, br)

par(mar = c(1, 1, 1, 1))
plot(re.b, col = matlab.like2(100), axes = F, box = F)
plot(br, add = T)

# enm landscape
rl <- raster(res = 1, xmn = -75, xmx = -33, ymn = -35, ymx = 6)
rl[] <- rbeta(ncell(rl), 5, 2)
rl.b <- mask(rl, br)

plot(rl.b, col = magenta2green(100))
plot(br, add = T)

###---------------------------------------------------------------------------###

## data
# values
da <- data.table(id = 1:ncell(re.b), cl = re.b[], la = rl.b[],
                 xyFromCell(rl, 1:ncell(rl)))
da

da.na <- na.omit(da)
da.na

da <- data.table(da.na, col_4 = "NA", val_4 = 0, col_9 = "NA", val_9 = 0)
da

###---------------------------------------------------------------------------###

## classification 4
da[, col_4 := c("blue", "green", "orange", "red")
   [max.col(setDT(.(cl <= .5 & la <= .5, 
                    cl <= .5 & la > .5,
                    cl > .5 & la <= .5,
                    cl > .5 & la > .5)))]]
                                                 


da[, val_4 := c(0, .25, .75, 1)
   [max.col(setDT(.(cl <= .5 & la <= .5, 
                    cl <= .5 & la > .5,
                    cl > .5 & la <= .5,
                    cl > .5 & la > .5)))]]

                                                

###---------------------------------------------------------------------------###

## classification 9
da[, col_9 := c("blue", "cyan", "cyan4", "green", "chocolate", "yellow", 
                "orange", "dark green", "red")
   [max.col(setDT(.(cl <= .25 & la <= .25, 
   cl <= .25 & la > .25 & la <= .75,
   cl > .25 & cl <= .75 & la <= .25,
   cl > .25 & cl <= .75 & la > .25 & la <= .75,
   cl > .75 & la <= .25,
   cl <= .25 & la > .75,
   cl > .25 & cl <= .75 & la > .75,
   cl > .75 & la > .25 & la <= .75,
   cl > .75 & la > .75)))]]
                                       


da[, val_9 := c(0, .125, .25, .375, .5, .625, .75, .875, 1)
   [max.col(setDT(.(cl <= .25 & la <= .25, 
                    cl <= .25 & la > .25 & la <= .75,
                    cl > .25 & cl <= .75 & la <= .25,
                    cl > .25 & cl <= .75 & la > .25 & la <= .75,
                    cl > .75 & la <= .25,
                    cl <= .25 & la > .75,
                    cl > .25 & cl <= .75 & la > .75,
                    cl > .75 & la > .25 & la <= .75,
                    cl > .75 & la > .75)))]]

###---------------------------------------------------------------------------###

## write data
fwrite(da, "da_ecoland.csv")

###---------------------------------------------------------------------------###

## scatterplot
par(mar = c(5, 5, 2, 2))
plot(da$cl, da$la, type = "n",
     xlim = c(0, 1),
     ylim = c(0, 1),
     xlab = "Climate suitability", 
     ylab = "Landscape suitability",
     col.axis = 'grey30',					
     cex.lab = 1.6,												
     cex.axis = 1.2, 
     las = 1)

smoothScatter(da[, 2:3], nrpoints = 0, nbin = 30,
              colramp = colorRampPalette(c("gray100", "gray75")), add = T)

points(da$cl, da$la, col = da$col_4, pch = 20, cex = .8)

abline(h = .5 , v = .5, col = "gray30", lty = 2)

## barplot
barplot(table(da[, 7]) / nrow(da), ylim = c(0, max(table(da[, 7]) / nrow(da)) + .1),
        col = c("blue", "green", "orange", "red"))

## map
da.4 <- da[, c(4, 5, 7)]
da.4

gridded(da.4) <- ~x + y
da.4

ra.4 <- raster(da.4)
ra.4

plot(ra.4, col = c("blue", "green", "orange", "red"))

writeRaster(ra.4, "ecoland_04.tif", format = "GTiff")


# plots
par(mfrow = c(1, 3))

plot(re.b, col = matlab.like2(100), main = "Climate")
plot(br, add = T)

plot(rl.b, col = magenta2green(100), main = "Landscape")
plot(br, add = T)

plot(ra.4, col = c("blue", "green", "orange", "red"), main = "Ecoland - 4")
plot(br, add = T)

dev.off()

###---------------------------------------------------------------------------###


## scatterplot
par(mar = c(5, 5, 2, 2))
plot(da$cl, da$la, type = "n",
     xlim = c(0, 1),
     ylim = c(0, 1),
     xlab = "Climate suitability", 
     ylab = "Landscape suitability",
     col.axis = 'grey30',					
     cex.lab = 1.6,												
     cex.axis = 1.2, 
     las = 1)

smoothScatter(da[, 2:3], nrpoints = 0, nbin = 30,
              colramp = colorRampPalette(c("gray100", "gray75")), add = T)

points(da$cl, da$la, col = da$col_9, pch = 20, cex = .8)

abline(h = .25 , v = .25, col = "gray30", lty = 2)
abline(h = .75 , v = .75, col = "gray30", lty = 2)

## barplot
barplot(table(da[, 9]) / nrow(da), ylim = c(0, max(table(da[, 9]) / nrow(da)) + .1),
        col = c("blue", "cyan", "cyan4", "green", "chocolate", "yellow", "orange", 
                "dark green", "red"))

## map
da.9 <- da[, c(4, 5, 9)]
da.9

gridded(da.9) <- ~x + y
da.9

ra.9 <- raster(da.9)
ra.9

plot(ra.9, col = c("blue", "cyan", "cyan4", "green", "chocolate", "yellow", "orange", 
             "dark green", "red"))

writeRaster(ra.9, "ecoland_09.tif", format = "GTiff")


# plots
par(mfrow = c(1, 3))

plot(re.b, col = matlab.like2(100), main = "Climate")
plot(br, add = T)

plot(rl.b, col = magenta2green(100), main = "Landscape")
plot(br, add = T)

plot(ra.9, col = c("blue", "cyan", "cyan4", "green", "chocolate", "yellow", "orange", 
                   "dark green", "red"), main = "Ecoland - 9")
plot(br, add = T)

###---------------------------------------------------------------------------###
