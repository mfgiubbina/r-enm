### script enm - dismo ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com

### geographic distance ###

###---------------------------------------------------------------------------###

## memory 
# clean and increase
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

###---------------------------------------------------------------------------###
## packages 

# install and load packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, dismo, maptools, vegan, colorRamps)

# verify packages
search()

# data
data(wrld_simpl)

###---------------------------------------------------------------------------###

## data
# directory
setwd("E:/github/enmR/data")

# ocurrences
po <- read.table("Bromelia_balansae.txt", h = T)
head(po, 10)

plot(po$long, po$lat, pch = 20)

#  variables
ti <- list.files(patt = "0k")
ti

en <- stack(ti)
names(en) <- paste0("bio", c("02", "04", "10", "16", "17"))
en

plot(en)

plot(en[[1]])
points(po$long, po$lat, pch = 20)

## background coordinates
co <- na.omit(data.frame(xyFromCell(en, 1:ncell(en)), en[[1]][]))
cs <- co[, -3]

colnames(cs) <- c("long", "lat")
head(cs, 10)

plot(en[[1]])
points(cs[sample(nrow(cs), 1000), ], pch = 20, cex = .8)


###---------------------------------------------------------------------------###

## enms
# selecting presence and absence
pr.specie <- po[, 2:3]
pr.specie
plot(pr.specie, pch = 20)


id.background <- sample(nrow(cs), nrow(pr.specie))
id.background

bc.specie <- cs[id.background, ]
bc.specie
plot(bc.specie, pch = 20)

plot(en[[1]])
points(pr.specie, pch = 20)
points(bc.specie, pch = 3)

###---------------------------------------------------------------------------###

## preparing data
# train and test data	
pr.sample.train <- sample(nrow(pr.specie), round(0.7 * nrow(pr.specie)))
pr.sample.train

plot(en[[1]])
points(pr.specie, pch = 20)
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(pr.specie[-pr.sample.train, ], pch = 20, col = "blue")


bc.sample.train <- sample(nrow(bc.specie), round(0.7 * nrow(bc.specie)))
bc.sample.train

plot(en[[1]])
points(bc.specie, pch = 3)
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")
points(bc.specie[-bc.sample.train, ], pch = 3, col = "blue")


# selecting data
train <- na.omit(prepareData(x = en, 
                             p = pr.specie[pr.sample.train, ], 
                             b = bc.specie[bc.sample.train, ]))
train

test <- na.omit(prepareData(x = en, 
                            p = pr.specie[-pr.sample.train, ], 
                            b = bc.specie[-bc.sample.train, ]))
test

###---------------------------------------------------------------------------###

## algorithm - geographic distance
# calibration
ext <- extent(-90, -32, -33, 23)

seamask <- crop(en[[1]], ext)
seamask

geodist <- geoDist(pr.specie[pr.sample.train, ], lonlat = T)
geodist

# projection
gm.geodist <- predict(seamask, distm, mask = T)
gm.geodist

plot(gm.geodist)
plot(wrld_simpl, add = T, border='dark grey')

writeRaster(gm.geodist, "gm.geodist.tif", format = "GTiff")


# evaluation
e.geodist <- evaluate(geodist, p = pr.specie[-pr.sample.train, ], 
              a = bc.specie[-bc.sample.train, ])

e.geodist

# roc
plot(e.geodist, "ROC")

# auc
e.geodist@auc

# find threshold
thr.geodist <- threshold(e.geodist)
thr.geodist

## thresholds
# sum of the sensitivity and specificity
plot(ds >= thr.geodist$spec_sens)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[-pr.sample.train, ], pch = 20, col = "blue")
points(bc.specie[-bc.sample.train, ], pch = 3, col = "blue")

###---------------------------------------------------------------------------###
