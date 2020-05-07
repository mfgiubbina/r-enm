### script enm - dismo ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com

### bioclim ###

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
co <- na.omit(rasterToPoints(en))

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

## algorithm - bioclim
# calibration
bioclim <- bioclim(train[which(train[, 1] == 1), -1])	
plot(bioclim)
response(bioclim)


# 1.2 projection
enm.bioclim <- predict(en, bioclim)	
enm.bioclim


plot(enm.bioclim)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")

writeRaster(enm.bioclim, "enm_bioclim.tif", format = "GTiff")


# 1.3 evaluation
e.bioclim <- evaluate(p = test[test[, 1] == 1, -1], 
                      a = test[test[, 1] == 0, -1], 
                      model = bioclim)
e.bioclim

str(e.bioclim)

# roc
plot(e.bioclim, "ROC")

# auc
e.bioclim@auc

# find threshold
thr.bioclim <- threshold(e.bioclim)
thr.bioclim

# tss
id <- which(e.bioclim@t == thr.bioclim$spec_sens)
tss <- e.bioclim@TPR[id] + e.bioclim@TNR[id] - 1
tss

# TPR: True positive rate
# TNR: True negative rate


## thresholds
# sum of the sensitivity and specificity
plot(enm.bioclim >= thr.bioclim$spec_sens)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[-pr.sample.train, ], pch = 20, col = "blue")
points(bc.specie[-bc.sample.train, ], pch = 3, col = "blue")

# no omission
plot(enm.bioclim >= thr.bioclim$no_omission)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[-pr.sample.train, ], pch = 20, col = "blue")
points(bc.specie[-bc.sample.train, ], pch = 3, col = "blue")

###---------------------------------------------------------------------------###
