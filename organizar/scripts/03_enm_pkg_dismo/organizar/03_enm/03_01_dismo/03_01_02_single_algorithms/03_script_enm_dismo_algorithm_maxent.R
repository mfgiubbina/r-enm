### script enm - dismo ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com

### maxent ###

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

## algorithm - maxent
# calibration
maxent <- maxent(train[, -1], train[, 1])
plot(maxent)
response(maxent)


# 1.2 projection
enm.maxent <- predict(en, maxent)	
enm.maxent


plot(enm.maxent)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")

writeRaster(enm.maxent, "enm_maxent.tif", format = "GTiff")


# 1.3 evaluation
e.maxent <- evaluate(p = test[test[, 1] == 1, -1], 
                      a = test[test[, 1] == 0, -1], 
                      model = maxent)
e.maxent

str(e.maxent)

# roc
plot(e.maxent, "ROC")

# auc
e.maxent@auc

# find threshold
thr.maxent <- threshold(e.maxent)
thr.maxent

# tss
id <- which(e.maxent@t == thr.maxent$spec_sens)
tss <- e.maxent@TPR[id] + e.maxent@TNR[id] - 1
tss

# TPR: True positive rate
# TNR: True negative rate


## thresholds
# sum of the sensitivity and specificity
plot(enm.maxent >= thr.maxent$spec_sens)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[-pr.sample.train, ], pch = 20, col = "blue")
points(bc.specie[-bc.sample.train, ], pch = 3, col = "blue")

# no omission
plot(enm.maxent >= thr.maxent$no_omission)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[-pr.sample.train, ], pch = 20, col = "blue")
points(bc.specie[-bc.sample.train, ], pch = 3, col = "blue")

###---------------------------------------------------------------------------###
