### script enm - dismo ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com

### svm ###

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
pacman::p_load(raster, rgdal, dismo, maptools, vegan, kernlab, colorRamps)

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

## algorithm - svm
# calibration
svm <- ksvm(pb ~ bio02 + bio04 + bio10 + bio16 + bio17, data = train)
svm

# 1.2 projection
enm.svm <- predict(en, svm)	
enm.svm


plot(enm.svm)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")

writeRaster(enm.svm, "enm_svm.tif", format = "GTiff")


# 1.3 evaluation
e.svm <- evaluate(p = test[test[, 1] == 1, -1], 
                  a = test[test[, 1] == 0, -1], 
                  model = svm)
e.svm

str(e.svm)

# roc
plot(e.svm, "ROC")

# auc
e.svm@auc

# find threshold
thr.svm <- threshold(e.svm)
thr.svm

# tss
id <- which(e.svm@t == thr.svm$spec_sens)
tss <- e.svm@TPR[id] + e.svm@TNR[id] - 1
tss

# TPR: True positive rate
# TNR: True negative rate


## thresholds
# sum of the sensitivity and specificity
plot(enm.svm >= thr.svm$spec_sens)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[-pr.sample.train, ], pch = 20, col = "blue")
points(bc.specie[-bc.sample.train, ], pch = 3, col = "blue")

# no omission
plot(enm.svm >= thr.svm$no_omission)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[-pr.sample.train, ], pch = 20, col = "blue")
points(bc.specie[-bc.sample.train, ], pch = 3, col = "blue")

###---------------------------------------------------------------------------###
