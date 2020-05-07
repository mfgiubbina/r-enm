### script enm ###

# Maurício Humberto Vancine - mauricio.vancine@gmail.com
# Thadeu Sobral de Souza - thadeusobral@gmail.com 

###---------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, dismo, maptools, gam, randomForest, kernlab, rJava, 
               vegan, colorRamps)

# verify packages
search()

# data
data(wrld_simpl)

###---------------------------------------------------------------------------###

# 2. import data
# directory
setwd("E:/mega/_disciplina_enm_unesp_2017/scripts_r/enm/01_dados")

# ocurrences
po <- read.table("Bromelia_balansae.txt", h = T)
head(po, 10)

plot(po$long, po$lat, pch = 20)

# export shape
po.s <- po
coordinates(po.s) <- ~long + lat
crs(po.s) <- CRS("+proj=longlat +datum=WGS84")
po.s

plot(po.s, axes = T)
writeOGR(po.s, "pontos", "Bromelia_balansae", driver = "ESRI Shapefile")

#  variables
ti <- list.files(patt = "0k")
ti

en <- stack(ti)
names(en) <- paste0("bio", c("02", "04", "10", "16", "17"))
en

plot(en)

plot(en[[1]])
points(po$long, po$lat, pch = 20)


## extract coordinates for background
# coordinates
id <- 1:ncell(en)
head(id, 50)
length(id)

co <- xyFromCell(en, id)
head(co, 50)

plot(en[[1]])
points(co, pch = "o", cex = 1e-1)

# without NAs
va <- values(en)[, 1]
head(va, 50)
length(va)

co.va <- data.frame(co, va)
head(co.va, 20)

co.va.na <- na.omit(co.va)
head(co.va.na, 10)

cs <- co.va.na[, -3]
head(cs, 10)

colnames(cs) <- c("long", "lat")
head(cs, 10)

plot(en[[1]])
points(cs, pch = "o", cex = 1e-1)

###---------------------------------------------------------------------------###

# verify maxent

# copy maxent.jar in "C:\Users\john01\Documents\R\win-library\3.4\dismo\java"

jar <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
file.exists(jar)

###---------------------------------------------------------------------------###

### ENMs ###

# diretory
setwd("..")
getwd()
dir.create("02_ouput_unicos")
setwd("02_ouput_unicos")
getwd()


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

## algorithms

## 1. bioclim
# 1.1 calibration
train[, 1]

which(train[, 1] == 1)

train[which(train[, 1] == 1), -1]

Bioclim <- bioclim(train[which(train[, 1] == 1), -1])	
plot(Bioclim)
response(Bioclim)


# 1.2 projection
enm.Bioclim <- predict(en, Bioclim)	
enm.Bioclim


plot(enm.Bioclim)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")


plot(enm.Bioclim > 0.2)
plot(wrld_simpl, add = T, border = "dark grey")

plot(enm.Bioclim > 0.3)
plot(wrld_simpl, add = T, border = "dark grey")

plot(enm.Bioclim > 0.4)
plot(wrld_simpl, add = T, border = "dark grey")

plot(enm.Bioclim > 0.5)
plot(wrld_simpl, add = T, border = "dark grey")


# 1.3 evaluation
e.Bioclim <- evaluate(p = test[test[, 1] == 1, -1], 
                      a = test[test[, 1] == 0, -1], 
                      model = Bioclim)
e.Bioclim

str(e.Bioclim)

# roc
plot(e.Bioclim, "ROC")

# auc
e.Bioclim@auc

# find threshold
thr.Bioclim <- threshold(e.Bioclim)
thr.Bioclim

# tss
id <- which(e.Bioclim@t == thr.Bioclim$spec_sens)
tss <- e.Bioclim@TPR[id] + e.Bioclim@TNR[id] - 1
tss

# TPR: True positive rate
# TNR: True negative rate


## thresholds
# sum of the sensitivity and specificity
plot(enm.Bioclim >= thr.Bioclim$spec_sens)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")

# no omission
plot(enm.Bioclim >= thr.Bioclim$no_omission)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")


###---------------------------------------------------------------------------###

## 2. maxent	
# 2.1 calibration
Maxent <- maxent(train[, -1], train[, 1])	
Maxent

plot(Maxent)
response(Maxent)

# 2.2 projection
enm.Maxent <- predict(en, Maxent) 
enm.Maxent

plot(enm.Maxent)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")


# 2.3 evaluation
e.Maxent <- evaluate(p = test[test[, 1] == 1, -1], 
                     a = test[test[, 1] == 0, -1], 
                     model = Maxent)
e.Maxent

str(e.Maxent)

# roc
plot(e.Maxent, "ROC")

# auc
e.Maxent@auc

# find threshold
thr.Maxent <- threshold(e.Maxent)
thr.Maxent

# tss
id <- which(e.Maxent@t == thr.Maxent$spec_sens)
tss <- e.Maxent@TPR[id] + e.Maxent@TNR[id] - 1
tss

## thresholds
# sum of the sensitivity and specificity
plot(enm.Maxent >= thr.Maxent$spec_sens)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")

plot(enm.Maxent >= thr.Maxent$spec_sens)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[-pr.sample.train, ], pch = 20, col = "blue")
points(bc.specie[-bc.sample.train, ], pch = 3, col = "blue")

# no omission
plot(enm.Maxent >= thr.Maxent$no_omission)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")

plot(enm.Maxent >= thr.Maxent$no_omission)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[-pr.sample.train, ], pch = 20, col = "blue")
points(bc.specie[-bc.sample.train, ], pch = 3, col = "blue")

###----------------------------------------------------------------------------###


## 3. glm
# 3.1 calibration
GLM <- glm(pb ~ bio02 + bio04 + bio10 + bio16 + bio17, 
           family = binomial(link = "logit"), data= train)

summary(GLM)
response(GLM)

# 3.2 projection
enm.GLM <- predict(en, GLM) 
enm.GLM

plot(enm.GLM)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")


# 3.3 evaluation
e.GLM <- evaluate(p = test[test[, 1] == 1, -1], 
                  a = test[test[, 1] == 0, -1], 
                  model = GLM)
e.GLM

# roc
plot(e.GLM, "ROC")

# auc
e.GLM@auc

# find threshold
thr.GLM <- threshold(e.GLM)
thr.GLM

# tss
id <- which(e.GLM@t == thr.GLM$spec_sens)
tss <- e.GLM@TPR[id] + e.GLM@TNR[id] - 1
tss

## thresholds
# sum of the sensitivity and specificity
plot(enm.GLM >= thr.GLM$spec_sens)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")

# no omission
plot(enm.GLM >= thr.GLM$no_omission)
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20, col = "red")
points(bc.specie[bc.sample.train, ], pch = 3, col = "red")

###----------------------------------------------------------------------------###

# comparation
par(mfrow = c(1, 2))

# maxent
plot(enm.Maxent, main = "Maxent", col = matlab.like2((100)))
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20)

# glm
plot(enm.GLM, main = "GLM", col = matlab.like2((100)))
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20)
  
###----------------------------------------------------------------------------###

# maxent
# sum of the sensitivity and specificity
plot(enm.Maxent >= thr.Maxent$spec_sens, main = "Maxent")
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20)

# glm
# sum of the sensitivity and specificity
plot(enm.GLM >= thr.GLM$spec_sens, main = "GLM")
plot(wrld_simpl, add = T, border = "dark grey")
points(pr.specie[pr.sample.train, ], pch = 20)

###----------------------------------------------------------------------------###
