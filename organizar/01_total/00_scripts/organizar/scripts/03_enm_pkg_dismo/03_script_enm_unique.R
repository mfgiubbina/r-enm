### script enm dismo - unique algorithms ###

# mauricio vancine
# 12-12-2018

# memory
rm(list = ls())

# packages
library(dismo)
library(kernlab)
library(randomForest)
library(raster)
library(rgdal)
library(rJava)
library(sf)
library(tidyverse)
library(viridis)

# verify packages
search()

###---------------------------------------------------------------------------###

## import data

# 1. occ
# directory
setwd("/media/mauricio/data1/000_trabalho/00_empresas/aquaflora/05_distribuicao_especies/00_treinamento/02_data/00_occ")
dir()

# occurrences
occ <- readr::read_csv("occ_haddadus_binotatus_oppc.csv") %>% 
  dplyr::filter(name == "haddadus_binotatus")
occ

# create shapefile
occ.sh <-  occ %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ.sh

# plot
ggplot() + 
  geom_sf(data = occ.sh, col = "black", size = 2.5, alpha = .4) +
  theme_minimal()
  
# export shapefile
sf::st_write(occ.sh, "occ_haddadus_binotatus_oppc.shp")

###---------------------------------------------------------------------------###

##  2. variables
# directory
setwd("/media/mauricio/data1/000_trabalho/00_empresas/aquaflora/05_distribuicao_especies/00_treinamento/02_data/01_var/02_selection/04_pca")
dir()

# list files
ti <- dir(patt = ".tif$")
ti

# import rasters
var <- raster::stack(ti)
var

# plot
plot(var, col = viridis::viridis(100))

ggplot() +
  geom_raster(data = raster::rasterToPoints(var[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = pc01)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  geom_point(data = occ, aes(lon, lat), size = 2.5, alpha = .4) +
  theme_minimal()

###---------------------------------------------------------------------------###

## extract coordinates for background
# coordinates
## background coordinates
bc <- tibble::as.tibble(raster::rasterToPoints(var)[, 1:2])
bc
colnames(bc) <- c("lon", "lat")
bc

# plot
ggplot() +
  geom_raster(data = raster::rasterToPoints(var[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = pc01)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  geom_point(data = bc %>% dplyr::sample_n(1000), aes(lon, lat), size = 2.5, pch = 15, alpha = .4) +
  theme_minimal()

###---------------------------------------------------------------------------###

# verify maxent
# copy maxent.jar in "C:\Users\seu_nome\Documents\R\win-library\3.5.1\dismo\java"
file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))

###---------------------------------------------------------------------------###

### enms ###

# diretory
setwd("/media/mauricio/data1/000_trabalho/00_empresas/aquaflora/05_distribuicao_especies/00_treinamento")

dir.create("03_modelos")
setwd("03_modelos")

dir.create("01_modelos_unicos")
setwd("01_modelos_unicos")
getwd()


# select presence points
pr.species <- occ %>% 
  dplyr::select(lon, lat)
pr.species

# select background points
bc.species <- bc %>% 
  dplyr::sample_n(nrow(pr.species))
bc.species

# plot
ggplot() +
  geom_raster(data = raster::rasterToPoints(var[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = pc01)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  geom_point(data = bc.species, aes(lon, lat), size = 2.5, pch = 15, alpha = .4) +
  geom_point(data = pr.species, aes(lon, lat), size = 2.5, pch = 20, alpha = .4) +
  theme_minimal()

###---------------------------------------------------------------------------###

## preparing data - train and test presence data	

# sample train
pr.sample.train.id <- sample(nrow(pr.species), round(.7 * nrow(pr.species)))

# select train data
pr.sample.train <- pr.species %>% 
  dplyr::slice(pr.sample.train.id)
pr.sample.train

# select test data
pr.sample.test <- pr.species %>% 
  dplyr::slice(-pr.sample.train.id)
pr.sample.test

# plot
ggplot() +
  geom_raster(data = raster::rasterToPoints(var[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = pc01)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), size = 3.5, pch = 20, alpha = .5) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", size = 3.5, pch = 20, alpha = .7) +
  theme_minimal()


## train and test background data	
# sample train
bc.sample.train.id <- sample(nrow(bc.species), round(.7 * nrow(bc.species)))
bc.sample.train.id

# select train data
bc.sample.train <- bc.species %>% 
  dplyr::slice(bc.sample.train.id)
bc.sample.train

# select test data
bc.sample.test <- bc.species %>% 
  dplyr::slice(-bc.sample.train.id)
bc.sample.test

# plot
ggplot() +
  geom_raster(data = raster::rasterToPoints(var[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = pc01)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  geom_point(data = bc.sample.train, aes(lon, lat), pch = 15, size = 3, alpha = .5) +
  geom_point(data = bc.sample.test, aes(lon, lat), color = "darkgreen", pch = 15, size = 3, alpha = .7) +
  theme_minimal()


# selecting data to enm
# train
train <- dismo::prepareData(x = var, 
                            p = pr.sample.train, 
                            b = bc.sample.train) %>%  na.omit
train

# test
test <- dismo::prepareData(x = var, 
                           p = pr.sample.test, 
                           b = bc.sample.test) %>% na.omit
test
  	
###---------------------------------------------------------------------------###

## algorithms
## 1. bioclim
# 1.1 calibration
train.cal <- train %>% 
  dplyr::filter(pb == 1) %>% 
  dplyr::select(-1)
train.cal

Bioclim <- dismo::bioclim(train.cal)	
plot(Bioclim)
response(Bioclim)

# 1.2 projection
enm.Bioclim <- dismo::predict(var, Bioclim, progress = "text")	
enm.Bioclim

# plot
ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.Bioclim) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  theme_minimal()

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.Bioclim) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.Bioclim) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

# 1.3 evaluation
e.Bioclim <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-1), 
                             a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-1), 
                             model = Bioclim)
e.Bioclim

str(e.Bioclim)

# roc
plot(e.Bioclim, "ROC")

# auc
e.Bioclim@auc

# find threshold
thr.Bioclim <- dismo::threshold(e.Bioclim)
thr.Bioclim

# tss
id <- which(e.Bioclim@t == thr.Bioclim$spec_sens)
tss <- e.Bioclim@TPR[id] + e.Bioclim@TNR[id] - 1
tss

# TPR: True positive rate
# TNR: True negative rate


## thresholds
# sum of the sensitivity and specificity
thr.Bioclim$spec_sens

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.Bioclim >= thr.Bioclim$spec_sens) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

# no omission
thr.Bioclim$no_omission

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.Bioclim >= thr.Bioclim$no_omission) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

###---------------------------------------------------------------------------###

## 2. maxent	
# 2.1 calibration
Maxent <- dismo::maxent(x = train %>% dplyr::select(-1), 
                        p = train %>% dplyr::select(1))	
Maxent

plot(Maxent)
response(Maxent)

# 2.2 projection
enm.Maxent <- dismo::predict(var, Maxent, progress = "text") 
enm.Maxent

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.Maxent) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  theme_minimal()

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.Maxent) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.Maxent) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

# 2.3 evaluation
e.Maxent <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-1), 
                            a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-1),  
                            model = Maxent)
e.Maxent

# roc
plot(e.Maxent, "ROC")

# auc
e.Maxent@auc

# find threshold
thr.Maxent <- dismo::threshold(e.Maxent)
thr.Maxent

# tss
id <- which(e.Maxent@t == thr.Maxent$spec_sens)
tss <- e.Maxent@TPR[id] + e.Maxent@TNR[id] - 1
tss

## thresholds
# sum of the sensitivity and specificity
thr.Maxent$spec_sens

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.Maxent >= thr.Maxent$spec_sens) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

# no omission
thr.Maxent$no_omission

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.Maxent >= thr.Maxent$no_omission) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

###----------------------------------------------------------------------------###

## 3. glm
# 3.1 calibration
GLM <- glm(pb ~ ., family = binomial(link = "logit"), data = train)

summary(GLM)
response(GLM)

# 3.2 projection
enm.GLM <- dismo::predict(var, GLM, progress = "text") 
enm.GLM

# map
ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.GLM) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  theme_minimal()

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.GLM) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.GLM) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()


# 3.3 evaluation
e.GLM <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-1), 
                         a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-1),
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
thr.GLM$spec_sens

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.GLM >= thr.GLM$spec_sens) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

# no omission
thr.GLM$no_omission

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.GLM >= thr.GLM$no_omission) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

###----------------------------------------------------------------------------###

# comparation
par(mfrow = c(1, 3))

# maxent
plot(enm.Bioclim, main = "Bioclim", col = matlab.like2((100)), legend = FALSE)

# maxent
plot(enm.Maxent, main = "Maxent", col = matlab.like2((100)), legend = FALSE)

# glm
plot(enm.GLM, main = "GLM", col = matlab.like2((100)), legend = FALSE)

###----------------------------------------------------------------------------###

# sum of the sensitivity and specificity

# bioclim
plot(enm.Bioclim >= thr.Bioclim$spec_sens, main = "Bioclim", legend = FALSE)

# maxent
plot(enm.Maxent >= thr.Maxent$spec_sens, main = "Maxent", legend = FALSE)

# glm
plot(enm.GLM >= thr.GLM$spec_sens, main = "GLM", legend = FALSE)

###----------------------------------------------------------------------------###

###----------------------------------------------------------------------------###

## 4. svm kernlab
# 4.1 calibration
SVM.kernlab <- kernlab::ksvm(pb ~ ., data = train)

summary(SVM.kernlab)

# 4.2 projection
enm.SVM.kernlab <- dismo::predict(var, SVM.kernlab, progress = "text") 
enm.SVM.kernlab

# map
ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.SVM.kernlab) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  theme_minimal()


# 4.3 evaluation
e.SVM.kernlab <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-1), 
                                 a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-1),
                                 model = SVM.kernlab)
e.SVM.kernlab

# roc
plot(e.SVM.kernlab, "ROC")

# auc
e.SVM.kernlab@auc

# find threshold
thr.SVM.kernlab <- threshold(e.SVM.kernlab)
thr.SVM.kernlab

# tss
id <- which(e.SVM.kernlab@t == thr.SVM.kernlab$spec_sens)
tss <- e.SVM.kernlab@TPR[id] + e.SVM.kernlab@TNR[id] - 1
tss

## thresholds
# sum of the sensitivity and specificity
thr.SVM.kernlab$spec_sens

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.SVM.kernlab >= thr.SVM.kernlab$spec_sens) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

# no omission
thr.SVM.kernlab$no_omission

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.SVM.kernlab >= thr.SVM.kernlab$no_omission) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

###----------------------------------------------------------------------------###

## 5. svm e1071
# 5.1 calibration
SVM.e1071 <- e1071::svm(pb ~ ., data = train, cost = 100, gamma = 1)

summary(SVM.e1071)

# 5.2 projection
enm.SVM.e1071 <- dismo::predict(var, SVM.e1071, progress = "text") 
enm.SVM.e1071

# map
ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.SVM.e1071) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  theme_minimal()


# 5.3 evaluation
e.SVM.e1071 <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-1), 
                               a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-1),
                               model = SVM.e1071)
e.SVM.e1071

# roc
plot(e.SVM.e1071, "ROC")

# auc
e.SVM.e1071@auc

# find threshold
thr.SVM.e1071 <- threshold(e.SVM.e1071)
thr.SVM.e1071

# tss
id <- which(e.SVM.e1071@t == thr.SVM.e1071$spec_sens)
tss <- e.SVM.e1071@TPR[id] + e.SVM.e1071@TNR[id] - 1
tss

## thresholds
# sum of the sensitivity and specificity
thr.SVM.e1071$spec_sens

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.SVM.e1071 >= thr.SVM.e1071$spec_sens) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()

# no omission
thr.SVM.e1071$no_omission

ggplot() +
  geom_raster(data = raster::rasterToPoints(enm.SVM.e1071 >= thr.SVM.e1071$no_omission) %>% tibble::as_tibble(), 
              aes(x, y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_point(data = pr.sample.train, aes(lon, lat), pch = 20, size = 2.5, alpha = .7) +
  geom_point(data = pr.sample.test, aes(lon, lat), color = "darkgreen", pch = 20, size = 2.5, alpha = .7) +
  theme_minimal()




