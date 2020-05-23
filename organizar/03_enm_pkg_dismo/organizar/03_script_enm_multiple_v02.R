### script enm - dismo ###

## multiple algorithms ##

# mauricio vancine
# 17-03-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(dismo)
library(gam)
library(ggsn)
library(kernlab)
library(randomForest)
library(raster)
library(rgdal)
library(rJava)
library(sf)
library(terra)
library(tidyverse)
library(viridis)

# occurrences -------------------------------------------------------------
# directory
setwd("/media/mauricio/data/gitlab/r-enm/data/occ")
dir()

# occurrences
occ <- readr::read_csv("occ_an_filtered.csv")
occ

# plot
ggplot(data = occ) +
  aes(x = longitude, y = latitude, color = species) +
  geom_point(size = 2.5, alpha = .4) +
  scale_color_brewer(palette = "Set1") +
  coord_equal() +
  theme_bw() +
  ggsn::north(data = sf::st_as_sf(occ, coords = c("longitude", "latitude")), symbol = 12) +
  ggsn::scalebar(data = sf::st_as_sf(occ, coords = c("longitude", "latitude")), location = "bottomleft",
                 dist = 500, dist_unit = "km", transform = TRUE, model = "WGS84")

# variables ---------------------------------------------------------------
# directory
setwd("/media/mauricio/data/gitlab/r-enm/data/var/05_pca")
dir()

# list files
ti <- dir(patt = ".tif$")
ti

# import rasters
var <- raster::stack(ti) %>% 
  raster::brick()
var

# plot
ggplot() +
  geom_raster(data = raster::rasterToPoints(var[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = pc01)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  geom_point(data = occ, aes(x = longitude, y = latitude, color = species), size = 2.5, alpha = .4) +
  scale_color_brewer(palette = "Set1") +
  coord_equal() +
  theme_bw() +
  ggsn::north(data = sf::st_as_sf(occ, coords = c("longitude", "latitude")), symbol = 12) +
  ggsn::scalebar(data = sf::st_as_sf(occ, coords = c("longitude", "latitude")), location = "bottomleft",
                 dist = 500, dist_unit = "km", transform = TRUE, model = "WGS84")

# extract background coordinates ------------------------------------------
# background coordinates
bg <- raster::rasterToPoints(var) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(1, 2) %>% 
  dplyr::rename(longitude = x, latitude = y)
bg

# plot
ggplot() +
  geom_raster(data = raster::rasterToPoints(var[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = pc01)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  geom_point(data = bg %>% dplyr::sample_n(1000), aes(x = longitude, y = latitude), size = 2.5, alpha = .4) +
  coord_equal() +
  theme_bw() +
  ggsn::north(data = sf::st_as_sf(occ, coords = c("longitude", "latitude")), symbol = 12) +
  ggsn::scalebar(data = sf::st_as_sf(occ, coords = c("longitude", "latitude")), location = "bottomleft",
                 dist = 500, dist_unit = "km", transform = TRUE, model = "WGS84") 

# verify maxent -----------------------------------------------------------
# copy maxent.jar in "C:\Users\seu_nome\Documents\R\win-library\3.5.1\dismo\java"
file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))

# enms --------------------------------------------------------------------
# diretory
setwd("..")
setwd("..")
dir.create("enms")
setwd("enms")
dir()

# enms
for(i in unique(occ$species)[4]){ # for to each specie

  # object for evaluation
  eval.species <- tibble::tibble()
  
  # selecting presence and absence data
  pr.specie <- occ %>% dplyr::filter(species == i) %>% 
    dplyr::select(longitude, latitude) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  bg.specie <- bg %>% dplyr::sample_n(nrow(pr.specie)) %>% 
    dplyr::select(longitude, latitude) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  # replicates
  for(r in seq(2)){	# number of replicas
    
    # object for evaluation
    eval.algorithm <- tibble::tibble()
    
    # partitioning data	
    pr.sample.train <- pr.specie %>% 
      dplyr::sample_frac(.7) %>% 
      dplyr::select(id) %>% 
      dplyr::pull()
    bg.sample.train <- bg.specie %>% 
      dplyr::sample_frac(.7) %>% 
      dplyr::select(id) %>% 
      dplyr::pull()
    
    # train and test data
    train <- dismo::prepareData(x = var, 
                                p = pr.specie %>% dplyr::filter(id %in% pr.sample.train) %>% dplyr::select(longitude, latitude), 
                                b = bg.specie %>% dplyr::filter(id %in% bg.sample.train) %>% dplyr::select(longitude, latitude)) %>% na.omit
    test <- dismo::prepareData(x = var, 
                               p = pr.specie %>% dplyr::filter(!id %in% pr.sample.train) %>% dplyr::select(longitude, latitude), 
                               b = bg.specie %>% dplyr::filter(!id %in% bg.sample.train) %>% dplyr::select(longitude, latitude)) %>% na.omit
    
    
    # models fitting
    # information
    print(paste("Models fitting to", i, "replica", ifelse(r < 10, paste0("0", r), r)))
    
    # algorithms
    Bioclim <- dismo::bioclim(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    Gower <- dismo::domain(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    Mahalanobis <- dismo::mahal(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    GLM <- glm(formula = pb ~ ., family = "binomial", data = train)
    GAM <- gam::gam(formula = paste0("pb", "~", paste0("s(", colnames(train)[-1], ")", collapse = "+")) %>% as.formula, family = "binomial", data = train)
    RandomForest <- randomForest::randomForest(formula = pb ~ ., data = train)
    SVM <- kernlab::ksvm(x = pb ~ ., data = train)
    Maxent <- dismo::maxent(x = train %>% dplyr::select(-pb), p = train %>% dplyr::select(pb))
    
    # lists
    fit <- list(Bioclim, Gower, Mahalanobis, GLM, GAM, RandomForest, SVM, Maxent)
    alg <- list("bioclim", "gower", "mahalanobis", "glm", "gam", "randomforest", "svm", "maxent")
  
    
    # predict
    for(a in seq(fit)){
      
      # information
      print(paste("Model predict algorithm", alg[[a]]))
      
      # model predict
      model.predict <- dismo::predict(var, fit[[a]], progress = "text")
      
      # model export
      terra::writeRaster(model.predict, paste0(alg[[a]], "_", i, "_r", ifelse(r < 10, paste0("0", r), r), ".tif"), 
                         options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)
      
      # model evaluation
      eval <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                              a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                              model = fit[[a]])
      
      # tss
      id.eval <- which(eval@t == threshold(eval, "spec_sens"))
      tss <- eval@TPR[id.eval] + eval@TNR[id.eval] - 1
      
      # evaluation data
      eval.data <- tibble::tibble(species = i, 
                                  replica = ifelse(r < 10, paste0("0", r), r), 
                                  algorithm = alg[[a]], 
                                  thr_max_spec_sens = eval@t[id.eval], 
                                  auc = eval@auc, 
                                  tss = tss,
                                  file = paste0(alg[[a]], "_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"))
      
      # combine evaluation
      eval.algorithm <- dplyr::bind_rows(eval.algorithm, eval.data)
      
    } # ends for "a"
    
    # combine evaluation
    eval.replica <- dplyr::bind_rows(eval.replica, eval.algorithm)
    
  } # ends for "r"
  
  # combine evaluation
  eval.species <- dplyr::bind_rows(eval.replica, eval.species)
  
  # export evaluation
  readr::write_csv(eval.species, paste0("00_eval_", i, ".csv"))
  
} # ends for"i"

# end ---------------------------------------------------------------------