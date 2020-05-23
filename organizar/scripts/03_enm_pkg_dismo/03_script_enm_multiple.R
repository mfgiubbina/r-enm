### script enm - dismo ###

## multiple algorithms ##

# mauricio vancine
# 15-03-2018

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
  theme_bw()

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
for(i in occ$species %>% unique){ # for to each specie

  # objects for evaluation
  eval.Bioclim <- tibble::tibble()
  eval.Gower <- tibble::tibble()
  eval.Mahalanobis <- tibble::tibble()
  eval.GLM <- tibble::tibble()
  eval.GAM <- tibble::tibble()
  eval.RandomForest <- tibble::tibble()
  eval.SVM <- tibble::tibble()
  eval.Maxent <- tibble::tibble()
  
  # selecting presence and absence data
  pr.specie <- occ %>% dplyr::filter(species == i) %>% 
    dplyr::select(longitude, latitude) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  bg.specie <- bg %>% dplyr::sample_n(nrow(pr.specie)) %>% 
    dplyr::select(longitude, latitude) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  # replicates
  for(r in seq(10)){	# number of replicas
    
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
    
    
    ### algorithms ###
    
    # 1. bioclim -----------------------------------------------------------------
    # information
    print(paste(i, "Bioclim", ifelse(r < 10, paste0("0", r), r)))
    
    # model fit
    Bioclim <- train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb) %>% 
      dismo::bioclim()
    
    # model predict
    dismo::predict(var, Bioclim, progress = "text") %>% 
      terra::writeRaster(paste0("bioclim_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"), 
                         options = c("COMPRESS=DEFLATE"), format = "GTiff")
    
    # model evaluation
    eBioclim <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                                a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                                model = Bioclim)
    idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens")))
    eval.Bioclim.sp <- tibble::tibble(species = i, 
                                      replica = ifelse(r < 10, paste0("0", r), r), 
                                      algorithm = "bioclim", 
                                      threshold = eBioclim@t[idBioclim], 
                                      auc = eBioclim@auc, 
                                      tss = eBioclim@TPR[idBioclim] + eBioclim@TNR[idBioclim] - 1,
                                      file = paste0("bioclim_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"))
    eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)
    
    
    # 2. gower ----------------------------------------------------------------
    # information
    print(paste(i, "Gower", ifelse(r < 10, paste0("0", r), r)))
    
    # model fit
    Gower <- train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb) %>% 
      dismo::domain()
    
    # model predict
      dismo::predict(var, Gower, progress = "text") %>% 
      terra::writeRaster(paste0("gower_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"), 
                         options = c("COMPRESS=DEFLATE"), format = "GTiff")
    
    # model evaluation
    eGower <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                              a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                              model = Gower)
    idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
    eval.Gower.sp <- tibble::tibble(species = i, 
                                    replica = ifelse(r < 10, paste0("0", r), r), 
                                    algorithm = "gower", 
                                    threshold = eGower@t[idGower], 
                                    auc = eGower@auc, 
                                    tss = eGower@TPR[idGower] + eGower@TNR[idGower] - 1,
                                    file = paste0("gower_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"))
    eval.Gower <- rbind(eval.Gower, eval.Gower.sp)
    
    
    # 3. mahalanobis ----------------------------------------------------------
    # information
    print(paste(i, "Mahalanobis", ifelse(r < 10, paste0("0", r), r)))
    
    # model fit
    Mahalanobis <- train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb) %>% 
      dismo::mahal()
    
    # model predict
    dismo::predict(var, Mahalanobis, progress = "text") %>% 
      terra::writeRaster(paste0("mahalanobis_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"), 
                         options = c("COMPRESS=DEFLATE"), format = "GTiff")
    
    # model evaluation
    eMahalanobis <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                                    a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                                    model = Mahalanobis)
    idMahalanobis <- which(eMahalanobis@t == as.numeric(threshold(eMahalanobis, "spec_sens")))
    eval.Mahalanobis.sp <- tibble::tibble(species = i, 
                                          replica = ifelse(r < 10, paste0("0", r), r), 
                                          algorithm = "mahalanobis", 
                                          threshold = eMahalanobis@t[idMahalanobis], 
                                          auc = eMahalanobis@auc, 
                                          tss = eMahalanobis@TPR[idMahalanobis] + eMahalanobis@TNR[idMahalanobis] - 1,
                                          file = paste0("mahalanobis_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"))
    eval.Mahalanobis <- rbind(eval.Mahalanobis, eval.Mahalanobis.sp)
    
    
    # 4. glm ------------------------------------------------------------------
    # information
    print(paste(i, "GLM", ifelse(r < 10, paste0("0", r), r)))
    
    # model fit
    GLM <- glm(pb ~ ., data = train)	
    
    # model predict
    dismo::predict(var, GLM, progress = "text") %>% 
      terra::writeRaster(paste0("glm_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"), 
                         options = c("COMPRESS=DEFLATE"), format = "GTiff")
    
    # model evaluation
    eGLM <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                            a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                            model = GLM)
    idGLM <- which(eGLM@t == as.numeric(threshold(eGLM, "spec_sens")))
    eval.GLM.sp <- tibble::tibble(species = i, 
                                  replica = ifelse(r < 10, paste0("0", r), r), 
                                  algorithm = "glm", 
                                  threshold = eGLM@t[idGLM],
                                  auc = eGLM@auc, 
                                  tss = eGLM@TPR[idGLM] + eGLM@TNR[idGLM] - 1,
                                  file = paste0("glm_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"))
    eval.GLM <- rbind(eval.GLM, eval.GLM.sp)
    
    
    # 5. gam ------------------------------------------------------------------
    # information
    print(paste(i, "GAM", ifelse(r < 10, paste0("0", r), r)))
    
    # model fit
    GAM <- gam::gam(paste0("pb", "~", paste0("s(", colnames(train)[-1], ")", collapse = "+")) %>% 
                      as.formula, family = "binomial", data = train)
    
    # model predict
    dismo::predict(var, GAM, progress = "text") %>% 
      terra::writeRaster(paste0("gam_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"), 
                         options = c("COMPRESS=DEFLATE"), format = "GTiff")
    
    # model evaluate
    eGAM <- evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                     a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                     model = GAM)
    idGAM <- which(eGAM@t == as.numeric(threshold(eGAM, "spec_sens")))
    eval.GAM.sp <- tibble::tibble(species = i, 
                                  replica = ifelse(r < 10, paste0("0", r), r), 
                                  algorithm = "gam", 
                                  threshold = eGAM@t[idGAM], 
                                  auc = eGAM@auc, 
                                  tss = eGAM@TPR[idGAM] + eGAM@TNR[idGAM] - 1,
                                   file = paste0("gam_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"))
    eval.GAM <- rbind(eval.GAM, eval.GAM.sp)
    
    
    # 6. random forest --------------------------------------------------------
    # information
    print(paste(i, "Random Forest", ifelse(r < 10, paste0("0", r), r)))
    
    # model fit
    RandomForest <- randomForest::randomForest(pb ~ ., data = train)
    
    # model predict
    ran <- dismo::predict(var, RandomForest, progress = "text") %>% 
      terra::writeRaster(paste0("randomforest_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"), 
                         options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    
    # model evaluation
    eRandomForest <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                                     a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb),
                                     model = RandomForest)
    idRandomForest <- which(eRandomForest@t == as.numeric(threshold(eRandomForest, "spec_sens")))
    eval.RandomForest.sp <- tibble::tibble(species = i, 
                                           replica = ifelse(r < 10, paste0("0", r), r), 
                                           algorithm = "randomforest", 
                                           threshold = eRandomForest@t[idRandomForest], 
                                           auc = eRandomForest@auc, 
                                           tss = eRandomForest@TPR[idRandomForest] + eRandomForest@TNR[idRandomForest] - 1,
                                           file = paste0("randomforest_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"))
    eval.RandomForest <- rbind(eval.RandomForest, eval.RandomForest.sp)
    
    
    # 7. svm	 -----------------------------------------------------------------
    # information
    print(paste(i, "SVM", ifelse(r < 10, paste0("0", r), r)))
    
    # model fit
    SVM <- kernlab::ksvm(pb ~ ., data = train)
    SVM
    
    # model predict
    dismo::predict(var, SVM, progress = "text") %>% 
      terra::writeRaster(paste0("svm_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"), 
                         options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    
    # model evaluation
    eSVM <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                            a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb),
                            model = SVM)
    idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
    eval.SVM.sp <- tibble::tibble(species = i, 
                                  replica = ifelse(r < 10, paste0("0", r), r), 
                                  algorithm = "svm", 
                                  threshold = eSVM@t[idSVM], 
                                  auc = eSVM@auc, 
                                  tss = eSVM@TPR[idSVM] + eSVM@TNR[idSVM] - 1,
                                  file = paste0("svm_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"))
    eval.SVM <- rbind(eval.SVM, eval.SVM.sp)
    
    
    # 8. maxent ---------------------------------------------------------------
    # information
    print(paste(i, "Maxent", ifelse(r < 10, paste0("0", r), r)))
    
    # model fit
    Maxent <- dismo::maxent(x = train %>% dplyr::select(-pb), 
                            p = train %>% dplyr::select(pb))
    
    # model predict
    dismo::predict(var, Maxent, progress = "text") %>% 
      terra::writeRaster(paste0("maxent_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"), 
                         options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    
    # model evaluation
    eMaxent <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                               a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb),
                               model = Maxent)
    idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
    eval.Maxent.sp <- tibble::tibble(species = i, 
                                     replica = ifelse(r < 10, paste0("0", r), r), 
                                     algorithm = "maxent", 
                                     threshold = eMaxent@t[idMaxent], 
                                     auc = eMaxent@auc, 
                                     tss = eMaxent@TPR[idMaxent] + eMaxent@TNR[idMaxent] - 1,
                                     file = paste0("maxent_", i, "_", ifelse(r < 10, paste0("0", r), r), ".tif"))
    eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)
    
  } # ends for "r"
  
  # evaluations -------------------------------------------------------------
  dplyr::bind_rows(eval.Bioclim, eval.Gower, eval.Mahalanobis, eval.GLM, eval.GAM, 
                   eval.RandomForest, eval.SVM, eval.Maxent) %>% 
    readr::write_csv(paste0("00_evaluation_", i, ".csv"))
  
} # ends for"i"

# end ---------------------------------------------------------------------