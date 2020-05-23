# script description #
# script:  enm - multiple algorithms
# package: dismo
# author:  mauricio vancine
# date:    06-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(beepr)
library(dismo)
library(earth)
library(gam)
library(e1071)
library(mda)
library(nnet)
library(randomForest)
library(raster)
library(rgdal)
library(rJava)
library(rnaturalearth)
library(sf)
library(terra)
library(tidyverse)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
raster::beginCluster(n = 4)

# directory
path <- "/home/mude/data/gitlab/r-enm/data"
setwd(path)
dir()

# occurrences -------------------------------------------------------------
occ <- readr::read_csv("/home/mude/data/gitlab/r-enm/data/01_occ/terretrial_animal_occ_spocc_raw_2019-04-23_filter_gnr_spatial_date.csv")
occ

# variables ---------------------------------------------------------------
# list files
tif <- dir(pattern = "tif$", recursive = TRUE) %>% 
  stringr::str_subset(pattern = "pca")
tif

# import rasters
var <- raster::stack(tif) %>% raster::brick()
var

# names
names(var)
names(var) <- stringr::str_replace(names(var), "wc20_ne_brazil_res05_", "")
names(var)
crs(var) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# plot
landscapetools::show_landscape(var$pc01) + 
  geom_point(data = occ, aes(x = longitude, y = latitude, color = species), 
             size = 2.5, alpha = .7)

# verify maxent -----------------------------------------------------------
# copy maxent.jar in "C:\Users\seu_nome\Documents\R\win-library\3.5.1\dismo\java"
file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))

# enms --------------------------------------------------------------------
# diretory
setwd(path)
dir.create("03_enms")
setwd("03_enms")

# parameters
re <- 3
pa <- .7
n_bg <- 500

# enms
for(i in occ$species %>% unique){} # for to each specie
  
  # directory
  dir.create(i)
  setwd(i)
  
  # information
  paste0("Preparing data for modeling ", i, " in ", getwd()) %>% print
  
  # object for evaluation
  eval_species <- tibble::tibble()
  
  # selecting presence and absence data
  pr_specie <- occ %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(longitude, latitude) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  bg_specie <- dismo::randomPoints(mask = var, n = n_bg) %>% 
    tibble::as_tibble() %>%
    dplyr::rename(longitude = x, latitude = y) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  # replicates
  for(r in seq(re)){}	# number of replicas
    
    # object for evaluation
    eval_algorithm <- tibble::tibble()
    
    # partitioning data	
    pr_sample_train <- pr_specie %>% 
      dplyr::sample_frac(pa) %>% 
      dplyr::select(id) %>% 
      dplyr::pull()
    
    bg_sample_train <- bg_specie %>% 
      dplyr::sample_frac(pa) %>% 
      dplyr::select(id) %>% 
      dplyr::pull()
    
    # train and test data
    train <- dismo::prepareData(x = var, 
                                p = pr_specie %>% dplyr::filter(id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                                b = bg_specie %>% dplyr::filter(id %in% bg_sample_train) %>% dplyr::select(longitude, latitude)) %>% na.omit
    
    test <- dismo::prepareData(x = var, 
                               p = pr_specie %>% dplyr::filter(!id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                               b = bg_specie %>% dplyr::filter(!id %in% bg_sample_train) %>% dplyr::select(longitude, latitude)) %>% na.omit
    
    
    ### model fitting ###
    # information
    print(paste("Models fitting to", i, "replica", r, "of", re))
    
    # algorithms
    # presence-only - envelope
    BIO <- dismo::bioclim(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # presence-only - distance-based
    DOM <- dismo::domain(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    MAH <- dismo::mahal(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # presence-absence - regression 
    GLM <- glm(formula = pb ~ ., family = "binomial", data = train)
    GAM <- gam::gam(formula = paste0("pb", "~", paste0("s(", colnames(train)[-1], ")", collapse = "+")) %>% as.formula, family = "binomial", data = train, warning = FALSE)
    MAR <- earth::earth(pb ~ ., glm = list(family = binomial), degree = 2, data = train)
    
    # presence-absence - machine learning
    ANN <- nnet::nnet(formula = pb ~ ., data = train, size = 2)
    tryCatch({
      BRT <- dismo::gbm.step(data = train, gbm.y = 1, gbm.x = 2:ncol(train), family = "bernoulli", silent = TRUE)
    }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
    CTA <- rpart::rpart(pb ~., data = train)
    RFR <- randomForest::randomForest(formula = pb ~ ., data = train)
    SVM <- e1071::svm(formula = pb ~ ., data = train)
    SVM
    SVM_one <- e1071::svm(formula = pb ~ ., data = train, type = "one-classification")
    SVM_one
    
    # presence-background
    MAX <- dismo::maxent(x = train %>% dplyr::select(-pb), p = train %>% dplyr::select(pb))
    
    # lists
    if(exists("BRT")){
      
      fit <- list(bioclim = BIO, domain = DOM, mahalanobis = MAH, glm = GLM, gam = GAM, mar = MAR, 
                  ann = ANN, brt = BRT, cta = CTA, randomforest = RFR, svm = SVM, svm_one = SVM_one, maxent = MAX)
      
    } else{
      fit <- list(bioclim = BIO, domain = DOM, mahalanobis = MAH, glm = GLM, gam = GAM, mar = MAR, 
                  ann = ANN, cta = CTA, randomforest = RFR, svm = SVM, svm_one = SVM_one, maxent = MAX)
      
    }
    
    # predict
    for(a in seq(fit)){
      
      # information
      print(paste("Model predict algorithm", fit[a] %>% names))
      
      # model predict
      if(fit[a] %>% names == "brt"){
        
        # brt model predict
        model_predict <- dismo::predict(var, fit[[a]], n.trees = BRT$gbm.call$best.trees, progress = "text")
        plot(model_predict)
        
        # model evaluation
        eval <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                                a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                                model = fit[[a]],
                                n.trees = BRT$gbm.call$best.trees)
        
      } else{
        # model predict
        model_predict <- dismo::predict(var, SVM, progress = "text", type = "response")
        plot(model_predict)
        
        # model evaluation
        eval <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                                a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                                model = fit[[a]])
        
      }
      
      # indices
      id_eval_kappa <- which(eval@t == dismo::threshold(eval, "kappa"))
      tss_kappa <- eval@TPR[id_eval_kappa] + eval@TNR[id_eval_kappa] - 1
      
      id_eval_spec_sens <- which(eval@t == dismo::threshold(eval, "spec_sens"))
      tss_spec_sens <- eval@TPR[id_eval_spec_sens] + eval@TNR[id_eval_spec_sens] - 1
      
      id_eval_equal_sens_spec <- which(eval@t == dismo::threshold(eval, "equal_sens_spec"))
      tss_equal_sens_spec <- eval@TPR[id_eval_equal_sens_spec] + eval@TNR[id_eval_equal_sens_spec] - 1
      
      id_eval_no_omission <- which(eval@t == dismo::threshold(eval, "no_omission"))
      tss_no_omission <- eval@TPR[id_eval_no_omission] + eval@TNR[id_eval_no_omission] - 1
      
      # evaluation data
      eval_data <- tibble::tibble(species = i, 
                                  replica = r, 
                                  algorithm = fit[a] %>% names, 
                                  thr_kappa = dismo::threshold(eval, "kappa"),
                                  thr_max_spec_sens = dismo::threshold(eval, "spec_sens"),
                                  thr_equal_sens_spec = dismo::threshold(eval, "equal_sens_spec"),
                                  thr_no_omission = dismo::threshold(eval, "no_omission"),
                                  prevalence = dismo::threshold(eval, "prevalence"),
                                  sensitivity = dismo::threshold(eval, "sensitivity"),
                                  tss_max_kappa = tss_kappa,
                                  tss_spec_sens = tss_spec_sens,
                                  tss_equal_sens_spec = tss_equal_sens_spec,
                                  tss_no_omission = tss_no_omission,
                                  auc = eval@auc, 
                                  file = paste0("enm_", i, "_", fit[a] %>% names, "_r", ifelse(r < 10, paste0("0", r), r), ".tif"))
      
      # combine evaluation
      eval_algorithm <- dplyr::bind_rows(eval_algorithm, eval_data)
      
      # model export
      terra::writeRaster(x = model_predict, 
                         filename = paste0("enm_", i, "_", fit[a] %>% names, "_r", ifelse(r < 10, paste0("0", r), r), ".tif"), 
                         options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)
      
    } # ends for "a"
    
    # combine evaluation
    eval_species <- dplyr::bind_rows(eval_species, eval_algorithm)
    
    # export evaluation
    readr::write_csv(eval_species, paste0("eval_", i, ".csv"))
    
    # notification sound
    beepr::beep(3)
    
  } # ends for "r"
  
  # directory
  setwd("..")
  
} # ends for"i"

# end ---------------------------------------------------------------------