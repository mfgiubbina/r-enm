#' ---
#' title: sdm - multiple method
#' authors: matheus lima-ribeiro, mauricio vancine
#' date: 2020-06-19
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

# java options
options(java.parameters = "-Xmx1g")

# packages
library(dismo)
library(e1071)
library(gam)
library(randomForest)
library(raster)
library(rJava)
library(sf)
library(tidyverse)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
raster::beginCluster(n = parallel::detectCores() - 1)

# maxent
if(file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))){
  print("File maxent.jar found!")
} else{
  print(paste0("File maxent.jar not found! Downloading in ", paste0(system.file(package = "dismo"), "/java")))
  setwd(paste0(system.file(package = "dismo"), "/java"))
  download.file("https://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download",
                "maxent.zip", mode = "wb")
  unzip("maxent.zip")}

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present/00_present_wc21"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ <- readr::read_csv("01_occurrences/03_clean/occ_clean_taxa_date_bias_limit_spatial.csv")
occ

# var
setwd(path); setwd("02_variables/04_processed_correlation"); dir()
var <- dir(pattern = "tif$") %>% 
  raster::stack() %>% 
  raster::brick()
names(var) <- stringr::str_replace(names(var), "var_wc21_55km_", "")
names(var)
var

raster::plot(var)
raster::plot(var[[1]])
points(occ$longitude, occ$latitude, pch = 20, col = as.factor(occ$species))

# enms --------------------------------------------------------------------
# directory
setwd(path); dir.create("03_enms"); setwd("03_enms")

# parameters
replica <- 5
partition <- .7
bkg_n <- 1e5

# enms
for(i in occ$species %>% unique){
  
  # directory
  dir.create(i); setwd(i)
  
  # information
  paste0("Preparing data for modeling ", i) %>% print
  
  # object for evaluation
  eval_species <- tibble::tibble()
  
  # selecting presence and pseudo-absence data
  pr_specie <- occ %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(longitude, latitude) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  pa_specie <- dismo::randomPoints(mask = var, n = nrow(pr_specie)) %>% 
    tibble::as_tibble() %>%
    dplyr::rename(longitude = x, latitude = y) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  bkg <- dismo::randomPoints(mask = var, n = bkg_n, warn = FALSE)
  
  # ------------------------------------------------------------------------
  
  # replicas
  for(r in replica %>% seq){
    
    # object for evaluation
    eval_method <- tibble::tibble()
    
    # partitioning data	
    pr_sample_train <- pr_specie %>% 
      dplyr::sample_frac(partition) %>% 
      dplyr::select(id) %>% 
      dplyr::pull()
    
    pa_sample_train <- pa_specie %>% 
      dplyr::sample_frac(partition) %>% 
      dplyr::select(id) %>% 
      dplyr::pull()
    
    # train and test data
    train_pa <- dismo::prepareData(x = var, 
                                   p = pr_specie %>% dplyr::filter(id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                                   b = pa_specie %>% dplyr::filter(id %in% pa_sample_train) %>% dplyr::select(longitude, latitude)) %>% na.omit
    
    train_pb <- dismo::prepareData(x = var, 
                                   p = pr_specie %>% dplyr::filter(id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                                   b = bkg) %>% na.omit
    
    test <- dismo::prepareData(x = var, 
                               p = pr_specie %>% dplyr::filter(!id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                               b = pa_specie %>% dplyr::filter(!id %in% pa_sample_train) %>% dplyr::select(longitude, latitude)) %>% na.omit
    
    # ------------------------------------------------------------------------
    
    ### model fitting ###
    # information
    print(paste("Models fitting to", i, "replica", r, "of", replica))
    
    # methods
    # presence-only - envelope
    BIO <- dismo::bioclim(x = train_pa %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # presence-only - distance-based
    DOM <- dismo::domain(x = train_pa %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    # MAH <- dismo::mahal(x = train_pa %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # presence-absence - statistics
    # GLM <- glm(formula = pb ~ ., data = train_pa, family = "binomial")
    # GAM <- gam::gam(formula = paste0("pb", "~", paste0("s(", colnames(train_pa)[-1], ")", collapse = "+")) %>% as.formula, 
    #                 family = "binomial", data = train_pa, warning = FALSE)
    
    # presence-absence - machine learning
    RFR <- randomForest::randomForest(formula = pb ~ ., data = train_pa)
    SVM <- e1071::svm(formula = pb ~ ., data = train_pa)
    
    # presence-background
    Sys.setenv(NOAWT = TRUE)
    MAX <- dismo::maxent(x = train_pb %>% dplyr::select(-pb), p = train_pb %>% dplyr::select(pb))
    
    # methods list
    fit <- list(bioclim = BIO, 
                domain = DOM, 
                # mahalanobis = MAH, 
                # glm = GLM, 
                # gam = GAM, 
                randomforest = RFR, 
                svm = SVM, 
                maxent = MAX)
    
    # ------------------------------------------------------------------------
    
    # predict
    for(a in seq(fit)){
      
      # information
      print(paste("Model predict method", fit[a] %>% names))
      
      # model predict
      model_predict <- raster::predict(var, fit[[a]], progress = "text")
      
      # model export
      raster::writeRaster(x = model_predict, 
                          filename = paste0("enm_", i, "_", fit[a] %>% names, "_r", ifelse(r < 10, paste0("0", r), r)), 
                          format = "GTiff", 
                          options = c("COMPRESS=DEFLATE"), 
                          progress = "text",
                          overwrite = TRUE)
      
      # ------------------------------------------------------------------------
      
      # model evaluation
      eval <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                              a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                              model = fit[[a]])
      
      # indices
      id_eval_spec_sens <- which(eval@t == dismo::threshold(eval, "spec_sens"))
      tss_spec_sens <- eval@TPR[id_eval_spec_sens] + eval@TNR[id_eval_spec_sens] - 1
      
      # evaluation data
      eval_data <- tibble::tibble(species = i, 
                                  replica = r, 
                                  method = fit[a] %>% names, 
                                  thr_max_spec_sens = dismo::threshold(eval, "spec_sens"),
                                  tss_spec_sens = tss_spec_sens,
                                  auc = eval@auc, 
                                  file = paste0("enm_", i, "_", fit[a] %>% names, "_r", ifelse(r < 10, paste0("0", r), r)))
      
      # combine evaluation
      eval_method <- dplyr::bind_rows(eval_method, eval_data)
      
    } # ends for "a"
    
    # combine evaluation
    eval_species <- dplyr::bind_rows(eval_species, eval_method)
    
  } # ends for "r"
  
  # export evaluations
  # directory
  setwd(path); dir.create("04_evaluations"); setwd("04_evaluations")
  dir.create(i); setwd(i)
  
  # export evaluations
  readr::write_csv(eval_species, paste0("00_eval_table_", i, ".csv"))
  
  # export presence and pseudo-absence points
  pr_specie %>% 
    dplyr::mutate(pa = 1) %>% 
    readr::write_csv(paste0("01_eval_table_pp_", i, ".csv"))
  pa_specie %>% 
    dplyr::mutate(pa = 0) %>%
    readr::write_csv(paste0("01_eval_table_pa_", i, ".csv"))
  
  # directory
  setwd(path); setwd("03_enms")
  
} # ends for "i"

# end ---------------------------------------------------------------------