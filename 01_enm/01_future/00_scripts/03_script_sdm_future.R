#' ---
#' title: sdm - multiple algorithm
#' authors: matheus lima-ribeiro, mauricio vancine
#' date: 2020-05-05
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(dismo)
library(kernlab)
library(randomForest)
library(raster)
library(rgdal)
library(rnaturalearth)
library(sf)
library(tidyverse)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
raster::beginCluster(n = 2)

# java options
options(java.parameters = "-Xmx1g")

# directory
path <- "/home/mude/data/github/00_github_organizar/01_r-sdm/00_pragmatico/01_future"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occurrences
occ <- readr::read_csv("02_occurrences/03_clean/00_occ_clean_taxa_date_bias_limit_spatial_2020-04-29.csv")
occ

# variables
setwd(path); setwd("01_variables/04_processed_correlation"); dir()

# present
var_p <- dir(pattern = "tif$") %>%
  stringr::str_subset("pres") %>% 
  raster::stack() %>% 
  raster::brick()
names(var_p) <- stringr::str_replace(names(var_p), "wc14_55km_pres_", "")
names(var_p)
var_p

# future
var_f <- dir(pattern = "tif$") %>% 
  stringr::str_subset("fut") %>% 
  raster::stack() %>% 
  raster::brick()
names(var_f) <- stringr::str_replace(names(var_f), "wc14_55km_fut_", "")
names(var_f)
var_f

# maps
plot(var_p[[1]])
plot(var_f[[1]])

# enms --------------------------------------------------------------------
# diretory
setwd(path); dir.create("03_sdm"); setwd("03_sdm")

# parameters
replica <- 5
partition <- .7

# enms
for(i in occ$species %>% unique){ # for to each specie
  
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
  
  pa_specie <- dismo::randomPoints(mask = var_p, n = nrow(pr_specie)) %>% 
    tibble::as_tibble() %>%
    dplyr::rename(longitude = x, latitude = y) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  # export background points
  pr_specie %>% 
    dplyr::mutate(pa = 1) %>% 
    readr::write_csv(paste0("03_presence_points_", i, ".csv"))
  
  pa_specie %>% 
    dplyr::mutate(pa = 0) %>%
    readr::write_csv(paste0("03_absence_points_", i, ".csv"))
  
  # -------------------------------------------------------------------------
  
  # replicas
  for(r in replica %>% seq){	# number of replicas
    
    # object for evaluation
    eval_algorithm <- tibble::tibble()
    
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
    train <- dismo::prepareData(x = var_p, 
                                p = pr_specie %>% dplyr::filter(id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                                b = pa_specie %>% dplyr::filter(id %in% pa_sample_train) %>% dplyr::select(longitude, latitude)) %>% na.omit
    
    test <- dismo::prepareData(x = var_p, 
                               p = pr_specie %>% dplyr::filter(!id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                               b = pa_specie %>% dplyr::filter(!id %in% pa_sample_train) %>% dplyr::select(longitude, latitude)) %>% na.omit
    
    
    ### model fitting ###
    # information
    print(paste("Models fitting to", i, "replica", r, "of", replica))
    
    # algorithms
    # presence-only - envelope
    BIO <- dismo::bioclim(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # presence-only - distance-based
    DOM <- dismo::domain(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # presence-absence - statistics
    GLM <- glm(formula = pb ~ ., data = train, family = "binomial")
    
    # presence-absence - machine learning
    RFR <- randomForest::randomForest(formula = pb ~ ., data = train)
    
    # presence-background
    Sys.setenv(NOAWT = TRUE)
    MAX <- dismo::maxent(x = train %>% dplyr::select(-pb), p = train %>% dplyr::select(pb))
    SVM <- kernlab::ksvm(x = pb ~ ., data = train)
    
    # lists
    fit <- list(bioclim = BIO, domain = DOM, glm = GLM, randomforest = RFR, maxent = MAX, svm = SVM)
    
    # -------------------------------------------------------------------------

    # predict
    for(a in seq(fit)){
      
      # information
      print(paste("Model predict algorithm", fit[a] %>% names))
      
      # model predict present
      model_predict_p <- dismo::predict(var_p, fit[[a]], progress = "text")
      
      # model export present
      raster::writeRaster(x = model_predict_p, 
                          filename = paste0("sdm_", i, "_", fit[a] %>% names, "_r", ifelse(r < 10, paste0("0", r), r), "_pres"), 
                          format = "GTiff", 
                          options = c("COMPRESS=DEFLATE"), 
                          overwrite = TRUE)
      
      # ------------------------------------------------------------------------
      
      # model predict future
      for(f in stringr::str_sub(names(var_f), 1, 13) %>% unique){
        
        # select variables
        var_f_sel <- var_f[[grep(f, names(var_f), value = TRUE)]]
        
        # names
        names(var_f_sel) <- names(var_p)
        
        # model predict future
        model_predict_f <- dismo::predict(var_f_sel, fit[[a]], progress = "text")
        
        # model export present
        raster::writeRaster(x = model_predict_f, 
                            filename = paste0("sdm_", i, "_", fit[a] %>% names, "_r", ifelse(r < 10, paste0("0", r), r), "_fut_", f), 
                            format = "GTiff", 
                            options = c("COMPRESS=DEFLATE"), 
                            overwrite = TRUE)
        
      } # ends for "f"
      
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
                                  algorithm = fit[a] %>% names, 
                                  thr_max_spec_sens = dismo::threshold(eval, "spec_sens"),
                                  tss_spec_sens = tss_spec_sens,
                                  auc = eval@auc, 
                                  file = paste0("sdm_", i, "_", fit[a] %>% names, "_r", ifelse(r < 10, paste0("0", r), r)))
      
      # combine evaluation
      eval_algorithm <- dplyr::bind_rows(eval_algorithm, eval_data)
      
    } # ends for "a"
    
    # combine evaluation
    eval_species <- dplyr::bind_rows(eval_species, eval_algorithm)
    
  } # ends for "r"
  
  # export evaluation
  setwd(path); dir.create("04_evaluation"); setwd("04_evaluation")
  dir.create(i); setwd(i)
  readr::write_csv(eval_species, paste0("00_evaluation_", i, ".csv"))
  
  # directory
  setwd(path); setwd("03_sdm")
  
} # ends for "i"

# end ---------------------------------------------------------------------
