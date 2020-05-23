# -------------------------------------------------------------------------
# sdm - multiple algorithm
# matheus lima-ribeiro - mslima.ribeiro@gmail.com 
# mauricio vancine - mauricio.vancine@gmail.com
# 28-10-2019
# -------------------------------------------------------------------------

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
raster::beginCluster(n = 8)

# information
# https://cran.r-project.org/web/packages/dismo/index.html
# https://biodiversityinformatics.amnh.org/open_source/maxent/
# https://rspatial.org/sdm/
# https://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf

# directory
path <- "/home/mude/data/gitlab/r-sdm/01_data"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ <- readr::read_csv("00_occ/hb_clean.csv")
occ

# var
setwd(path); setwd("01_var/01_mask"); dir()
var_p <- dir(pattern = "tif$") %>% 
  raster::stack() %>% 
  raster::brick()
names(var_p) <- stringr::str_replace(names(var_p), "wc14_5km_", "")
var_p

setwd(path); setwd("01_var/01_mask/02_future"); dir()
var_f <- dir(pattern = "tif$", recursive = TRUE) %>% 
  raster::stack() %>% 
  raster::brick()
names(var_f) <- stringr::str_replace(names(var_f), "wc14_5km_", "")
var_f

plot(var_p[[1]])
plot(var_f[[1]])

# enms --------------------------------------------------------------------
# diretory
setwd("/home/mude/data/gitlab/r-sdm"); dir.create("03_output"); setwd("03_output")

# parameters
replica <- 5
partition <- .7

# enms
for(i in occ$species %>% unique){} # for to each specie
  
  # information
  paste0("Preparing data for modeling ", i, " in ", getwd()) %>% print
    
  # directory
  dir.create(i)
  setwd(i)

    # selecting presence and absence data
  pr_i <- occ %>% 
    dplyr::filter(species == i) %>% 
    dplyr::select(longitude, latitude) %>% 
    dplyr::mutate(id = seq(nrow(.)))
  
  pa_i <- dismo::randomPoints(mask = var_p, n = nrow(pr_i)) %>% 
    tibble::as_tibble() %>%
    dplyr::rename(longitude = x, latitude = y) %>% 
    dplyr::mutate(id = seq(nrow(.)))  
  
  # export coordinates
  raster::xyFromCell(var_p, seq(ncell(var_p))) %>% 
    tibble::as_tibble() %>% 
    readr::write_csv("00_coordinates.csv")
  
  # export presences and absences
  readr::write_csv(pr_i, "01_presences.csv")
  readr::write_csv(pa_i, "01_absences.csv")
  
  # variables
  sdm_i <- tibble::tibble()
  eval_i <- tibble::tibble()
  
  # replicates
  for(r in replica %>% seq){}	# number of replicas
    
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
    # DOM <- dismo::domain(x = train %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))
    
    # presence-absence - machine learning
    RFR <- randomForest::randomForest(formula = pb ~ ., data = train)
    
    # presence-background
    MAX <- dismo::maxent(x = train %>% dplyr::select(-pb), p = train %>% dplyr::select(pb))
    SVM <- kernlab::ksvm(x = pb ~ ., data = train)
    
    # lists
    fit <- list(bioclim = BIO, randomforest = RFR, maxent = MAX, svm = SVM)
    
    # predict
    for(a in seq(fit)){}
      
      # information
      print(paste("Model predict algorithm", fit[a] %>% names))
      
      # model predict present
      model_predict <- dismo::predict(var_p, fit[[a]], progress = "text")
      
      # model export present
      raster::writeRaster(x = model_predict, 
                          filename = paste0("sdm_", i, "_", fit[a] %>% names, "_r", ifelse(r < 10, paste0("0", r), r), "_present"), 
                          format = "GTiff", 
                          options = c("COMPRESS=DEFLATE"), 
                          overwrite = TRUE)
      
      for(f in stringr::str_sub(names(var_f), 1, 8) %>% unique){
        
        # select var
        var_f_sel <- var_f[[grep(f, names(var_f), value = TRUE)]]
        
        # names
        names(var_f_sel) <- names(var_p)
        
        # model predict future
        model_predict_future <- dismo::predict(var_f_sel, fit[[a]], progress = "text")
        
        # model export present
        raster::writeRaster(x = model_predict_future, 
                            filename = paste0("sdm_", i, "_", fit[a] %>% names, "_r", ifelse(r < 10, paste0("0", r), r), "_", f), 
                            format = "GTiff", 
                            options = c("COMPRESS=DEFLATE"), 
                            overwrite = TRUE)
        
      } # ends for "f"
      
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
                                  file = paste0("sdm_", i, "_", fit[a] %>% names, "_r", ifelse(r < 10, paste0("0", r), r), ".tif"))
      
      # combine evaluation
      eval_algorithm <- dplyr::bind_rows(eval_algorithm, eval_data)
      
    } # ends for "a"
    
    # combine evaluation
    eval_species <- dplyr::bind_rows(eval_species, eval_algorithm)
    
  } # ends for "r"
  
  # export evaluation
  setwd("..")
  
  dir.create("01_evaluation")
  setwd("01_evaluation")
  dir.create("00_raw")
  setwd("00_raw")
  
  readr::write_csv(eval_species, paste0("eval_", i, ".csv"))
  
  # directory
  setwd(".."); setwd(".."); setwd("..") 
  
} # ends for "i"

# end ---------------------------------------------------------------------
