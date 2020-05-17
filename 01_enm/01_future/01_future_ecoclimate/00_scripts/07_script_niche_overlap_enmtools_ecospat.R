#' ---
#' title: niche overlap ecospat
#' author: mauricio vancine
#' date: 2020-05-08
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(ENMTools)
library(tidyverse)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ <- readr::read_csv("02_occurrences/03_clean/occ_clean_taxa_date_bias_limit_spatial.csv")
occ

occ_cb <- occ %>% 
  dplyr::filter(species == "chrysocyon_brachyurus") %>% 
  dplyr::select(longitude, latitude) %>% 
  as.data.frame()
occ_cb

occ_sl <- occ %>% 
  dplyr::filter(species == "solanum_lycocarpum") %>% 
  dplyr::select(longitude, latitude) %>% 
  as.data.frame()
occ_sl

# var
setwd(path); setwd("01_variables/04_processed_correlation"); dir()
var <- dir(pattern = "tif$") %>% 
  raster::stack() %>% 
  raster::brick()
names(var) <- stringr::str_replace(names(var_p), "wc14_55km_", "")
names(var)
var

raster::plot(var)

# creating enmtools.species objects ---------------------------------------
cb <- ENMTools::enmtools.species(
  species.name = "Chrysocyon brachyurus", 
  presence.points = occ_cb)
cb

sl <- ENMTools::enmtools.species(
  species.name = "Solanum lycocarpum", 
  presence.points = occ_sl)
sl

# ecospat tests -----------------------------------------------------------
# ecospat identity test
ecospat_identity_test <- ENMTools::enmtools.ecospat.id(species.1  = cb, 
                                            species.2 = sl, 
                                            env = var,
                                            nreps = 99,
                                            R = 100)
ecospat_identity_test

# results
ecospat_identity_test$test.results$obs$D
mean(ecospat_identity_test$test.results$sim$D)
sd(ecospat_identity_test$test.results$sim$D)
ecospat_identity_test$test.results$obs$I
mean(ecospat_identity_test$test.results$sim$I)
sd(ecospat_identity_test$test.results$sim$I)


# end ---------------------------------------------------------------------