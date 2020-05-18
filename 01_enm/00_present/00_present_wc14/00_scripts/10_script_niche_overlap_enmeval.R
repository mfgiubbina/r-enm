#' ---
#' title: niche overlap ecospat
#' author: mauricio vancine
#' date: 2020-05-11
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(ecospat)
library(tidyverse)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present/00_present_wc14"
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
names(var) <- stringr::str_replace(names(var), "wc14_55km_", "")
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

# ecospat identity test ---------------------------------------------------
# test
ecospat_identity_test <- ENMTools::enmtools.ecospat.id(
  species.1  = sl, 
  species.2 = sl, 
  env = var,
  nreps = 99,
  R = 100)
ecospat_identity_test

# results
ecospat_identity_test$test.results$obs$D
ecospat_identity_test$p.values["D"]
mean(ecospat_identity_test$test.results$sim$D)
sd(ecospat_identity_test$test.results$sim$D)

ecospat_identity_test$test.results$obs$I
ecospat_identity_test$p.values["I"]
mean(ecospat_identity_test$test.results$sim$I)
sd(ecospat_identity_test$test.results$sim$I)

# graphics
ecospat_identity_test
ecospat_identity_test$d.plot + geom_histogram(color = "gray30", fill = "gray70", alpha = 1) + theme_bw()
ecospat_identity_test$i.plot + geom_histogram(color = "gray30", fill = "gray70", alpha = 1) + theme_bw()

# ecospat similarity test -------------------------------------------------
# test
ecospat_similarity_test <- ENMTools::enmtools.ecospat.bg(
  species.1  = cb, 
  species.2 = sl, 
  env = var,
  test.type = "symmetric",
  nreps = 99,
  R = 100,
  nback = 1e3)
ecospat_similarity_test

# results
ecospat_similarity_test$test.results$obs$D
ecospat_similarity_test$p.values["D"]
mean(ecospat_similarity_test$test.results$sim$D)
sd(ecospat_similarity_test$test.results$sim$D)

ecospat_similarity_test$test.results$obs$I
ecospat_similarity_test$p.values["I"]
mean(ecospat_similarity_test$test.results$sim$I)
sd(ecospat_similarity_test$test.results$sim$I)

# graphics
ecospat_similarity_test
ecospat_similarity_test$d.plot + geom_histogram(color = "gray30", fill = "gray70", alpha = 1) + theme_bw()
ecospat_similarity_test$i.plot + geom_histogram(color = "gray30", fill = "gray70", alpha = 1) + theme_bw()

# end ---------------------------------------------------------------------