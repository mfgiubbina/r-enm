# -------------------------------------------------------------------------
# niche overlap ecospat
# mauricio vancine - mauricio.vancine@gmail.com
# 08-11-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(ecospat)
library(fasterize)
library(FactoMineR)
library(factoextra)
library(parallel)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)

# informations
# cmerow.github.io/RDataScience

# directory
path <- "/home/mude/data/onedrive/trabalho/ram/3_estatistica_modelagem_paisagem/131_marcones"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
setwd("02_data/01_occ/02_clean")
occ <- sf::st_read("occ.shp")
occ

# wc14
# directory
setwd(path)
setwd("02_data/02_var/02_wc14/01_mask/00_present")

# import
var <- dir(pattern = ".tif$") %>% 
  raster::stack() %>% 
  raster::brick()
var

# names
names(var)
names(var) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var)

# raster points
ra <- fasterize::raster(var[[1]])
ra

ra_occ_ca <- occ %>% 
  dplyr::filter(species == "copernicia_alba") %>% 
  dplyr::mutate(n = 1) %>% 
  sf::as_Spatial() %>% 
  raster::rasterize(., ra, field = 1, background = 0)
ra_occ_ca <- var[[1]] * ra_occ_ca
ra_occ_ca[ra_occ_ca > 0] <- 1
names(ra_occ_ca) <- "ca"
ra_occ_ca
plot(ra_occ_ca)

ra_occ_cp <- occ %>% 
  dplyr::filter(species == "copernicia_prunifera") %>% 
  dplyr::mutate(n = 1) %>% 
  sf::as_Spatial() %>% 
  raster::rasterize(., ra, field = 1, background = 0)
ra_occ_cp <- var[[1]] * ra_occ_cp
ra_occ_cp[ra_occ_cp > 0] <- 1
names(ra_occ_cp) <- "cp"
ra_occ_cp
plot(ra_occ_cp)

# stack
var_occ <- raster::stack(var, ra_occ_ca, ra_occ_cp)
var_occ

# extract values
var_da_env <- var_occ %>% 
  raster::values() %>% 
  tibble::as_tibble() %>%
  tidyr::drop_na()
var_da_env

# niche overlap -----------------------------------------------------------
# directory
setwd(path)
setwd("03_nicheoverlap")

# PCA-ENVIRONMENT
# The PCA is calibrated on all the sites of the study area
pca_env <- ade4::dudi.pca(var_da_env[, 1:19], scannf = FALSE , nf = 2, center = TRUE)
pca_env

# Plot Variables Contribution with ecospat.plot.contrib()
png("pca_env.png", wi = 20, he = 20, un = "cm", res = 300)
ecospat::ecospat.plot.contrib(contrib = pca_env$co, eigen = pca_env$eig)
dev.off()

# Predict the scores on the axes
# PCA scores for the whole study area
scores_globclim <- pca_env$li
scores_globclim

# PCA scores for the species native distribution
scores_sp_ca <- ade4::suprow(pca_env, var_da_env[which(var_da_env[, 20] == 1), 1:19])$li
scores_sp_ca

# PCA scores for the species invasive distribution
scores_sp_cp <- ade4::suprow(pca_env, var_da_env[which(var_da_env[, 21] == 1), 1:19])$li
scores_sp_cp

# Calculate the Occurrence Densities Grid with ecospat.grid.clim.dyn()
# gridding the ca niche
grid_clim_ca <- ecospat::ecospat.grid.clim.dyn(glob = scores_globclim,
                                               glob1 = scores_globclim,
                                               sp = scores_sp_ca, 
                                               R = 100,
                                               th.sp = 0)
grid_clim_ca
plot(grid_clim_ca$w)

# gridding the invasive niche
grid_clim_cp <- ecospat::ecospat.grid.clim.dyn(glob = scores_globclim,
                                               glob1 = scores_globclim,
                                               sp = scores_sp_cp, 
                                               R = 100,
                                               th.sp = 0)
grid_clim_cp
plot(grid_clim_cp$w)
plot(grid_clim_cp$z.cor)

# Calculate Niche Overlap with ecospat.niche.overlap()
# Compute Schoener's D and Hellinger’s-based I, index of niche overlap
DI_overlap <- ecospat::ecospat.niche.overlap(grid_clim_ca, grid_clim_cp, cor = TRUE)
DI_overlap

# Perform the Niche Similarity Test with ecospat.niche.similarity.test()
sim_test <- ecospat::ecospat.niche.similarity.test(z1 = grid_clim_ca, 
                                                   z2 = grid_clim_cp,
                                                   rep = 1e3,
                                                   rand.type = 2)
sim_test

sim_test_da <- tibble::tibble(d = round(sim_test$obs$D, 2),
                              p_d = round(sim_test$p.D, 2),
                              i = round(sim_test$obs$I, 2),
                              i_d = round(sim_test$p.I, 2))
sim_test_da

readr::write_csv(sim_test_da, "niche_similarity_test.csv")

# Plot tests  
png("d_hist.png", he = 20, wi = 20, un = "cm", res = 300)
ecospat::ecospat.plot.overlap.test(sim_test, "D", "Similarity")
dev.off()

png("i_hist.png", he = 20, wi = 20, un = "cm", res = 300)
ecospat::ecospat.plot.overlap.test(sim_test, "I", "Similarity")
dev.off()

# Delimiting niche categories and quantifying niche dynamics in analogue climates with ecospat.niche.dyn.index()
niche_dyn <- ecospat::ecospat.niche.dyn.index(grid_clim_ca, grid_clim_ca, intersection = .1)
niche_dyn

png("niche_overlap.png", he = 20, wi = 20, un = "cm", res = 300)
ecospat::ecospat.plot.niche.dyn(z1 = grid_clim_ca, 
                                z2 = grid_clim_cp, 
                                quant = .25, 
                                interest = 2,
                                title = "Niche Overlap", 
                                name.axis1 = "PC1",
                                name.axis2 = "PC2")
dev.off()

# end ---------------------------------------------------------------------