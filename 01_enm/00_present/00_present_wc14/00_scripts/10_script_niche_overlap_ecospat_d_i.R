#' ---
#' title: niche overlap ecospat
#' author: mauricio vancine
#' date: 2020-06-16
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(ecospat)
library(parallel)
library(tidyverse)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present/00_present_wc14"
setwd(path)
dir()

# import data -------------------------------------------------------------
# occ
occ <- readr::read_csv("01_occurrences/03_clean/occ_clean_taxa_date_bias_limit_spatial.csv")
occ

# cb
occ_sp1 <- occ %>% 
  dplyr::filter(species == "chrysocyon_brachyurus") %>% 
  dplyr::select(longitude, latitude) %>% 
  as.data.frame()
occ_sp1

# sl
occ_sp2 <- occ %>% 
  dplyr::filter(species == "solanum_lycocarpum") %>% 
  dplyr::select(longitude, latitude) %>% 
  as.data.frame()
occ_sp2

# var
setwd(path); setwd("02_variables/04_processed_correlation"); dir()
var <- dir(pattern = "tif$") %>% 
  raster::stack() %>% 
  raster::brick()
names(var) <- stringr::str_replace(names(var), "var_wc14_55km_", "")
names(var)
var

# map
raster::plot(var)
raster::plot(var$bio02)
points(occ_sp1$longitude, occ_sp1$latitude, pch = 20, col = "darkorange")
points(occ_sp2$longitude, occ_sp2$latitude, pch = 20, col = "forestgreen")

# extract values ----------------------------------------------------------
# var cb
var_sp1 <- raster::extract(var, occ_sp1)
var_sp1

# var sl
var_sp2 <- raster::extract(var, occ_sp2)
var_sp2

# var background
var_bkg <- na.exclude(raster::values(var))
var_bkg

# pca environment ---------------------------------------------------------
# pca
pca_env <- ade4::dudi.pca(var_bkg, center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
pca_env

# plot
ecospat::ecospat.plot.contrib(contrib = pca_env$co, eigen = pca_env$eig)

# predict the scores on the axes
scores_sp1 <- ade4::suprow(pca_env, var_sp1)$lisup # scores for cb
scores_sp1

scores_sp2 <- ade4::suprow(pca_env, var_sp2)$lisup # scores for sl
scores_sp2

scores_bkg <- pca_env$li # scores for background climate
scores_bkg

# calculation of occurence density ----------------------------------------
# niche z to cb
z1 <- ecospat::ecospat.grid.clim.dyn(scores_bkg, scores_bkg, scores_sp1, R = 100, th.sp = 0, th.env = 0)
z1

plot(z1$z.uncor)
points(scores_sp1, pch = 20, col = "darkorange")

# niche z to sl
z2 <- ecospat::ecospat.grid.clim.dyn(scores_bkg, scores_bkg, scores_sp2, R = 100, th.sp = 0, th.env = 0)
z2

plot(z2$z.uncor)
points(scores_sp2, pch = 20, col = "forestgreen")

# niche overlap -----------------------------------------------------------
# niche overlap
no <- ecospat::ecospat.niche.overlap(z1, z2, cor = TRUE)
no

# plot
ecospat::ecospat.plot.niche.dyn(z1, z2, quant = .25, interest = 1, 
                                colz1 = adjustcolor("darkorange", .7), colz2 = adjustcolor("forestgreen", .7), 
                                colinter = adjustcolor("darkorchid", .3), colZ1 = "gray", colZ2 = "gray",
                                title = "Niche Overlap", name.axis1 = "PC1", name.axis2 = "PC2")

# equivalency test --------------------------------------------------------
# niche equivalency test
eq <- ecospat::ecospat.niche.equivalency.test(z1, z2, rep = 10, alternative = "lower", 
                                              ncores = parallel::detectCores() - 1)
eq

# values
mean(eq$sim$D); sd(eq$sim$D); eq$p.D
mean(eq$sim$I); sd(eq$sim$I); eq$p.I

# plot
ecospat::ecospat.plot.overlap.test(eq, type = "D", title = "Equivalency")
ecospat::ecospat.plot.overlap.test(eq, type = "I", title = "Equivalency")

# similarity test ----------------------------------------------------------
# niche similarity test
si_sp1_sp2 <- ecospat::ecospat.niche.similarity.test(z1, z2, rep = 10, alternative = "greater",
                                                     rand.type = 1, ncores = parallel::detectCores() - 1)
si_sp1_sp2

si_sp2_sp1 <- ecospat::ecospat.niche.similarity.test(z2, z1, rep = 10, alternative = "greater",
                                                     rand.type = 1, ncores = parallel::detectCores() - 1)
si_sp2_sp1

# values
mean(si_sp1_sp2$sim$D); sd(si_sp1_sp2$sim$D); si_sp1_sp2$p.D
mean(si_sp1_sp2$sim$I); sd(si_sp1_sp2$sim$I); si_sp1_sp2$p.I
mean(si_sp2_sp1$sim$D); sd(si_sp2_sp1$sim$D); si_sp2_sp1$p.D
mean(si_sp2_sp1$sim$I); sd(si_sp2_sp1$sim$I); si_sp2_sp1$p.I

# plot
ecospat::ecospat.plot.overlap.test(si_sp1_sp2, type = "D", title = "Similarity sp2->sp1")
ecospat::ecospat.plot.overlap.test(si_sp1_sp2, type = "I", title = "Similarity sp2->sp1")

ecospat::ecospat.plot.overlap.test(si_sp2_sp1, type = "D", title = "Similarity sp1->sp2")
ecospat::ecospat.plot.overlap.test(si_sp2_sp1, type = "I", title = "Similarity sp1->sp2")

# figure ------------------------------------------------------------------
layout(matrix(c(1,1,2,2, 1,1,2,2, 3,3,5,6, 4,4,7,8), 4, 4, byrow = TRUE))
layout.show(8)
ecospat::ecospat.plot.contrib(contrib = pca_env$co, eigen = pca_env$eig)
ecospat::ecospat.plot.niche.dyn(z1, z2, quant = .25, interest = 1, 
                                colz1 = adjustcolor("darkorange", .7), colz2 = adjustcolor("forestgreen", .7), 
                                colinter = adjustcolor("darkorchid", .3), colZ1 = "gray", colZ2 = "gray",
                                title = "Niche Overlap", name.axis1 = "PC1", name.axis2 = "PC2")
ecospat::ecospat.plot.overlap.test(eq, type = "D", title = "Equivalency")
ecospat::ecospat.plot.overlap.test(eq, type = "I", title = "Equivalency")
ecospat::ecospat.plot.overlap.test(si_sp1_sp2, type = "D", title = "Similarity (p2 -> sp1)")
ecospat::ecospat.plot.overlap.test(si_sp2_sp1, type = "D", title = "Similarity (sp1 -> sp2)")
ecospat::ecospat.plot.overlap.test(si_sp1_sp2, type = "I", title = "Similarity (sp2 -> sp1)")
ecospat::ecospat.plot.overlap.test(si_sp2_sp1, type = "I", title = "Similarity (sp1 -> sp2)")
def.par <- par(no.readonly = TRUE)
par(def.par)  #- reset to default

# export ------------------------------------------------------------------
# directory
setwd(path); dir.create("10_niche_overlap"); setwd("10_niche_overlap")

# table
tibble::tibble(
  d_desc = c("d_obs", 
             "d_eq_test_mean", "d_eq_test_sd", "d_eq_p",
             "d_si_sp1_sp2_test_mean", "d_si_sp1_sp2_test_sd", "d_si_sp1_sp2_p",
             "d_si_sp2_sp1_test_mean", "d_si_sp2_sp1_test_sd", "d_si_sp2_sp1_p"),
  d = c(no$D, 
        mean(eq$sim$D), sd(eq$sim$D), eq$p.D,
        mean(si_sp1_sp2$sim$D), sd(si_sp1_sp2$sim$D), si_sp1_sp2$p.D,
        mean(si_sp2_sp1$sim$D), sd(si_sp2_sp1$sim$D), si_sp2_sp1$p.D),
  i_desc = c("i_obs", 
             "i_eq_test_mean", "i_eq_test_sd", "i_eq_p",
             "i_si_sp1_sp2_test_mean", "i_se_sp1_sp2_test_sd", "i_se_sp1_sp2_p",
             "i_se_sp2_sp1_test_mean", "i_se_sp2_sp1_test_sd", "i_sp2_sp1_se_p"),
  i = c(no$I, 
        mean(eq$sim$I), sd(eq$sim$I), eq$p.I, 
        mean(si_sp1_sp2$sim$I), sd(si_sp1_sp2$sim$I), si_sp1_sp2$p.I,
        mean(si_sp2_sp1$sim$I), sd(si_sp2_sp1$sim$I), si_sp2_sp1$p.I)
) %>% 
  dplyr::mutate(d = round(d, 2), i = round(i, 2)) %>% 
  readr::write_csv("overlap_test.csv")

# figure
png("overlap_niche.png", wi = 25, he = 20, un = "cm", res = 300)
layout(matrix(c(1,1,2,2, 1,1,2,2, 3,3,5,6, 4,4,7,8), 4, 4, byrow = TRUE))
layout.show(8)
ecospat::ecospat.plot.contrib(contrib = pca_env$co, eigen = pca_env$eig)
ecospat::ecospat.plot.niche.dyn(z1, z2, quant = .25, interest = 1, 
                                colz1 = adjustcolor("darkorange", .7), colz2 = adjustcolor("forestgreen", .7), 
                                colinter = adjustcolor("darkorchid", .3), colZ1 = "gray", colZ2 = "gray",
                                title = "Niche Overlap", name.axis1 = "PC1", name.axis2 = "PC2")
ecospat::ecospat.plot.overlap.test(eq, type = "D", title = "Equivalency")
ecospat::ecospat.plot.overlap.test(eq, type = "I", title = "Equivalency")
ecospat::ecospat.plot.overlap.test(si_sp1_sp2, type = "D", title = "Similarity (sp2 -> sp1)")
ecospat::ecospat.plot.overlap.test(si_sp2_sp1, type = "D", title = "Similarity (sp1 -> sp2)")
ecospat::ecospat.plot.overlap.test(si_sp1_sp2, type = "I", title = "Similarity (sp2 -> sp1)")
ecospat::ecospat.plot.overlap.test(si_sp2_sp1, type = "I", title = "Similarity (sp1 -> sp2)")
dev.off()

# draft -------------------------------------------------------------------
# ecospat::ecospat.plot.overlap.test(eq, type = "D", title = paste0("Equivalency (D=", round(eq$obs$D , 3), ")"))
# ecospat::ecospat.plot.overlap.test(eq, type = "I", title = paste0("Equivalency (I=", round(eq$obs$I , 3), ")"))
# ecospat::ecospat.plot.overlap.test(si_sp1_sp2, type = "D", title = paste0("Similarity sl->cb (D=", round(si_sp1_sp2$obs$D, 3), ")"))
# ecospat::ecospat.plot.overlap.test(si_sp2_sp1, type = "D", title = paste0("Similarity cb->sl (D=", round(si_sp2_sp1$obs$D, 3), ")"))
# ecospat::ecospat.plot.overlap.test(si_sp1_sp2, type = "I", title = paste0("Similarity sl->cb (I=", round(si_sp1_sp2$obs$I, 3), ")"))
# ecospat::ecospat.plot.overlap.test(si_sp2_sp1, type = "I", title = paste0("Similarity cb->sl (I=", round(si_sp2_sp1$obs$I, 3), ")"))

# end ---------------------------------------------------------------------

