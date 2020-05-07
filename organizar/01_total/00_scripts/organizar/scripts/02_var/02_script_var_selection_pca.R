# script description #
# script: variables - pca
# author: mauricio vancine
# date:   06-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(factoextra)
library(FactoMineR)
library(landscapetools)
library(raster)
library(rgdal)
library(RStoolbox)
library(tidyverse)

# raster options
raster::rasterOptions()
raster::rasterOptions(maxmemory = 1e+50)
raster::rasterOptions(chunksize = 1e+50)
raster::rasterOptions()

# directory
setwd("/media/mude/data/gitlab/r-enm/data/02_var/bioclim_v20")
dir()

# import variables e get data ---------------------------------------------
# list variables
tif <- dir(pattern = "tif")
tif

# import rasters
var <- raster::stack(tif)
var

# extract values
var_da <- var %>% 
  raster::values() %>% 
  tibble::as_tibble() %>% 
  tidyr::drop_na()
var_da

# column names
colnames(var_da)
colnames(var_da) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
colnames(var_da)
var_da

# verify
dplyr::glimpse(var_da)

# pca ---------------------------------------------------------------------
# directory
dir.create("pca") 
setwd("pca") 

# pca
pca <- FactoMineR::PCA(var_da, scale.unit = TRUE, graph = FALSE)
pca

# eigenvalues
eig <- factoextra::get_eig(pca) %>% 
  tibble::as_tibble() %>% 
  round(2) %>% 
  dplyr::mutate(id = rownames(factoextra::get_eig(pca))) %>% 
  dplyr::select(id, everything())
eig

readr::write_csv(eig, "pca_eigenvalues.csv")

# eigenvalues plot
factoextra::fviz_eig(pca, addlabels = TRUE, ggtheme = theme_classic())
ggsave("pca_screeplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300, comp = "lzw")

# contributions
loa <- pca$var$contrib %>% round(2)
loa

write.csv(loa, "pca_contributions.csv")

# biplot
factoextra::fviz_pca(pca, geom = "point", alpha.ind = .5, repel = TRUE, ggtheme = theme_bw())
ggsave("pca_biplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300, comp = "lzw")

# raster pca --------------------------------------------------------------
# pca
pca_raster <- RStoolbox::rasterPCA(var, spca = TRUE) 
pca_raster

# plot pca map
landscapetools::show_landscape(pca_ra$PC1)

# export ------------------------------------------------------------------
raster::writeRaster(x = pca.ra$map[[1:6]], 
                    filename = paste0("wc20_masknebrazil_res05g_pc0", 1:6), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff", 
                    overwrite = TRUE)

# end ---------------------------------------------------------------------