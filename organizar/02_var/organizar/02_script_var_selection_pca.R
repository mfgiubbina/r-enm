### script enm - dismo ###

## variables selection - pca ##

# mauricio vancine
# 30-03-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(factoextra)
library(FactoMineR)
library(raster)
library(RStoolbox)
library(tidyverse)
library(viridis)


# w14 ---------------------------------------------------------------------
# directiry
setwd("/media/mauricio/data/006_orientacoes/lucas/03_data/var/01_bioclim_v14")
dir()

# import variables e get data
var.wc14 <- raster::stack(dir(pattern = "sa"))
var.wc14

# names
names(var.wc14) <- names(var.wc14) %>% 
  sub("wc14_10m_", "", .) %>% 
  sub("_sa", "", .)
names(var.wc14)

# extract values
var.wc14.da <- var.wc14 %>% 
  raster::values() %>% 
  na.omit
var.wc14.da

# pca
# directory
dir.create("02_pca") 
setwd("02_pca") 

# pca
pca.wc14 <- FactoMineR::PCA(var.wc14.da, scale.unit = TRUE, graph = FALSE)
pca.wc14

# eigenvalues
eig.wc14 <- factoextra::get_eig(pca.wc14) %>% 
  tibble::as_tibble() %>% 
  round(2) %>% 
  dplyr::mutate(id = rownames(factoextra::get_eig(pca))) %>% 
  dplyr::select(id, everything())
eig.wc14

readr::write_csv(eig.wc14, "eigenvalues.csv")

# eigenvalues plot
factoextra::fviz_eig(pca.wc14, addlabels = TRUE, ggtheme = theme_classic())
ggsave("00_screeplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

# contributions
con.wc14 <- pca.wc14$var$contrib %>% 
  round(2)
con.wc14

write.csv(con.wc14, "contributions.csv")

# contributions
for(i in seq(5)){
  print(i)
  factoextra::fviz_contrib(pca.wc14, choice = "var", axes = i, ggtheme = theme_classic())
  ggsave(paste0("contributions_pc", i, ".tiff"), he = 15, wi = 20, un = "cm", dpi = 300)
}

# biplot
factoextra::fviz_pca(pca.wc14, geom = "point", col.ind = "black", alpha.ind = .05, repel = TRUE)
ggsave("03_biplot_pca.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

# raster pca
## pca of raster
pca.wc14.ra <- RStoolbox::rasterPCA(var.wc14, spca = TRUE) 
pca.wc14.ra

# plot pcas variables
ggplot() +
  geom_raster(data = raster::rasterToPoints(pca.wc14.ra$map[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = PC1)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  coord_equal() +
  theme_bw()

# export
raster::writeRaster(x = pca.wc14.ra$map[[1:6]], filename = paste0("pc0", 1:6, "_wc14"), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

# w14 ---------------------------------------------------------------------
# directiry
setwd("/media/mauricio/data/006_orientacoes/lucas/03_data/var/02_bioclim_v20/01_wc20_sa")
dir()

# import variables e get data
var.wc20 <- raster::stack(dir(pattern = "sa"))
var.wc20

# names
names(var.wc20) <- names(var.wc20) %>% 
  sub("wc20_10m_", "", .) %>% 
  sub("_sa", "", .)
names(var.wc20)

# extract values
var.wc20.da <- var.wc20 %>% 
  raster::values() %>% 
  na.omit
var.wc20.da

# pca
# directory
dir.create("02_pca") 
setwd("02_pca") 

# pca
pca.wc20 <- FactoMineR::PCA(var.wc20.da, scale.unit = TRUE, graph = FALSE)
pca.wc20

# eigenvalues
eig.wc20 <- factoextra::get_eig(pca.wc20) %>% 
  tibble::as_tibble() %>% 
  round(2) %>% 
  dplyr::mutate(id = rownames(factoextra::get_eig(pca))) %>% 
  dplyr::select(id, everything())
eig.wc20

readr::write_csv(eig.wc20, "eigenvalues.csv")

# eigenvalues plot
factoextra::fviz_eig(pca.wc20, addlabels = TRUE, ggtheme = theme_classic())
ggsave("00_screeplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

# contributions
con.wc20 <- pca.wc20$var$contrib %>% 
  round(2)
con.wc20

write.csv(con.wc20, "contributions.csv")

# contributions
for(i in seq(5)){
  print(i)
  factoextra::fviz_contrib(pca.wc20, choice = "var", axes = i, ggtheme = theme_classic())
  ggsave(paste0("contributions_pc", i, ".tiff"), he = 15, wi = 20, un = "cm", dpi = 300)
}

# biplot
factoextra::fviz_pca(pca.wc20, geom = "point", col.ind = "black", alpha.ind = .05, repel = TRUE)
ggsave("03_biplot_pca.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

# raster pca
## pca of raster
pca.wc20.ra <- RStoolbox::rasterPCA(var.wc20, spca = TRUE) 
pca.wc20.ra

# plot pcas variables
ggplot() +
  geom_raster(data = raster::rasterToPoints(pca.wc20.ra$map[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = PC1)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  coord_equal() +
  theme_bw()

# export
raster::writeRaster(x = pca.wc20.ra$map[[1:6]], filename = paste0("pc0", 1:6, "_wc20"), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

# eta ---------------------------------------------------------------------
# directiry
setwd("/media/mauricio/data/006_orientacoes/lucas/03_data/var/03_eta_hadgen/01_eta_sa")
dir()

# import variables e get data
var.eta <- raster::stack(dir(pattern = "sa"))
var.eta

# names
names(var.eta) <- names(var.eta) %>% 
  sub("eta_hadgen_10m_", "", .) %>% 
  sub("_sa", "", .)
names(var.eta)

# extract values
var.eta.da <- var.eta %>% 
  raster::values() %>% 
  na.omit
var.eta.da

# pca
# directory
dir.create("02_pca") 
setwd("02_pca") 

# pca
pca.eta <- FactoMineR::PCA(var.eta.da, scale.unit = TRUE, graph = FALSE)
pca.eta

# eigenvalues
eig.eta <- factoextra::get_eig(pca.eta) %>% 
  tibble::as_tibble() %>% 
  round(2) %>% 
  dplyr::mutate(id = rownames(factoextra::get_eig(pca))) %>% 
  dplyr::select(id, everything())
eig.eta

readr::write_csv(eig.eta, "eigenvalues.csv")

# eigenvalues plot
factoextra::fviz_eig(pca.eta, addlabels = TRUE, ggtheme = theme_classic())
ggsave("00_screeplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

# contributions
con.eta <- pca.eta$var$contrib %>% 
  round(2)
con.eta

write.csv(con.eta, "contributions.csv")

# contributions
for(i in seq(5)){
  print(i)
  factoextra::fviz_contrib(pca.eta, choice = "var", axes = i, ggtheme = theme_classic())
  ggsave(paste0("contributions_pc", i, ".tiff"), he = 15, wi = 20, un = "cm", dpi = 300)
}

# biplot
factoextra::fviz_pca(pca.eta, geom = "point", col.ind = "black", alpha.ind = .05, repel = TRUE)
ggsave("03_biplot_pca.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

# raster pca
## pca of raster
pca.eta.ra <- RStoolbox::rasterPCA(var.eta, spca = TRUE) 
pca.eta.ra

# plot pcas variables
ggplot() +
  geom_raster(data = raster::rasterToPoints(pca.eta.ra$map[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = PC1)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  coord_equal() +
  theme_bw()

# export
raster::writeRaster(x = pca.eta.ra$map[[1:5]], filename = paste0("pc0", 1:5, "_eta"), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

# end ---------------------------------------------------------------------