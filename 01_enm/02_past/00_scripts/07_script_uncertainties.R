# -------------------------------------------------------------------------
# uncertainties - hierarchical anova
# mauricio vancine - mauricio.vancine@gmail.com
# 17-11-2019
# -------------------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(rgdal)
library(tidyverse)
library(vegan)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
raster::beginCluster(n = 3)

# directory
path <- "/home/mude/data/gitlab/r-sdm"
setwd(path)
dir()

# import evaluates --------------------------------------------------------
# directory
setwd("02_output")

# list files
csv <- dir(pattern = "eval_", recursive = TRUE)
csv

# import models
eva <- purrr::map_dfr(csv, readr::read_csv)
eva

# uncertainties -----------------------------------------------------------
# auc
auc_limit <- .75

# algorithms
alg <- eva$algorithm %>% unique
alg

# gcms
gcm <- c("ac", "cc", "hd", "ip", "mg", "mr")
gcm

# cenarios
ce <- c("present", "45bi50", "45bi70", "85bi50", "85bi70")
ce

# hierarchical anova
setwd(path)
dir.create("05_hierarchical_anova")
setwd("05_hierarchical_anova")

for(i in sp){
  
  # anova hierarquica

  MSout <- NULL

  for(i in 1:nrow(suit)){
    
    sui_part <- as.numeric(suit[i,])
    lm_model <- lm(suit.parcial ~ ce + alg %in% ce + gcm %in% ce)
    modelo.anova <- anova(modelo.lm)
    MSout <- rbind(MSout, modelo.anova$"Mean Sq")
    
    }

  nrow(MSout)
  colnames(MSout) <- c("tempo", "enm_tempo", "aogcm_tempo", "residuo")

  # calculando proporcao de variancia explicada por cada componente
  MSout.sum <- apply(MSout, 1, sum)

  MSout.prop <- MSout/MSout.sum
  MSout.prop <- cbind(suit0k[,1:2], MSout.prop)

  summary(MSout.prop)

  # mapear incerteza
  gridded(MSout.prop) <- ~x+y
  incerteza <- stack(MSout.prop)

  plot(incerteza)

}

# end ---------------------------------------------------------------------