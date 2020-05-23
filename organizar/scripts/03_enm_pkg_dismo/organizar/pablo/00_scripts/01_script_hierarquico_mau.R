### script anova hierarquica ###

# matheus lima-ribeiro
# mauricio humberto Vancine

###-----------------------------------------------------------------------------------###

# memory
rm(list=ls())

# packages
library(vegan)
library(raster)
library(data.table)
library(tidyverse)

###-----------------------------------------------------------------------------------###

# directory
setwd("F:/modelos_plantas_pablo/03_enm")

# species
sp <- dir(patt = ".txt") %>% 
  grep("enm_bioclim", ., value = TRUE) %>%
  stringr::str_replace("enm_bioclim_", "") %>% 
  stringr::str_replace(".txt", "")
sp

### anova
for(i in sp){
  
  # data
  da <- dir(patt = "enm_") %>% 
    grep(i, ., value = TRUE) %>%
    purrr::map_dfc(data.table::fread)
  dim(da)
  
  colnames(da) <- sub(" ", "_", colnames(da))
  da.id.na <- which(is.na(da$Adiantum_calcareum_bioclim_CCSM_r01_p000) == TRUE)
  da.na <- da[-da.id.na, ]
  
  da.co <- data.table::data.table(x = da.na$x, y = da.na$y)
  
  print("Standardization....")
  
  for(j in 0:21){
    da.pa <- da.na %>% 
      subset(select = grep(ifelse(j < 10, paste0("_p00", j), paste0("_p0", j)), 
                           colnames(da), value = TRUE)) %>% 
      vegan::decostand(method = "standardize", margin = 2, na.rm = TRUE)
    
    da.st <- cbind(da.co, da.pa)
  }
  

  # fatores para ANOVA
  tem <- stringr::str_split_fixed(colnames(da.st), "_", n = 6)[-c(1, 2), 6]
  alg <- stringr::str_split_fixed(colnames(da.st), "_", n = 5)[-c(1, 2), 3]
  gcm <- stringr::str_split_fixed(colnames(da.st), "_", n = 5)[-c(1, 2), 4]
  
  
  ### Anova hierarquica
  MSout <- NULL
  
  for(k in 1:nrow(da.st)){
    da.parcial <- as.numeric(da.st[k, -c(1, 2)])
    modelo.lm <- lm(da.parcial ~ tem + alg%in%tem + gcm%in%tem)
    modelo.anova <- anova(modelo.lm)
    MSout <- rbind(MSout, modelo.anova$"Mean Sq")
  }
  
  nrow(MSout)
  colnames(MSout) <- c("tempo", "enm_tempo", "aogcm_tempo", "residuo")
  
  #calculando proporcao de variancia explicada por cada componente
  MSout.sum <- apply(MSout, 1, sum)
  
  MSout.prop <- MSout/MSout.sum
  MSout.prop <- cbind(suit0k[,1:2], MSout.prop)
  
  summary(MSout.prop)
  
  # mapear incerteza
  
  gridded(MSout.prop) <- ~x+y
  incerteza <- stack(MSout.prop)
  
  plot(incerteza)
  
  
  
  