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
setwd("F:/pablo/03_enm")

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
    purrr::map_dfr(data.table::fread)
  
  da.st <- data.table::data.table(x = da$x, y = da$y)
  
  for(j in 0:21){
    da.pa <- da %>% 
      subset(select = grep(ifelse(j < 10, paste0("_p00", j), paste0("_p0", j)), 
                           colnames(da), value = TRUE)) %>% 
      vegan::decostand(method = "standardize", margin = 2, na.rm = TRUE)
      
    da.st <- cbind(da.st, da.pa)
  }
  
  dim(da)
  dim(da.st)
  
  da.00 <- decostand(da[, grep("_p000", colnames(da), value = TRUE)], method = "standardize", margin = 2)
  da.01 <- decostand(da[grep("_p001", colnames(da))], method = "standardize", margin = 2)
  da.02 <- decostand(da[grep("_p002", colnames(da))], method = "standardize", margin = 2)
  da.03 <- decostand(da[grep("_p003", colnames(da))], method = "standardize", margin = 2)
  da.04 <- decostand(da[grep("_p004", colnames(da))], method = "standardize", margin = 2)
  da.05 <- decostand(da[grep("_p005", colnames(da))], method = "standardize", margin = 2)
  da.06 <- decostand(da[grep("_p006", colnames(da))], method = "standardize", margin = 2)
  da.07 <- decostand(da[grep("_p007", colnames(da))], method = "standardize", margin = 2)
  da.08 <- decostand(da[grep("_p008", colnames(da))], method = "standardize", margin = 2)
  da.09 <- decostand(da[grep("_p009", colnames(da))], method = "standardize", margin = 2)
  da.10 <- decostand(da[grep("_p010", colnames(da))], method = "standardize", margin = 2)
  da.11 <- decostand(da[grep("_p011", colnames(da))], method = "standardize", margin = 2)
  da.12 <- decostand(da[grep("_p012", colnames(da))], method = "standardize", margin = 2)
  da.13 <- decostand(da[grep("_p013", colnames(da))], method = "standardize", margin = 2)
  da.14 <- decostand(da[grep("_p014", colnames(da))], method = "standardize", margin = 2)
  da.15 <- decostand(da[grep("_p015", colnames(da))], method = "standardize", margin = 2)
  da.16 <- decostand(da[grep("_p016", colnames(da))], method = "standardize", margin = 2)
  da.17 <- decostand(da[grep("_p017", colnames(da))], method = "standardize", margin = 2)
  da.18 <- decostand(da[grep("_p018", colnames(da))], method = "standardize", margin = 2)
  da.19 <- decostand(da[grep("_p019", colnames(da))], method = "standardize", margin = 2)
  da.20 <- decostand(da[grep("_p020", colnames(da))], method = "standardize", margin = 2)
  da.21 <- decostand(da[grep("_p021", colnames(da))], method = "standardize", margin = 2)
  
  
  # da.st <- cbind(da.00, da.01, da.02, da.03, da.04, da.05, da.06, da.07, da.08, da.09, da.10, 
  #                da.11, da.12, da.13, da.14, da.15, da.16, da.17, da.18, da.19, da.20, da.21)
  # dim(da.st)
  # colnames(da.st)
  
  # fatores para ANOVA
  enm <- rep(c("Bioclim", "Gower", "Maxent", "RandomForest", "SVM"), each = 8)
  enm 
  
  aogcm <- rep(c("ccsm", "cnrm", "fgoals", "giss", "ipsl", "miroc", "mpi", "mri"), 5)
  aogcm
  
  colnames(da.00)
  
  ### Anova hierarquica
  MSout <- NULL
  
  for(i in 1:nrow(da.00)){
    da.parcial <- as.numeric(da.00[i, ])
    modelo.lm <- lm(da.parcial ~ enm + aogcm)
    modelo.anova <- anova(modelo.lm)
    MSout <- rbind(MSout, modelo.anova$"Mean Sq")
    
  }# fecha for 'i'
  
  
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
  
  
  
  