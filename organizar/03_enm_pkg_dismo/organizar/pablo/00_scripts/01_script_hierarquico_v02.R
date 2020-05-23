rm(list=ls())

library(vegan)
library(sp)
library(raster)


setwd("/Users/matheusribeiro/Dropbox/aaa_Matheus_Ribeiro-2014-11-06/UFG - campus Jatai/Disciplinas/ENMs/dados/outputs")

suit0k <- read.table("Output_0k.txt", h=T)
suit21k <- read.table("Output_21k.txt", h=T) 
suitrcp45 <- read.table("Output_rcp45.txt", h=T) 

suit0k.std <- decostand(suit0k[,-c(1:2)], method= "standardize", margin= 2)
suit21k.std <- decostand(suit21k[,-c(1:2)], method= "standardize", margin= 2)
suitrcp45.std <- decostand(suitrcp45[,-c(1:2)], method= "standardize", margin= 2)

suit <- cbind(suit0k.std, suit21k.std, suitrcp45.std)

#fatores para ANOVA
tempo <- c(rep("pres", ncol(suit0k.std)), rep("lgm", ncol(suit21k.std)), rep("fut", ncol(suitrcp45.std)))
enm <- rep(c("Bioclim", "Gower", "SVM", "GLM", "Maxent"), 9)
aogcm <- rep(c(rep("ccsm", 5), rep("cnrm", 5), rep("miroc", 5)), 3)




######
### Anova hierarquica

MSout <- NULL

for(i in 1: nrow(suit)){
	suit.parcial <- as.numeric(suit[i,])
	modelo.lm <- lm(suit.parcial ~ tempo + enm%in%tempo + aogcm%in%tempo)
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



