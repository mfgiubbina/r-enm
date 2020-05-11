### ensemble por media - parte 3 ###

# importando os arquivos
suit0k <- read.table("Output0k.txt", h = T) 
head(suit0k)
suit21k <- read.table("Output21k.txt", h = T) 
suit6k <- read.table("Output6k.txt", h = T) 

# padronizacao
suit0k.std <- decostand(suit0k[, -c(6, 7)], method = "standardize", margin = 2)
head(suit0k.std)

suit21k.std <- decostand(suit21k[, -c(6, 7)], method = "standardize", margin = 2)
head(suit21k.std)

suit6k.std <- decostand(suit6k[, -c(6, 7)], method = "standardize", margin = 2)
head(suit6k.std)


# organizando a tabela das adequabilidades
suit <- cbind(suit0k.std, suit21k.std, suit6k.std)
head(suit)

# fatores para anova
tempo <- c(rep("pres", ncol(suit0k.std)), rep("lgm", ncol(suit21k.std)), 
	     rep("Holo", ncol(suit6k.std )))
tempo

enm <- rep(c("Bioclim", "Gower", "Maha", "SVM", "Maxent"), 3)
enm

MSout <- NULL

# anova hierarquica
for(i in 1:nrow(suit)){
  suit.parcial <- as.numeric(suit[i, ])
  modelo.lm <- aov(suit.parcial ~ tempo + enm)
  modelo.anova <- anova(modelo.lm)
  MSout <- rbind(MSout, modelo.anova$"Mean Sq")
  } # fecha for 'i'

modelo.anova


#Colocando nomes na Variveis
nrow(MSout)
colnames(MSout) <- c("tempo", "enm", "tempo x enm")

#calculando proporcao de variancia explicada por cada componente
MSout.sum <- apply(MSout, 1, sum)
 
MSout.prop <- MSout/MSout.sum
MSout.prop <- cbind(suit0k[, 1:2], MSout.prop)

head(MSout.prop)

# mapear incerteza

gridded(MSout.prop) <- ~x+y
incerteza <- stack(MSout.prop)
plot(incerteza)

MSout.prop


writeRaster(incerteza$tempo, "incerteza_tempo.asc", format="ascii")
writeRaster(incerteza$enm, "incerteza_enm.asc", format="ascii")
writeRaster(incerteza$tempo.x.enm, "incerteza_enm_tempo.asc", format="ascii")
