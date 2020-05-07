rm(list=ls())
library(vegan)

setwd("/Users/matheusribeiro/Dropbox/aaa_Matheus_Ribeiro-2014-11-06/Producao Cientifica/em andamento/Luciana Vitorino - mangaba/modelagem")

tss <- read.table("TSS_id.csv", h=T, sep=",")
id.tss <- which(tss[,"TSS"] < 0.5)
tss <- tss[-id.tss,"TSS"]

coord <- read.table("grid neotropico.txt", h=T)[,2:3]

mang0k <- read.table("ALLmodels_Presente.csv", h=T, sep=",")
mang6k <- read.table("ALLmodels_6Ka.csv", h=T, sep=",")[-id.tss]
mang21k <- read.table("ALLmodels_21Ka.csv", h=T, sep=",")[-id.tss]
#mangrcp45 <- read.table("ALLmodels_Futuro.csv", h=T, sep=",")

todos <- rbind(mang0k, mang6k, mang21k)
todos.std <- decostand(todos, "standardize", 2)

mang0k.std <- todos.std[1:nrow(mang0k),]
mang6k.std <- todos.std[(nrow(mang0k)+1):(nrow(mang0k)*2),]
mang21k.std <- todos.std[((nrow(mang0k)*2)+1):(nrow(mang0k)*3),]

mang0k.ens <- apply(mang0k.std, 1, function (x) sum(x*tss)/sum(tss))
mang6k.ens <- apply(mang6k.std, 1, function (x) sum(x*tss)/sum(tss))
mang21k.ens <- apply(mang21k.std, 1, function (x) sum(x*tss)/sum(tss))

ens <- data.frame(coord, pres= mang0k.ens, hol= mang6k.ens, lgm= mang21k.ens)

write.table(ens, "FULLensamble.txt", sep="	", row.names=F)



###############
####### rascunho

a <- data.frame(a=1:10, b=11:20, c=21:30)
b<- c(0.1, 0.5, 0.2)

apply(a, 1, function(x) sum(x*b)/sum(b))

sum(a[10,]*b)/sum(b)



