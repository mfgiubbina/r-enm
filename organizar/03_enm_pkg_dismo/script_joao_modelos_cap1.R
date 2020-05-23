rm(list=ls()) #remover lista

#leia os pacotes abaixo:

library(sp)
library(raster)

## Ler os registros de ocorrencia
setwd("C:/Geoprocessamento/Cap_1/Estudo de caso 1/Joao")
list.files()

sp<-read.csv("C:/Geoprocessamento/Cap_1/Estudo de caso 1/Joao/chaq_all_oppc.csv")
sp

sp<-sp[,2:3]

#sp<-cbind(sp[,2],sp[,1])

colnames(sp)<-c("x", "y")

head(sp)

library(maptools)
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-90,-35), ylim=c(-37, 12), axes=T, col="ivory2", cex.lab=1)
box()
points(sp[,1],sp[,2], col="red")

### manipulando arquivos ambientais - GIS

library(raster)
setwd("ambientais")
setwd("mask/modis")
dir()

bio1 = raster("mo_bio01_sa_south_eq_eq.tif")
bio4 = raster("mo_bio04_sa_south_eq_eq.tif")
bio12 = raster("mo_bio12_sa_south_eq_eq.tif")
bio15 = raster("mo_bio15_sa_south_eq_eq.tif")



#bioclim=stack(dir())
#bioclim

## Escolhendo a extensao geografica

e <- extent(c(-90, -35, -37, 12))

bio1 = crop(bio1,e)
bio4 = crop(bio4,e)
bio12 = crop(bio12,e)
bio15 = crop(bio15,e)

par(mfrow=c(2,2))

plot(bio1)
plot(bio4)
plot(bio12)
plot(bio15)

coord<-xyFromCell(bio1, 1:ncell(bio1))#extrai coordenada
head(coord)
nrow(coord)


bio1 = extract(bio1,1:ncell(bio1))#extrai o valor da camada
bio4 = extract(bio4,1:ncell(bio4))
bio12 = extract(bio12,1:ncell(bio12))
bio15 = extract(bio15,1:ncell(bio15))

#Construindo a matriz de dados climaticos

clima = cbind(bio1, bio4, bio12, bio15)
class(clima)
head(clima)
ncol(clima)
nrow(clima)


clima.valores<-cbind(coord, clima)
head(clima.valores)
clima.valores[clima.valores == -9999] <- NA
clima.valores <- na.omit(clima.valores)
head(clima.valores)
nrow(clima.valores)

getwd()
setwd("C:/Geoprocessamento/Cap_1/Estudo de caso 1/Joao")

write.table(clima.valores,"modis.txt", sep="\t")

### escolha das variaveis
library(psych)

fa.parallel(clima.valores[,-c(1:2)], fa='fa') #scree plot
clima0k.fa <- fa(clima.valores[,-c(1:2)], nfactors= 5, rotate= 'varimax')
clima0k.loadings <- loadings(clima0k.fa)

######################################################################
### Criar grade

setwd("C:/Geoprocessamento/Cap_1/Estudo de caso 1/Joao")
clima0k<-read.table("modis.txt",na.strings=c("", "NA"), h=T)

nrow(clima0k)
head(clima0k)
gridded(clima0k)<- ~x+y
clima0k.r<-stack(clima0k)
par(mfrow=c(2,2))
plot(clima0k.r)

## Extrair dados bioclim?ticos dos registros de ocorr?ncia

species.var<-extract(clima0k.r,sp[,1:2],cellnumbers=T)
head(species.var)
species.var<-cbind(sp,species.var) ## cbind junta dados coordenadas + bioclim?ticos 
head(species.var)


## Identificar os registros de ocorr?ncia duplicados e retir?-los

duplicated(species.var[,3]) ## TRUE para valores duplicados nas c?lulas, cells = 3
id<-which(duplicated(species.var[,3])==TRUE) ## cria um arquivo que pergunta (which) de santa.var s?o TRUE
head(id)

species.var<-species.var[-id,] # excluir as linhas duplicadas 
nrow(species.var)

species.var<-na.omit(species.var) # excluir NA
nrow(species.var)

## Salvar o arquivo sp.var em txt

write.table(species.var, "chaq_modis.txt", row.names=F, sep=" ")

## Selecionar background (pseudoausencia)- aleat?rio

clima0k<-read.table("modis.txt",header=T)
head(clima0k)
clima0k<-na.omit(clima0k)
head(clima0k)

back.id<-sample(1:nrow(clima0k),nrow(species.var)) # mantendo preval?ncia de 0.5
head(back.id)

back<-clima0k[back.id,]
head(back)
points(back[,"x"], back[,"y"], pch=20, col='red')


## Salvar background em txt

write.table(back, "chaq_modis_random.txt", row.names=F, sep=" ")


##########################################################################

#leia os pacotes abaixo:

library(sp)
library(raster)
library(dismo)
library(kernlab) # SVM
library(rJava)
#C:\Users\Pesquisador\Documents\R\win-library\3.2\dismo\java

setwd("C:/Geoprocessamento/Cap_1/Estudo de caso 1/Joao")
#splist<-c("chaq_chelsa.txt","chaq_merraclim.txt","chaq_worldclim2.txt","chaq_worldclim1.txt","chaq_modis.txt")
list<-c("chelsa","merraclim","worldclim2","worldclim1","modis")
sp1<-"chaq_"

for(i in list){
  
  print(i)
  
  back <- read.table(paste(sp1,i,"_random.txt",sep=""),h=T)#back
  ocor <- read.table(paste(sp1,i, ".txt",sep=""),h=T)#ocorrencia
  
  
  cross_validation <- 10             #numero de repeticoes do cross validation
  
  #####
  # loop para considerar diferentes AOGCMs
  
  Output0k <- NULL
  auc_thr <- NULL
  tss <- NULL
  
  
  # lendo arquivos climaticos, ocorrencia, background
  setwd("C:/Geoprocessamento/Cap_1/Estudo de caso 1/Joao") # dados climaticos: worldclim, merra
  
  
  clima0k<-read.table(paste(i,".txt",sep=""),h=T)
  
  
  gridded(clima0k) <- ~x+y
  
  clima0k.r <- stack(clima0k)
  
  #objetos para guardar os resultados parciais a cada loop do cross-validation
  
  Bioclim.Pout0k <- NULL
  Gower.Pout0k <- NULL
  SVM.Pout0k <- NULL
  Maxent.Pout0k <- NULL
  
  par(mfrow=c(2,2))
  
  ###
  # loop para cross-validation
  for(j in 1:cross_validation){
    
    # dados de treino e teste
    sample.ocor <- sample(1:nrow(ocor), round(0.75*nrow(ocor)))
    sample.back <- sample(1:nrow(back), round(0.75*nrow(back)))
    
    treino <- prepareData(x= clima0k.r, p= ocor[sample.ocor,1:2], b= back[sample.back,1:2], xy=T)
    
    teste <- prepareData(x= clima0k.r, p= ocor[-sample.ocor,1:2], b= back[-sample.back,1:2], xy=T)
    
    
    
    ######
    ## Bioclim
    
    #ajustando o modelo
    Bioclim.model <- bioclim(treino[treino[,"pb"]==1, -c(1:3)])
    
    #fazendo predicoes
    Bioclim0k <- predict(clima0k.r, Bioclim.model)
    plot(Bioclim0k)
    
    #avaliando o modelo
    Bioclim.eval <- evaluate(p= teste[teste[,"pb"]==1, -c(1:3)], a= teste[teste[,"pb"]==0, -c(1:3)], model= Bioclim.model)
    
    #encontrando threshold
    Bioclim.thr <- threshold(Bioclim.eval)
    
    
    ######
    ## Gower
    
    #ajustando o modelo
    Gower.model <- domain(treino[treino[,"pb"]==1, -c(1:3)])
    
    #fazendo predicoes
    Gower0k <- predict(clima0k.r, Gower.model)
    plot(Gower0k)
    
    #avaliando o modelo
    Gower.eval <- evaluate(p= teste[teste[,"pb"]==1, -c(1:3)], a= teste[teste[,"pb"]==0, -c(1:3)], model= Gower.model)
    
    #encontrando threshold
    Gower.thr <- threshold(Gower.eval)
    
    
    ######
    ## SVM
    
    #ajustando o modelo
    SVM.model <- ksvm(pb ~ bio1+bio4+bio12+bio15, data= treino)
    
    #fazendo predicoes
    SVM0k <- predict(clima0k.r, SVM.model)
    plot(SVM0k)
    
    #avaliando o modelo
    SVM.eval <- evaluate(p= teste[teste[,"pb"]==1, -c(1:3)], a= teste[teste[,"pb"]==0, -c(1:3)], model= SVM.model)
    
    #encontrando threshold
    SVM.thr <- threshold(SVM.eval)
    
    ######
    ## Maxent
    
    #   MaxEnt is available as a standalone Java program. Dismo has a function 'maxent' that communicates with this program. To use it you must first download the program from http://www.cs.princeton.edu/~schapire/maxent/. Put the le 'maxent.jar' in the 'java' folder of the 'dismo' package. That is the folder returned by system.file("java", package="dismo"). You need MaxEnt version 3.3.3b or higher.
    
    # checking if the jar file is present. If not, skip this bit
    jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
    file.exists(jar)
    
    #ajustando o modelo
    Sys.setenv(NOAWT=TRUE)
    Maxent.model <- maxent(treino[,-c(1:3)], treino[,"pb"])
    
    
    
    #fazendo predicoes
    Maxent0k <- predict(clima0k.r, Maxent.model)
    plot(Maxent0k)
    
    #avaliando o modelo
    Maxent.eval <- evaluate(p= teste[teste[,"pb"]==1, -c(1:3)], a= teste[teste[,"pb"]==0, -c(1:3)], model= Maxent.model)
    
    #encontrando threshold
    Maxent.thr <- threshold(Maxent.eval)
    
    
    
    # salvando outputs particiais a cada loop 'i' cross-validation
    
    Bioclim.Pout0k <- cbind(Bioclim.Pout0k, values(Bioclim0k))
    Gower.Pout0k <- cbind(Gower.Pout0k, values(Gower0k))
    SVM.Pout0k <- cbind(SVM.Pout0k, values(SVM0k))
    Maxent.Pout0k <- cbind(Maxent.Pout0k, values(Maxent0k))
    
    auc_thr <- cbind(auc_thr, c(Bioclim.eval@auc, Bioclim.thr[2]), c(Gower.eval@auc, Gower.thr[2]),c(Maxent.eval@auc, Maxent.thr[2]),c(SVM.eval@auc, SVM.thr[2]))
    tss <-cbind(tss,c(Bioclim.eval@TPR + Bioclim.eval@TNR - 1),c(Gower.eval@TPR + Gower.eval@TNR - 1),c(Maxent.eval@TPR + Maxent.eval@TNR - 1),c(SVM.eval@TPR + SVM.eval@TNR - 1))
    
    
  } #fecha for 'j' - cross-validation
  
  
  
  
  #calculando as medias dos modelos parciais cross-validation
  
  Bioclim.Pout0k.mean <- apply(Bioclim.Pout0k, 1, mean)
  Gower.Pout0k.mean <- apply(Gower.Pout0k, 1, mean)
  SVM.Pout0k.mean <- apply(SVM.Pout0k, 1, mean)
  Maxent.Pout0k.mean <- apply(Maxent.Pout0k, 1, mean)
  
  Output0k <- cbind(Output0k, Bioclim= Bioclim.Pout0k.mean, Gower= Gower.Pout0k.mean, SVM= SVM.Pout0k.mean, Maxent= Maxent.Pout0k.mean)
  
  
  #inserindo coordenadas geograficas aos outputs
  
  coords <- xyFromCell(clima0k.r, 1:ncell(clima0k.r))
  
  Output0k <- cbind(coords, Output0k)
  
  # excluindo NAs dos outputs
  Output0k <- na.omit(Output0k)
  
  name<-gsub("chaq_","",i) # gsub recorta o nome do arquivo, como eu quero s? o nome do modelo e que seja o mesmo para os dados de entrada e dos modelos climaticos fiz assim - pode melhorar!
  
  setwd("C:/Geoprocessamento/Cap_1/Estudo de caso 1/Joao")
  write.table(Output0k,file=paste(gsub(".txt","",name),"_out.txt",sep=""))
  write.table(auc_thr,file=paste(gsub(".txt","",name),"_auc.txt",sep=""))
  write.table(tss,file=paste(gsub(".txt","",name),"_tss.txt",sep=""))
  
  
} #fecha "i"


getwd()

#Calculo o TSS da seguinte forma:

#Sensibilidade = TPR (true positive rate)
#Especificidade = TNR (true negative rate)

#TSS = sensibilidade(TPR) + especificidade(TNR) - 1
#isto ?, TSS ? a soma dos acertos dos modelos nas presen?as (TPR) e aus?ncias (TNR, descontada a taxa de acertos aleat?rios (1).

#A fun??o "evaluate" do pacote dismo calcula TPR e TNR (entre outros ?ndices). Assumimos que temos um objeto de avalia??o do modelo bioclim chamado 'Bioclim.eval':
#Bioclim.eval <- evaluate(p= teste[teste[,"pb"]==1, -c(1:3)], a= teste[teste[,"pb"]==0, -c(1:3)], model= Bioclim.model)


#Pra ver todos os ?ndices que a fun??o "evaluate" calcula, basta executar:
#str(Bioclim.eval)

#Pra acessar os ?ndices, use:
#Bioclim.eval@TPR
#Bioclim.eval@TNR
#Bioclim.eval@auc


#Pra calcular TSS:
#Bioclim.eval@TPR + Bioclim.eval@TNR - 1


Out<-read.table("Output_0k.txt", h=T)

head(Out)

gridded(Out)<-~x+y

Out.r<-stack(Out)
plot(Out.r)


