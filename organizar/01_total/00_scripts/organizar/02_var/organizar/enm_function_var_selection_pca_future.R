#Env: Um RasterStack com as variáveis originais
#Dir: O Diretorio onde os PCs que explicam 95% da variância serão salvos
#DirP: Diretorio com as variáveis ambientais do futuro (atualmente 4 formatos de arquivos são aceitos: asc/tif/bil/txt)
#Save (Y/N): Caso Save=='Y' salvará os eixos que explicam 95% da variância (atualmente na mesma pasta das variáveis para o futuro originais)


setwd("/Users/matheusribeiro/Dropbox/aaa_Matheus_Ribeiro-2014-11-06/shapes_matriz/Biomas/cerrado_pantanal")
cerrado <- shapefile("cerrado.shp")
setwd("/Users/matheusribeiro/Dropbox/aaa_Matheus_Ribeiro-2014-11-06/shapes_matriz/Brasil")
BR <- shapefile("Brasil.shp")
setwd("/Users/matheusribeiro/Dropbox/aaa_Matheus_Ribeiro-2014-11-06/shapes_matriz/Biomas")
biomas <- shapefile("biomas.shp")

setwd("/Volumes/WININSTALL/worldClim/Bio30s(1.4)")
Dir = "/Volumes/WININSTALL/worldClim/Bio30s(1.4)"

files <- list.files(Dir, pattern='.bil')
Env <- stack(files[c(1, 12:19, 2:11)])
Env <- crop(Env, extent(-65, -34, -35, 0))
names(Env) <- paste("Bio", 1:19, sep="")

plot(Env[[1]])
plot(cerrado, add=T, border=2)
plot(BR, add=T, border="grey50", lwd= 0.2)
plot(biomas, add=T)

write.table(data.frame(mean= means, sd= stds), "a_Media_SD.txt", sep="\t", row.names=F)
write.table(Coef, "a_Coeficientes PCApres.txt", sep="\t", row.names=F)


###############################################
############# PCA completo - PCApres e PCAfut

PCAFuturo<-function(Env,
                    Dir,
                    DirP,
                    Save=''){

  #1.Realizar a PCA das variaveis no presente
  DF<-rasterToPoints(Env)
  DF<-na.omit(DF)
  PcaR<-DF[,-c(1:2)]
  
  means<-colMeans(PcaR)
  stds<-apply(PcaR,2,sd)
  
  #Scale transform 
  DScale <- data.frame(apply(PcaR,2,scale))
  
  # Realizar a PCA 
  DPca <- prcomp(DScale,retx=TRUE,center=F,scale=F)

  #Salvar os coeficientes
  Coef<-DPca$rotation
  
  #Porcentagem da variacaoo explicada
  NEixos<-length(summary(DPca)$importance[3,])
  CumVar<-summary(DPca)$importance[3,]
  VarEx<-data.frame(CumVar)
  
  #Recuperar os loadings e transformar em um data.frame
  Eix<-as.data.frame(DPca$x)
  EixXY<-cbind(DF[,(1:2)],Eix)
  gridded(EixXY)<- ~x+y
  PCAPr<-stack(EixXY)
  PCA.95 <- PCAPr[[1:(sum(VarEx<=0.95)+1)]]
  if(Save=="Y"){
    writeRaster(PCA.95,paste(Dir, "/", names(PCA.95), " #Pres.tif",sep=""),bylayer=T,format="GTiff",overwrite=T)
  }
  
  #2.Projetar a PCA para variaveis do futuro
  
  ProjEX <- unique(tools::file_ext(list.files(DirP)))
  form <- c('bil','asc','txt','tif')
  ProjEX <- ProjEX[ProjEX%in%form]
  
  if(ProjEX == 'bil'){
    ProjT<-brick(stack(list.files(DirP,pattern='.bil')))
  }
  
  if(ProjEX == 'asc'){
    ProjT<-brick(stack(list.files(DirP,pattern='.asc')))
  }
  
  if(ProjEX == 'txt'){
    ProjT<-read.table(list.files(DirP,pattern='.txt'),h=T)
    gridded(ProjT)<- ~x+y
    ProjT<-brick(stack(ProjT))
  }
  
  if(ProjEX == 'tif'){
    setwd(DirP)
    ProjT<-brick(stack(list.files(DirP,pattern='.tif')))
    setwd("..")
  }
  
  ProjE<-rasterToPoints(ProjT)
  ProjE<-na.omit(ProjE)
  ProjER <-ProjE[,-c(1:2)]
  
  scale<-sweep(ProjER,2,means)
  scale<-scale %*% diag(1/stds)
  PCAFut<-NULL
  
  for (x in 1:ncol(ProjER)){
    CoefPC<-as.numeric(Coef[,x])
    PC<-scale %*% CoefPC
    PCAFut <- cbind(PCAFut,PC)
  }
  colnames(PCAFut) <- colnames(Coef)
  PCAFut <- data.frame(cbind(ProjE[,(1:2)],PCAFut))
  gridded(PCAFut)<- ~x+y
  PCAFut<-stack(PCAFut)
  names(PCAFut) <- names(PCAPr)
  PCAFut.95 <- PCAFut[[1:nlayers(PCA.95)]]
  if(Save=="Y"){
    writeRaster(PCAFut.95,paste(DirP,names(PCAFut.95),sep="/"),bylayer=T,format="GTiff",overwrite=T)
  }
  return(PCAFut.95)
}







##################################################
############# PCA fut - le media, sd e PCAcoefs do presente para projetar PCAfut

#mediaSD: nome do arquivo (data.frame) com os vetores de MEDIA (1a coluna) e SD (2a coluna) das variaveis ambientais do presente
#PCAcoefs: nome do arquivo (data.frame) com os coeficientes da PCA para presente
Dir: diretorio com os arquivos mediaSD e PCAcoefs
#DirF: Diretorio com as variáveis ambientais do futuro (tif _ modelo worldClim)
#e: extensao espacial da area de estudo/background para cortar os mapas do futuro

#Save (Y/N): Caso Save=='Y' salvará os eixos que explicam 95% da variância (atualmente na mesma pasta das variáveis para o futuro originais)

rm(list=ls())


PCAfut<-function(mediaSD = "a-Media_SD.txt",
                 PCAcoefs = "a_Coeficientes PCApres.txt",
                 Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
                 DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/cc26bi50",
                 e = c(-65, -33, -35, 0),
                 nEixos = 6,
                 output= " #cc26bi50"){
  
  library(raster)
  
  #Projetar a PCA para variaveis do futuro
  means <- read.table(paste(Dir, mediaSD, sep="/"), h=T)[,1]
  stds <- read.table(paste(Dir, mediaSD, sep="/"), h=T)[, 2]
  Coef <- read.table(paste(Dir, PCAcoefs, sep="/"), h=T)
  
  files <- paste(DirF, list.files(DirF, pattern='.tif'), sep="/")
  files <- c(files[1], files[12:19], files[2:11])
  ProjT<- stack(files)
  ProjT<- crop(ProjT, extent(e))
  
  ProjE<-rasterToPoints(ProjT)
  ProjE<-na.omit(ProjE)
  ProjER <-ProjE[,-c(1:2)]
  
  scale<-sweep(ProjER,2,means)
  scale<-scale %*% diag(1/stds)
  PCAFut<-NULL
  
  for (x in 1:nEixos){
    CoefPC<-as.numeric(Coef[,x])
    PC<-scale %*% CoefPC
    PCAFut <- cbind(PCAFut,PC)
  }
  colnames(PCAFut) <- colnames(Coef)[1: nEixos]
  PCAFut <- data.frame(cbind(ProjE[,(1:2)],PCAFut))
  gridded(PCAFut)<- ~x+y
  PCAFut<-stack(PCAFut)

  writeRaster(PCAFut, paste(DirF, paste(names(PCAFut), output, sep=""), sep="/"), bylayer=T, format="GTiff", overwrite=T)
 
}#ends function "PCAfut"


###############################################
#########

PCAfut(	mediaSD = "a-Media_SD.txt",
		PCAcoefs = "a_Coeficientes PCApres.txt",
		Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
		e = c(-65, -33, -35, 0),
		nEixos = 6,
		DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/cc26bi50",
		output= " #cc26bi50")


PCAfut(	mediaSD = "a-Media_SD.txt",
		PCAcoefs = "a_Coeficientes PCApres.txt",
		Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
		e = c(-65, -33, -35, 0),
		nEixos = 6,
		DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/cc45bi50",
		output= " #cc45bi50")
		
PCAfut(	mediaSD = "a-Media_SD.txt",
		PCAcoefs = "a_Coeficientes PCApres.txt",
		Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
		e = c(-65, -33, -35, 0),
		nEixos = 6,
		DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/cc60bi50",
		output= " #cc60bi50")

PCAfut(	mediaSD = "a-Media_SD.txt",
		PCAcoefs = "a_Coeficientes PCApres.txt",
		Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
		e = c(-65, -33, -35, 0),
		nEixos = 6,
		DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/cc85bi50",
		output= " #cc85bi50")	
		


PCAfut(	mediaSD = "a-Media_SD.txt",
		PCAcoefs = "a_Coeficientes PCApres.txt",
		Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
		e = c(-65, -33, -35, 0),
		nEixos = 6,
		DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/hd26bi50",
		output= " #hd26bi50")


PCAfut(	mediaSD = "a-Media_SD.txt",
		PCAcoefs = "a_Coeficientes PCApres.txt",
		Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
		e = c(-65, -33, -35, 0),
		nEixos = 6,
		DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/hd45bi50",
		output= " #hd45bi50")
		
PCAfut(	mediaSD = "a-Media_SD.txt",
		PCAcoefs = "a_Coeficientes PCApres.txt",
		Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
		e = c(-65, -33, -35, 0),
		nEixos = 6,
		DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/hd60bi50",
		output= " #hd60bi50")

PCAfut(	mediaSD = "a-Media_SD.txt",
		PCAcoefs = "a_Coeficientes PCApres.txt",
		Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
		e = c(-65, -33, -35, 0),
		nEixos = 6,
		DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/hd85bi50",
		output= " #hd85bi50")		



PCAfut(	mediaSD = "a-Media_SD.txt",
		PCAcoefs = "a_Coeficientes PCApres.txt",
		Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
		e = c(-65, -33, -35, 0),
		nEixos = 6,
		DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/mr26bi50",
		output= " #mr26bi50")


PCAfut(	mediaSD = "a-Media_SD.txt",
		PCAcoefs = "a_Coeficientes PCApres.txt",
		Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
		e = c(-65, -33, -35, 0),
		nEixos = 6,
		DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/mr45bi50",
		output= " #mr45bi50")
		
PCAfut(	mediaSD = "a-Media_SD.txt",
		PCAcoefs = "a_Coeficientes PCApres.txt",
		Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
		e = c(-65, -33, -35, 0),
		nEixos = 6,
		DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/mr60bi50",
		output= " #mr60bi50")

PCAfut(	mediaSD = "a-Media_SD.txt",
		PCAcoefs = "a_Coeficientes PCApres.txt",
		Dir = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/wc2.0_30s",
		e = c(-65, -33, -35, 0),
		nEixos = 6,
		DirF = "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050/mr85bi50",
		output= " #mr85bi50")		



