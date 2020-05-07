#Env: Um RasterStack com as variáveis originais
#EnvP: rasterStack com as variaveis para projecao (futuro, passado, ...)
#Dir: O Diretorio onde os PCs (originais e para projecao) que explicam 95% da variância serão salvos
#Save (Y/N): Caso Save=='Y' salvará os eixos que explicam 95% da variância (atualmente na mesma pasta das variáveis para o futuro originais)

#PCAProjection
#library(ENMGadgets) #via github

PCAFuturo<-function(Env,
                    EnvP,
                    nomePCAfilePres,
                    nomePCAfileFut,
                    Dir,
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
    writeRaster(PCA.95,paste(Dir, "/", nomePCAfilePres,sep=""),bylayer=F,format="GTiff",overwrite=T)
  }
  
  #2.Projetar a PCA para variaveis do futuro
  
 
  
    ProjT<- EnvP

  
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
    writeRaster(PCAFut.95,paste(Dir, nomePCAfileFut,sep="/"),bylayer=F,format="GTiff",overwrite=T)
  }
  return(PCAFut.95)
}
