#setwd("/Users/matheusribeiro/Dropbox/aaa_Matheus_Ribeiro-2014-11-06/Producao Cientifica/em andamento/Karol/modelos ENMs")
#a= read.table("allobates brunneus.txt", h=T)
#map(xlim= c(-82, -34) , ylim= c(-18, 13))
#points(coord.sp, col="blue")
#points(a[,-1], pch=20, col=2)



#rm(list=ls())

options(java.parameters = "-Xmx1g" )
library(raster)
library(maps)
library(dismo)
library(adehabitatHS)
library(data.table)
library(Rfast)
library(vegan)
library(rJava)
library(kernlab)


dir.climaPres <- "/Volumes/RIBEIRO_SEA/WorldClim/Bio30s(1.4)"
#dir.clima2050 <- "/Volumes/RIBEIRO_SEA/worldClim/30 sec (1km)/2050"
dir.clima2070 <- "/Volumes/RIBEIRO_SEA/WorldClim/2070"
dir.ocor <- "/Users/matheusribeiro/Dropbox/aaa_Matheus_Ribeiro-2014-11-06/Producao Cientifica/em andamento/Kaue/Dados"
dir.output <- "//Users/matheusribeiro/Dropbox/aaa_Matheus_Ribeiro-2014-11-06/Producao Cientifica/em andamento/Kaue/Dados/outputENMs"


AOGCMs <- c("cc26bi70","cc45bi70","cc60bi70","cc85bi70", "hd26bi70","hd45bi70","hd60bi70","hd85bi70", "mr26bi70","mr45bi70","mr60bi70","mr85bi70")


setwd(dir.ocor)
ocor <- read.csv("Ocorrencia serpentes cerrado.csv", h=T)


climaPres <- stack(paste(dir.climaPres, paste0("PC", 1:6, " #Pres.tif"), sep= "/"))
names(climaPres) <- paste("PC", 1:6, sep="")
coord <- rasterToPoints(climaPres[[1]])[,1:2]
bkg <- coord[sample(1:nrow(coord), 10000), ]
combClima <- data.frame(a= c(1,1,1,1,1,2,2,2,2,3,3,3,4,4,5), b= c(2,3,4,5,6,3,4,5,6,4,5,6,5,6,6))
	

#selecionando especies com 3 ou mais pontos
spp <- levels(ocor[,"species"])
ocor1 <- NULL
spp1 <- NULL
for(i in 1:length(spp)){
	coord.sp <- ocor[ocor[,"species"]==spp[i],2:3]
	sp.val <- extract(climaPres, coord.sp, cellnumber=T)
	id.dup <- duplicated(sp.val[,"cells"])
	coord.sp <- coord.sp[which(id.dup==FALSE),]

	ocor1 <- rbind(ocor1, data.frame(species= spp[i], coord.sp))
	spp1 <- rbind(spp1, data.frame(species= spp[i], PtsTotal= nrow(sp.val), PtsUnicos= nrow(coord.sp)))
}#end for'i'
rm(i)
ocor <- ocor1
#id.spp.mais4 <- which(spp1[,"PtsUnicos"]>3)

spp.menos4 <- spp1[which(spp1[,"PtsUnicos"] <= 3), "species"]
spp.4_25pts <- spp1[which(spp1[,"PtsUnicos"] >= 3 & spp1[,"PtsUnicos"] <= 25), "species"]
spp.mais25 <- spp1[which(spp1[,"PtsUnicos"] >= 25), "species"]
rm(spp, spp1, coord.sp, sp.val, id.dup, sp.val, ocor1)


#############################################################################

#############   MODELOS ENMs  #######################

#########################################################################

#ENMs para cada sp 4-25 pontos

#spp.4_25pts <- spp.4_25pts[-1:3]
sp = spp.4_25pts[1]
for(sp in spp.4_25pts){
	coord.sp <- ocor[ocor[,1]==sp,2:3]

	print(paste("#___________#", sp, "   ", Sys.time(),  sep=""))

	for(j in 1:nrow(combClima)){

		climaPres.comb <- climaPres[[ combClima[j,] ]]
		treino <- prepareData(x= climaPres.comb, p= coord.sp, b= bkg)
		
		
		##### Jacknife
		Bioclim.xi <- Enfa.xi <- Gower.xi <- Maxent.xi <- SVM.xi <- numeric()
		Bioclim.pi <- Enfa.pi <- Gower.pi <- Maxent.pi <- SVM.pi <- numeric()
		
		for(i in 1:nrow(coord.sp)){
			Bioclim.model.loo <- bioclim(x= climaPres.comb, p= coord.sp[-i,])
			Bioclim.out.loo <- predict(x= climaPres.comb, object= Bioclim.model.loo)
			Bioclim.thr <- quantile(extract(Bioclim.out.loo, coord.sp[-i,]), 0.05)			
			Bioclim.xi[i] <- ifelse(extract(Bioclim.out.loo, coord.sp[i,])>= Bioclim.thr, 1, 0)
			Bioclim.pi[i] <- sum(values(Bioclim.out.loo >= Bioclim.thr), na.rm=T)/length(na.omit(values(Bioclim.out.loo)))
			rm(Bioclim.model.loo, Bioclim.out.loo, Bioclim.thr)

			Gower.model.loo <- dismo::domain(x= climaPres.comb, p= coord.sp[-i,])
			Gower.out.loo <- predict(x= climaPres.comb, object= Gower.model.loo)
			Gower.thr <- quantile(extract(Gower.out.loo, coord.sp[-i,]), 0.05)			
			Gower.xi[i] <- ifelse(extract(Gower.out.loo, coord.sp[i,])>= Gower.thr, 1, 0)
			Gower.pi[i] <- sum(values(Gower.out.loo >= Gower.thr), na.rm=T)/length(na.omit(values(Gower.out.loo)))
			rm(Gower.model.loo, Gower.out.loo, Gower.thr)

			Sys.setenv(NOAWT=TRUE)
			Maxent.model.loo <- maxent(x= treino[-i,-1], p= treino[-i,"pb"])
			Maxent.out.loo <- predict(object= climaPres.comb, model= Maxent.model.loo)
			Maxent.thr <- quantile(extract(Maxent.out.loo, coord.sp[-i,]), 0.05)			
			Maxent.xi[i] <- ifelse(extract(Maxent.out.loo, coord.sp[i,])>= Maxent.thr, 1, 0)
			Maxent.pi[i] <- sum(values(Maxent.out.loo >= Maxent.thr), na.rm=T)/length(na.omit(values(Maxent.out.loo)))
			rm(Maxent.model.loo, Maxent.out.loo, Maxent.thr)

			SVM.model.loo <- ksvm(pb ~ ., data= treino)
			SVM.out.loo <- predict(object= climaPres.comb, model= SVM.model.loo)
			SVM.thr <- quantile(extract(SVM.out.loo, coord.sp[-i,]), 0.05)			
			SVM.xi[i] <- ifelse(extract(SVM.out.loo, coord.sp[i,])>= SVM.thr, 1, 0)
			SVM.pi[i] <- sum(values(SVM.out.loo >= SVM.thr), na.rm=T)/length(na.omit(values(SVM.out.loo)))
			rm(SVM.model.loo, SVM.out.loo, SVM.thr)

			#Enfa
			climaPres.values <- values(climaPres.comb)
			  climaPres.spdf <- na.omit(data.frame(xyFromCell(climaPres.comb, 1:ncell(climaPres.comb)), climaPres.values))
			  gridded(climaPres.spdf) <- ~x+y
			pr.cell <- extract(climaPres.comb, coord.sp, cellnumber=T)
			  pr <- data.frame(pr= rep(0, ncell(climaPres.comb)), climaPres.values)
			  pr[pr.cell[,"cells"],] <- 1
			  pr <- na.omit(pr)
			  pr <- pr[,1]
			media.climaPres <- apply(slot(climaPres.spdf, "data"), 2, mean)
 		   	  sd.climaPres <- apply(slot(climaPres.spdf, "data"), 2, sd) 
    		  climaPres.scale<- sweep(slot(climaPres.spdf, "data"),2, media.climaPres) #diminui cada coluna de 'new.climate' da sua respectiva media em 'm.baseline'
  			  climaPres.scale<- as.matrix(climaPres.scale) %*% diag(1/sd.climaPres)
			Enfa.model.loo <- madifa(dudi.pca(climaPres.scale, center=F, scale=F, scannf=F), pr, scannf=F)
			Enfa.out.loo <- Predict.enfa(object.enfa= Enfa.model.loo, baseline.climate= climaPres.spdf, new.climate= climaPres.spdf)
			Enfa.thr <- quantile(extract(Enfa.out.loo, coord.sp[-i,]), 0.05)			
			Enfa.xi[i] <- ifelse(extract(Enfa.out.loo, coord.sp[i,])>= Enfa.thr, 1, 0)
			Enfa.pi[i] <- sum(values(Enfa.out.loo >= Enfa.thr), na.rm=T)/length(na.omit(values(Enfa.out.loo)))
			rm(climaPres.values, climaPres.spdf, pr.cell, pr, Enfa.model.loo, Enfa.out.loo, Enfa.thr)

			
			print(paste("LOO coord:", i, "/", nrow(coord.sp), " (comb:", j, "/15", ") #", Sys.time(), sep=""))
			
			}#ends for(i)_jacknife
				
		Bioclim.loo <- jacknife.LOO(Bioclim.xi, Bioclim.pi)
		Enfa.loo <- jacknife.LOO(Enfa.xi, Enfa.pi)
		Gower.loo <- jacknife.LOO(Gower.xi, Gower.pi)
		Maxent.loo <- jacknife.LOO(Maxent.xi, Maxent.pi)
		SVM.loo <- jacknife.LOO(SVM.xi, SVM.pi)
		Resu.loo <- rbind(Bioclim.loo, Enfa.loo, Gower.loo, Maxent.loo, SVM.loo)
		colnames(Resu.loo) <- c("d", "p_value")
		setwd(dir.output)
		write.table(Resu.loo, paste("Out2. Eval_Comb", j," (", sp, ") #Jacknife_LOO.txt", sep=""), sep="\t")
		rm(Bioclim.xi, Bioclim.pi, Enfa.xi, Enfa.pi, Gower.xi, Gower.pi, Maxent.xi, Maxent.pi, SVM.xi, SVM.pi)
		rm(Bioclim.loo, Enfa.loo, Gower.loo, Maxent.loo, SVM.loo, Resu.loo)
			
			

		
		#############
		#### ENMs- small models 
		
		#Bioclim
		Bioclim.model <- bioclim(x= climaPres.comb, p= coord.sp)
		Bioclim.pres <- predict(x= climaPres.comb, object= Bioclim.model)
		
		#Gower
		Gower.model <- dismo::domain(x= climaPres.comb, p= coord.sp)
		Gower.pres <- predict(x= climaPres.comb, object= Gower.model)
		
		#Maxent
		Sys.setenv(NOAWT=TRUE)
		Maxent.model <- maxent(x= treino[,-1], p= treino[,"pb"])
		Maxent.pres <- predict(object= climaPres.comb, model= Maxent.model)
		
		#SVM
		SVM.model <- ksvm(pb ~ ., data= treino)
		SVM.pres <- predict(object= climaPres.comb, model= SVM.model)
		
		#Enfa
		climaPres.values <- values(climaPres.comb)
		  climaPres.spdf <- na.omit(data.frame(xyFromCell(climaPres.comb, 1:ncell(climaPres.comb)), climaPres.values))
		  gridded(climaPres.spdf) <- ~x+y
		pr.cell <- extract(climaPres.comb, coord.sp, cellnumber=T)
		  pr <- data.frame(pr= rep(0, ncell(climaPres.comb)), climaPres.values)
		  pr[pr.cell[,"cells"],] <- 1
		  pr <- na.omit(pr)
		  pr <- pr[,1]	
		media.climaPres <- apply(slot(climaPres.spdf, "data"), 2, mean)
    	  sd.climaPres <- apply(slot(climaPres.spdf, "data"), 2, sd) 
    	  climaPres.scale<- sweep(slot(climaPres.spdf, "data"),2, media.climaPres) #diminui cada coluna de 'new.climate' da sua respectiva media em 'm.baseline'
  		  climaPres.scale<- as.matrix(climaPres.scale) %*% diag(1/sd.climaPres)
		Enfa.model <- madifa(dudi.pca(climaPres.scale, center=F, scale=F, scannf=F), pr, scannf=F)
		Enfa.pres <- Predict.enfa(object.enfa= Enfa.model, baseline.climate= climaPres.spdf, new.climate= climaPres.spdf)
		rm(climaPres.values, pr.cell, pr)
		
		setwd(dir.output)
		writeRaster(Bioclim.pres, paste("Out1. OrigModel_Comb", j," (", sp, ") #Bioclim_Pres.tif", sep=""), format= "GTiff", overwrite=T)
		writeRaster(Enfa.pres, paste("Out1. OrigModel_Comb", j," (", sp, ") #Enfa_Pres.tif", sep=""), format= "GTiff", overwrite=T)
		writeRaster(Gower.pres, paste("Out1. OrigModel_Comb", j," (", sp, ") #Gower_Pres.tif", sep=""), format= "GTiff", overwrite=T)
		writeRaster(Maxent.pres, paste("Out1. OrigModel_Comb", j," (", sp, ") #Maxent_Pres.tif", sep=""), format= "GTiff", overwrite=T)
		writeRaster(SVM.pres, paste("Out1. OrigModel_Comb", j," (", sp, ") #SVM_Pres.tif", sep=""), format= "GTiff", overwrite=T)
		rm(Bioclim.pres, Enfa.pres, Gower.pres, Maxent.pres, SVM.pres)
		
		print(paste("Small models (Pres) #", Sys.time(), sep="" ))
		
		
		
		aogcm= AOGCMs[1]
		for(aogcm in AOGCMs){
			
			climaFut <- stack(paste(dir.clima2070, aogcm, paste("PC", 1:6, " #", aogcm,".tif", sep=""), sep= "/"))
			  names(climaFut) <- paste("PC", 1:6, sep="")
			climaFut.comb <- climaFut[[ combClima[j,] ]]
			climaFut.values <- values(climaFut.comb)
			  climaFut.spdf <- na.omit(data.frame(xyFromCell(climaFut.comb, 1:ncell(climaFut.comb)), climaFut.values))
			  gridded(climaFut.spdf) <- ~x+y
			rm(climaFut, climaFut.values)
			
			Bioclim.fut <- predict(x= climaFut.comb, object= Bioclim.model)		
			Gower.fut <- predict(x= climaFut.comb, object= Gower.model)		
			Maxent.fut <- predict(object = climaFut.comb, model = Maxent.model)		
			SVM.fut <- predict(object = climaFut.comb, model = SVM.model)		
			Enfa.fut <- Predict.enfa(object.enfa= Enfa.model, baseline.climate= climaPres.spdf, new.climate= climaFut.spdf)
			
			setwd(dir.output)
			writeRaster(Bioclim.fut, paste("Out1. OrigModel_Comb", j," (", sp, ") #Bioclim_", aogcm,".tif", sep=""), format= "GTiff", overwrite=T)
			writeRaster(Enfa.fut, paste("Out1. OrigModel_Comb", j," (", sp, ") #Enfa_", aogcm,".tif", sep=""), format= "GTiff", overwrite=T)
			writeRaster(Gower.fut, paste("Out1. OrigModel_Comb", j," (", sp, ") #Gower_", aogcm,".tif", sep=""), format= "GTiff", overwrite=T)
			writeRaster(Maxent.fut, paste("Out1. OrigModel_Comb", j," (", sp, ") #Maxent_", aogcm,".tif", sep=""), format= "GTiff", overwrite=T)
			writeRaster(SVM.fut, paste("Out1. OrigModel_Comb", j," (", sp, ") #SVM_", aogcm,".tif", sep=""), format= "GTiff", overwrite=T)
			rm(Bioclim.fut, Enfa.fut, Gower.fut, Maxent.fut, SVM.fut)

			
			print(paste("Small models (", aogcm, ") #", Sys.time(), sep="" ))
			
			}#ends for(aogcm)
	
		}#ends for(comb)



	###################
	###### ENSEMBLE
	
	## Ensemble presente
	Ens.pres <- stack()
	setwd(dir.output)
	for(j in 1:nrow(combClima)){

		d <- read.table( paste("Out2. Eval_Comb", j," (", sp, ") #Jacknife_LOO.txt", sep=""), h=T)[,"d"]
		Comb.pres <- stack(paste("Out1. OrigModel_Comb", j," (", sp, ") #", c("Bioclim", "Enfa","Gower","Maxent","SVM"),"_Pres.tif", sep=""))
		  names(Comb.pres) <- c("Bioclim", "Enfa","Gower","Maxent","SVM")
		ENMvals <- data.frame(na.omit(values(Comb.pres)) )
		  ENMvals <- decostand(ENMvals, "range", 2)
		  ENMvals <- apply(ENMvals, 1, function(x) sum(x*d)/sum(d))
		Ens.pres <- stack(Ens.pres, rasterFromXYZ(cbind(coord, ENMvals)))
		rm(ENMvals, Comb.pres, d)
		
	}#ends for(j):ensemblePres
	
	Ensemble.pres <- calc(Ens.pres, fun= mean)
	writeRaster(Ensemble.pres, paste("Out3. ContEnsemble (", sp, ") #Pres.tif", sep=""), format= "GTiff", overwrite=T)
	rm(Ens.pres, Ensemble.pres)
	print(paste("--> ENSEMBLE (Pres) #", Sys.time(), sep="" ))
	
	## Ensemble futuro
	setwd(dir.output)
	RCPs <- data.frame(RCP26= AOGCMs[c(1,5,9)], RCP45= AOGCMs[c(2,6,10)], RCP60= AOGCMs[c(3,7,11)], RCP85= AOGCMs[c(4,8,12)] )	
	for(rcp in 1:ncol(RCPs)){
		aogcm= AOGCMs[1]
		Ensemble.fut <- stack()
		for(i in 1:nrow(RCPs)){
			aogcm <- RCPs[i,rcp]
			Ens.fut <- stack()		
			for(j in 1:nrow(combClima)){

				d <- read.table( paste("Out2. Eval_Comb", j," (", sp, ") #Jacknife_LOO.txt", sep=""), h=T)[,"d"]
				Comb.fut <- stack( paste("Out1. OrigModel_Comb", j," (", sp, ") #", c("Bioclim", "Enfa","Gower","Maxent","SVM"), "_", aogcm,".tif", sep="") )
				  names(Comb.fut) <- c("Bioclim", "Enfa","Gower","Maxent","SVM")
				ENMvals <- data.frame(na.omit(values(Comb.fut)) )
				  ENMvals <- decostand(ENMvals, "range", 2)
				  ENMvals <- apply(ENMvals, 1, function(x) sum(x*d)/sum(d))
				Ens.fut <- stack(Ens.fut, rasterFromXYZ(cbind(coord, ENMvals)))
				rm(ENMvals, Comb.fut, d)
		
			}#ends for(j_comb):ensembleFut

		Ensemble.fut <- stack( Ensemble.fut, calc(Ens.fut, fun= mean))
		rm(Ens.fut)
		print(paste("Ensemble inicial (", colnames(RCPs)[rcp], " / ", aogcm, ") #", Sys.time(), sep="" ))
			
		}#ends for(i_aogcm):ensembleFUT
		
		 writeRaster(calc(Ensemble.fut, fun= mean), paste("Out3. ContEnsemble (", sp, ") #",colnames(RCPs)[rcp],".tif", sep=""), format= "GTiff", overwrite=T)
		 rm(Ensemble.fut)
		 print(paste("--> ENSEMBLE (", colnames(RCPs)[rcp], ") #", Sys.time(), sep="" ))
	
		 }#ends for(rcp)
	
}#ends for(sp)	
