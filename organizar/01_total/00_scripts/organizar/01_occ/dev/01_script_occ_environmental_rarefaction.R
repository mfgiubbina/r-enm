###-------------------------------------------------------------------------------###
# Pablo Hendrigo Alves de Melo
# pablopains@yahoo.com.br
# 13-11-2017
###-------------------------------------------------------------------------------###


# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# temp
tempdir <- function() "D:\\temps"
unlockBinding("tempdir", baseenv())
assignInNamespace("tempdir", tempdir, ns = "base", envir = baseenv())
assign("tempdir", tempdir, baseenv())
lockBinding("tempdir", baseenv())
tempdir()

if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, sdmvspecies, dismo, pbdb, data.table,
               sqldf, maps, tcltk)
source('C:/Dados/GitHub/modelagem/script/envSample_SaraVarela.R') 

setwd("C:/Dados/GitHub/environmental_data/cur/2_5m/bio")
getwd()

# list name of archives
selbiovar = "bio02|bio04|bio10|bio13|bio14|bio18"
tif <- list.files(patt = selbiovar)
tif

# load variables ans rename
en <- stack(tif)
# en <- na.omit(en.v)


# direstory
setwd("C:/dados/Github/Modelagem/Data")
getwd()

# import
da <- fread("OnePointPerCell_2_5m.csv")
da <- fread("points.csv")
da <- fread("plantascalcariomodelo_unico_todas.txt")

# ordena spp por n. ocorrências
sp <- spn <- {}
for(i in unique(da$sp)){sp <- c(sp,i); spn <- c(spn,nrow(da[da$sp==i,]))}  
index2 <- order(spn); sp <- sp[index2]; spn <- spn[index2]
for (i in 1:length(sp)) {print(paste0(sp[i],'_(',spn[i],')'))}
#
table(da$sp)

# selecione o tamanho ideal da amostra, Varela et al. (2014)
# caso alguma espéce não alcance o número máximo de registros ela será colocada na classe anterior
index = 4
sample.size.top <- c(5,10,15,25,30,50,75,100,250,500,1000)[index]
sample.size.bot <- c(5,10,15,25,30,50,75,100,250,500,1000)[index-1]

if (!dir.exists('filtered.points')){dir.create('filtered.points')}; setwd('filtered.points')
filtered.points.spp.top <- filtered.points.spp.bot <- data.table()

for (j in unique(da$sp) ){

  print(j)
  coords.thrs = da[da$sp==j,]
  coords.thrs <- coords.thrs[,2:3] 
  
  data<- extract(en, coords.thrs)
  data<- as.data.frame(na.omit(data))

  # id = 1:nrow(data)
  # coords.thrs = coords.thrs[id,]
  
  if (nrow(coords.thrs) != nrow(data)) 
  {
   print(paste0(j,' (ajuste no numero de linhas) ',nrow(coords.thrs),' -> ',nrow(data) ))
   next  
  }

  nocc = nrow(data)
  
  spp.index <- nocc >= sample.size.top
  max.occ = ifelse(spp.index==T,sample.size.top,sample.size.bot)

  #if (j =='Spathicarpa gardneri'){max.occ=sample.size.bot}
  
  print(max.occ)
  
  # filters a list of vectors with the values of the variables for the species occurrences
  filters = list(data$bio02,
                 data$bio04,
                 data$bio10,
                 data$bio13,
                 data$bio14,
                 data$bio18)

  r =0.5
  # res a list of numbers that set the size of the grid to filter the data for each variable
  res <- list((range(data$bio02)[2] - range(data$bio02)[1])/r,
              (range(data$bio04)[2] - range(data$bio04)[1])/r,
              (range(data$bio10)[2] - range(data$bio10)[1])/r,
              (range(data$bio13)[2] - range(data$bio13)[1])/r,
              (range(data$bio14)[2] - range(data$bio14)[1])/r,
              (range(data$bio18)[2] - range(data$bio18)[1])/r)
  
  filtered.points = envSample(coords.thrs, 
             filters,
             res,
             F)

  while(nrow(filtered.points) < max.occ){ #nrow(filtered.points)){
    print(r)
    r<- r+0.5
    # res a list of numbers that set the size of the grid to filter the data for each variable
    res <- list((range(data$bio02)[2] - range(data$bio02)[1])/r,
                (range(data$bio04)[2] - range(data$bio04)[1])/r,
                (range(data$bio10)[2] - range(data$bio10)[1])/r,
                (range(data$bio13)[2] - range(data$bio13)[1])/r,
                (range(data$bio14)[2] - range(data$bio14)[1])/r,
                (range(data$bio18)[2] - range(data$bio18)[1])/r)
    
    filtered.points <- envSample(coords.thrs, 
                                 filters, 
                                 res = res, 
                                 do.plot=F)
    
  }#end while(nrow(filtered.points) < max.occ)
  
  filtered.points <- envSample(coords.thrs, 
                               filters,
                               res, 
                               do.plot=F)	
  
  if(nrow(filtered.points) > max.occ){
    id.filter <- sample(1:nrow(filtered.points), max.occ)
    filtered.points <- filtered.points[id.filter,]
    
  }#end if(nrow(filtered.points) > max.occ)
  
  if (spp.index==T){ 
    filtered.points.spp.top = rbind(filtered.points.spp.top, 
                          data.table(sp = j, long = filtered.points$lon, lat = filtered.points$lat))
  
  }else{
    filtered.points.spp.bot = rbind(filtered.points.spp.bot, 
                                    data.table(sp = j, long = filtered.points$lon, lat = filtered.points$lat))
  }
    
  
} # end j  

fwrite(filtered.points.spp.top, paste0('filtered.points.spp_',sample.size.top,'.txt'))
fwrite(filtered.points.spp.bot, paste0('filtered.points.spp_',sample.size.bot,'.txt'))
