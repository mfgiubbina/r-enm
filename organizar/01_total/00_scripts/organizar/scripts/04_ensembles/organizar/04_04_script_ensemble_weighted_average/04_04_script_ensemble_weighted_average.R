### script ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 04/01/2018

###----------------------------------------------------------------------------###

# 1. clear memory and load packages 
# clear workspace and increase memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, data.table, vegan, ggplot2, stringr, viridis)
search()

###----------------------------------------------------------------------------###

# import data
# directory
setwd("E:/github_mauriciovancine/R-ENM/output")

# enms
# list files
tif <- list.files(patt = ".tif$")
tif

enm <- raster(tif[1])
enm
plot(enm, col = viridis(100))

# evaluate
txt <- list.files(patt = ".txt$")
txt

eva <- lapply(txt, read.table)
eva
names(eva) <- txt
eva

###-----------------------------------------------------------------------------###

## weighted average ensemble 

# species
sp <- sub("zEval_svm_", "", sub(".txt", "", grep("svm", txt, value = T)))
sp

# algorithms
al <- c("Bioclim", "Gower", "Mahalanobis", "Maxent", "SVM")
al

# data.table
da <- data.table()
da

# tss
tss <- .5

# raster
ens <- enm[[1]]
ens[] <- NA
names(ens) <- "ens"
ens

###-----------------------------------------------------------------------------###

# ensemble
dir.create("ensemble_wei")

for(i in sp){
  
  tif.sp <- grep(i, tif, value = TRUE)
  eva.sp <- eva[grep(i, names(eva))]
  
  eva.t <- do.call("rbind", eva.sp)
  tss.da <- eva.t$TSS
  tss.id <- which(tss.da > tss)
  tss.va <- tss.da[tss.da > tss]
  

  if(length(tss.id) == 0){
    
    print(paste0("Ops! The ensemble for '", i, "' don't have models with TSS above ", tss, "!"))
    
  } else{
    
    print(paste0("The ensemble for '", i, "' started, relax, take a coffee, it may take awhile..."))
    
    da <- rbind(da, stack(tif.sp[tss.id])[], use.names = FALSE)
    da.r <- data.table(decostand(da, "range", na.rm = TRUE)) 
    
    ens[] <- apply(da.r, 1, function (x) sum(x * tss.va) / sum(tss.va))
    
    setwd("ensemble_wei")
    writeRaster(ens, paste0("ens_wei_ave_", i, ".tif"), format = "GTiff", overwrite = TRUE)
   
    fwrite(data.table(mo = row.names(eva.t[eva.t$TSS > tss,]), 
                      eva.t[eva.t$TSS > tss,]), "_models_used_ensemble_wei.csv")
     setwd("..")
    
    print(paste0("Nice! The ensemble for '", i, "' it's done!"))
    
    da <- data.table()
    ens[] <- NA
    
    }
  
  print("Yeh! It's over!!!")
  
  }
  
                   
###-----------------------------------------------------------------------------###
