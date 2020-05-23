### script ensemble weighted average ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 05/06/2017

###----------------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, vegan, data.table, vegan, viridis)
search()

###----------------------------------------------------------------------------###

# import data
# directory
setwd("E:/github_mauriciovancine/R-ENM/output")

# enms
# list files
tif <- list.files(patt = ".tif$")
tif

enm <- raster(tif[sample(1:length(tif), 1)])
enm
plot(enm, col = viridis(100), main = names(enm))

# evaluate
txt <- list.files(patt = ".txt$")
txt

eva <- lapply(txt, read.table)
eva
names(eva) <- txt
eva

###-----------------------------------------------------------------------------###

## weighted average ensemble 
# lists
# species
sp <- sub("zEval_svm_", "", sub(".txt", "", grep("svm", txt, value = TRUE)))
sp

# gcms
gc <- c("ACCESS")
gc <- ""
gc

# periods
pe <- c("pres", "rcp45_2050", "rcp45_2070", "rcp85_2050", "rcp85_2070")
pe <- ""
  
# algorithms
al <- c("Bioclim", "Gower", "Mahalanobis", "Maxent", "SVM")
al

# data.table
da <- data.table()
da

# names
na <- data.table()

# tss
tss <- .5

# raster
ens <- enm[[1]]
values(ens)[is.na(values(ens)) == FALSE] <- 1
plot(ens, col = viridis(100))
names(ens) <- "ens"
ens

# ensemble
dir.create("ensemble_wei2")

for(i in sp){
  
  print(paste0("Nice! The ensemble '", i, "' start!"))
  
  tif.sp <- grep(i, tif, value = TRUE)
  eva.sp <- eva[grep(i, names(eva))]
  
  tss.da <- do.call("rbind", eva.sp)$TSS
  tss.id <- which(tss.da > tss)
  tss.va <- tss.da[tss.da > tss]
  
  if(length(tss.id) == 0){
    
    print(paste0("Ops! The ensemble for '", i, "' don't have models with TSS above", tss, "!"))
    
  } else{
    
    for(j in gc){
      
      tif.gc <- grep(j, tif.sp, value = TRUE)
      eva.gc <- eva.sp[grep(j, names(eva.sp))]
      
      print(paste0("The ensemble for ", i, " started, relax, take a coffee, it may take awhile..."))
      
      
      for(k in pe){
        
        print(paste0("Nice! The ensemble '", i, "', GCM '", j, "', for '", k, "', it's done!"))
    
        tif.pe <- grep(k, tif.gc, value = TRUE)
        # da <- rbind(da, stack(tif.pe[tss.id])[], use.names = FALSE)
        da <- rbind(da, na.omit(stack(tif.pe[tss.id])[]), use.names = FALSE)
        na <- rbind(na, data.table(pe = rep(k, each = length(tif.pe[tss.id])), 
                                   mo = sub(".tif", "", tif.pe[tss.id])))
        }
      
      da.r <- data.table(decostand(da, "range", na.rm = TRUE)) 
      # da.r.pe <- data.table(pe = rep(pe, each = length(enm[])), da.r)
      da.r.pe <- data.table(pe = rep(pe, each = nrow(da)/length(pe)), da.r)
      

      for(l in pe){
        da.pe <- da.r.pe[pe == l, -1]
        values(ens)[is.na(values(ens)) == FALSE] <- apply(da.pe, 1, function (x) sum(x * tss.va) / sum(tss.va))
            
        plot(ens)
            
        setwd("ensemble_wei2")
    
        writeRaster(ens, paste0("ensemble_wei_aver_", i, "_", j, "_", l, ".tif"), format = "GTiff",
                    overwrite = TRUE)
    
        fwrite(na, "_models_used_ensemble_wei.csv")
      
        setwd("..")
  
      }
      
      ens[] <- NA
  
    }
    
  }
  
  print("Yeh! It's over!!!")
  
}

###----------------------------------------------------------------------------###
