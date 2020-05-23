### script enm dismo - multiple algorithms in millenary scale ###

# Pablo Hendrigo Alves de Melo
# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 29-03-2019

###---------------------------------------------------------------------------###

# memory
rm(list = ls())

# packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, dismo, gam, kernlab, randomForest, gbm, rJava, vegan,
               spocc, usdm, tidyverse, data.table, viridis, RStoolbox, RCurl, dplyr)

###---------------------------------------------------------------------------###

## data

# directory
path <- "D:/BKP Kew/models_pablo_plants"
setwd(path)

# Tempo 1 para interpola??o = Presente
tempo1 = 0

# Tempo 2 para interpola??o = Ultimo Glacial M?ximo
tempo2 = 21

# Per?odo desejado
tempo.ini = 1
tempo.fim = 21

# replicas
r = 2

# aogcm
aogcm <- c('CCSM', 'CNRM', 'FGOALS', 'GISS', 'IPSL', 'MIROC', 'MPI', 'MRI')
aogcm

#--- Carrega concentracao de O2 ---#
LR04 = fread(paste0(path,"/02_var/data/LR04_Stack.txt"))
o.tempo1 = as.numeric(LR04[LR04$Time==tempo1,c('d18O')])
o.tempo2 = as.numeric(LR04[LR04$Time==tempo2,c('d18O')])
o.erro1 = as.numeric(LR04[LR04$Time==tempo1,c('Error')])
o.erro2 = as.numeric(LR04[LR04$Time==tempo2,c('Error')])

# occurrences
po.all <- fread(paste0(path,"/01_occ/datasetmodel.txt"))
po.all


###---------------------------------------------------------------------------###
library(CoordinateCleaner)
po.all.cl <- clean_coordinates(x = po.all,
                              species = 'sp',
                              lon = 'long',
                              lat = 'lat',
                              # countries = 'country',
                              tests = c("capitals",
                                        "centroids",
                                        # "countries",
                                        "duplicates",
                                        "equal",
                                        "institutions",
                                        "seas",
                                        "zeros",
                                        'ranges',
                                        'urban'))
colSums(!po.all.cl[,8:17])
# .val     .equ     .zer     .cap     .cen     .sea     .urb    .inst     .dpl .summary
# 0        0        0       37        3      196      232       12     5771     6027

table(po.all.cl$sp)

po.all.cl <- po.all.cl %>%
  dplyr::filter(.inst == TRUE) %>%
  dplyr::filter(.sea == TRUE) %>%
  dplyr::filter(.dpl == TRUE) %>%
  dplyr::filter(.cap == TRUE) %>%
  dplyr::filter(.urb == TRUE) %>%
  dplyr::filter(.cen == TRUE)

###---------------------------------------------------------------------------###

colnames(po.all.cl)[1:7] <- c('sp', 'long', 'lat', 'year', 'recordedByStandardized',
                      'recordNumberStandardized',	'source')

po.all.cl <- po.all.cl[,c('sp', 'long', 'lat', 'year', 'recordedByStandardized',
                    'recordNumberStandardized',	'source')]


po <- data.frame(po.all[order(po.all$sp)], stringsAsFactors = F)
po


## variables
setwd(paste0(path, "/02_var/var"))
en <- stack(dir(patt = ".tif"))
en

plot(en$bio03_CCSM_0k, col = viridis(100))


###---------------------------------------------------------------------------###

## occurrences selection

# one point per cell
id <- en$bio03_CCSM_0k
plot(id)

# id das celulas
id[!is.na(id)] <- seq(id[!is.na(id)])
plot(id)

# oppc
po.fi <- po %>%
  dplyr::mutate(oppc = raster::extract(id, dplyr::select(po, long, lat))) %>%
  dplyr::distinct(sp, oppc, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(oppc))
po.fi

table(po$sp) %>% as.data.frame()
table(po.fi$sp) %>% as.data.frame()

plot(en[[1]], col = viridis(100))
points(po$lon, po$lat, pch = 20)
points(po.fi$lon, po.fi$lat, pch = 20, col = "red")

###---------------------------------------------------------------------------###

## background coordinates
bc <- rasterToPoints(en[[1]])[, 1:2]
colnames(bc[, -3]) <- c("long", "lat")

plot(en[[1]], col = viridis(100))
points(bc[sample(1:nrow(bc), 1000), ], pch = 20, cex = .5, col = "blue")
points(po.fi[, 2:3], pch = 20, cex = .5, col = "red")

###---------------------------------------------------------------------------###

# verify maxent
file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))

###---------------------------------------------------------------------------###

### enms ###

# dir
setwd(path)
dir.create("03_enm_again")
setwd("03_enm_again")

# enms
for(i in seq(1)){ # for to each specie

  # data for enm
  da.bioclim <- data.frame(xyFromCell(en, cell = 1:ncell(en)))
  da.gower <- data.frame(xyFromCell(en, cell = 1:ncell(en)))
  da.randomforest <- data.frame(xyFromCell(en, cell = 1:ncell(en)))
  da.maxent <- data.frame(xyFromCell(en, cell = 1:ncell(en)))
  da.svm <- data.frame(xyFromCell(en, cell = 1:ncell(en)))

  # selecting presence and absence
  id.specie <- as.character(unique(po.fi[, 1]))[i]
  pr.specie <- po.fi[which(po.fi[, 1] == id.specie), 2:3]
  id.background <- sample(nrow(bc), nrow(pr.specie))
  bc.specie <- bc[id.background, ]


  # aogcm
  for(a in aogcm){

    # variables for evaluate
    eval.Bioclim <- NULL
    eval.Gower <- NULL
    eval.RandomForest <- NULL
    eval.Maxent <- NULL
    eval.SVM <- NULL

    # selecting aogcm
    en.a <- en[[grep(a, names(en))]]
    en.a.0 <- en.a[[grep("0k", names(en.a))]]
    en.a.21 <- en.a[[grep("21k", names(en.a))]]


    # repetition
    for(r in seq(r)){	# number of replicas

      # information
      print(paste0(id.specie, " ", a, " ", r))

      ## preparing the models
      # train and test data
      pr.sample.train <- sample(nrow(pr.specie), round(.75 * nrow(pr.specie)))
      bc.sample.train <- sample(nrow(bc.specie), round(.75 * nrow(bc.specie)))
      train <- na.omit(prepareData(x = en.a.0,
                                   p = pr.specie[pr.sample.train, ],
                                   b = bc.specie[bc.sample.train, ]))
      test <- na.omit(prepareData(x = en.a.0,
                                  p = pr.specie[-pr.sample.train, ],
                                  b = bc.specie[-bc.sample.train, ]))

      ### algorithms ###

      ## 1. bioclim
      Bioclim <- bioclim(train[which(train[, 1] == 1), -1])
      da.bioclim <- cbind(da.bioclim, predict(brick(en.a.0), Bioclim, progress = "text")[])

      colu <- paste0(id.specie, "_bioclim_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p000")
      colnames(da.bioclim) <- c(colnames(da.bioclim[, -ncol(da.bioclim)]), colu)

      eBioclim <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Bioclim)
      idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens")))
      eval.Bioclim.sp <- tibble::tibble(species = id.specie,
                                        replica = ifelse(r < 10, paste0("0", r), r),
                                        algorithm = "bioclim",
                                        threshold = eBioclim@t[idBioclim],
                                        auc = eBioclim@auc,
                                        tss = eBioclim@TPR[idBioclim] + eBioclim@TNR[idBioclim] - 1,
                                        file = paste0(id.specie, "_bioclim_", a, "_r", ifelse(r < 10, paste0("0", r), r)))
      eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)


      for(tempox in tempo.ini:tempo.fim){

        periodo = ifelse(tempox < 100, ifelse(tempox<10, paste0("00", tempox), paste0("0", tempox)), tempox)

        tempox.lbl = paste0(tempox, 'kyr')
        o.tempox = as.numeric(LR04[LR04$Time==tempox,c('d18O')])
        o.errox = as.numeric(LR04[LR04$Time==tempox,c('Error')])

        print(paste0(tempox.lbl,': ',o.tempox) )
        print('calculando...')

        # calculo
        var.tempox = en.a.0 - (((en.a.0 - en.a.21) * (o.tempo1 - o.tempox)) / (o.tempo1 - o.tempo2))
        print(paste0('projetando: ', tempox.lbl))

        da.bioclim <- cbind(da.bioclim, colu = values(predict(var.tempox, Bioclim, progress = "text")))

        colu <- paste0(id.specie, "_bioclim_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p", periodo)
        colnames(da.bioclim) <- c(colnames(da.bioclim[, -ncol(da.bioclim)]), colu)

      }

      print(paste0("Yeh! The model witch AOGMC ", a," of ",id.specie, ", algorithm 'Bioclim', replica ",
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))

      ## 2. gower

      Gower <- domain(train[which(train[, 1] == 1), -1])
      da.gower <- cbind(da.gower, predict(brick(en.a.0), Gower, progress = "text")[])

      colu <- paste0(id.specie, "_gower_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p000")
      colnames(da.gower) <- c(colnames(da.gower[, -ncol(da.gower)]), colu)

      eGower <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Gower)
      idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
      eval.Gower.sp <- tibble::tibble(species = id.specie,
                                        replica = ifelse(r < 10, paste0("0", r), r),
                                        algorithm = "gower",
                                        threshold = eGower@t[idBioclim],
                                        auc = eGower@auc,
                                        tss = eGower@TPR[idGower] + eGower@TNR[idGower] - 1,
                                        file = paste0(id.specie, "_gower_", a, "_r", ifelse(r < 10, paste0("0", r), r)))
      eval.Gower <- rbind(eval.Gower, eval.Gower.sp)


      for(tempox in tempo.ini:tempo.fim){

        periodo = ifelse(tempox < 100, ifelse(tempox<10, paste0("00", tempox), paste0("0", tempox)), tempox)

        tempox.lbl = paste0(tempox, 'kyr')
        o.tempox = as.numeric(LR04[LR04$Time==tempox,c('d18O')])
        o.errox = as.numeric(LR04[LR04$Time==tempox,c('Error')])

        print(paste0(tempox.lbl,': ',o.tempox) )
        print('calculando...')

        # calculo
        var.tempox = en.a.0 - (((en.a.0 - en.a.21) * (o.tempo1 - o.tempox)) / (o.tempo1 - o.tempo2))
        print(paste0('projetando: ', tempox.lbl))

        da.gower <- cbind(da.gower, colu = values(predict(var.tempox, Gower, progress = "text")))

        colu <- paste0(id.specie, "_gower_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p", periodo)
        colnames(da.gower) <- c(colnames(da.gower[, -ncol(da.gower)]), colu)

      }

      print(paste0("Yeh! The model witch AOGMC ", a," of ", id.specie, ", algorithm 'Gower', replica ",
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))

      ## 3. random forest

      RandomForest <- randomForest(pb ~ ., data = train) ## ????
      da.randomforest <- cbind(da.randomforest, predict(brick(en.a.0), RandomForest, progress = "text")[])

      colu <- paste0(id.specie, "_randomforest_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p000")
      colnames(da.randomforest) <- c(colnames(da.randomforest[, -ncol(da.randomforest)]), colu)

      eRandomForest <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = RandomForest)
      idRandomForest <- which(eRandomForest@t == as.numeric(threshold(eRandomForest, "spec_sens")))
      eval.RandomForest.sp <- tibble::tibble(species = id.specie,
                                      replica = ifelse(r < 10, paste0("0", r), r),
                                      algorithm = "randomforest",
                                      threshold = eRandomForest@t[idRandomForest],
                                      auc = eRandomForest@auc,
                                      tss = eRandomForest@TPR[idRandomForest] + eRandomForest@TNR[idRandomForest] - 1,
                                      file = paste0(id.specie, "_randomforest_", a, "_r", ifelse(r < 10, paste0("0", r), r)))
      eval.RandomForest <- rbind(eval.RandomForest, eval.RandomForest.sp)


      for(tempox in tempo.ini:tempo.fim){

        periodo = ifelse(tempox < 100, ifelse(tempox<10, paste0("00", tempox), paste0("0", tempox)), tempox)

        tempox.lbl = paste0(tempox, 'kyr')
        o.tempox = as.numeric(LR04[LR04$Time==tempox,c('d18O')])
        o.errox = as.numeric(LR04[LR04$Time==tempox,c('Error')])

        print(paste0(tempox.lbl,': ',o.tempox) )
        print('calculando...')

        # calculo
        var.tempox = en.a.0 - (((en.a.0 - en.a.21) * (o.tempo1 - o.tempox)) / (o.tempo1 - o.tempo2))
        print(paste0('projetando: ', tempox.lbl))

        da.randomforest <- cbind(da.randomforest, colu = values(predict(var.tempox, RandomForest, progress = "text")))

        colu <- paste0(id.specie, "_randomforest_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p", periodo)
        colnames(da.randomforest) <- c(colnames(da.randomforest[, -ncol(da.randomforest)]), colu)

      }

      print(paste0("Yeh! The model witch AOGMC ", a," of ", id.specie, ", algorithm 'Random Forest', replica ",
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))


      ## 4. maxent

      Maxent <- maxent(train[, -1], train[, 1])
      da.maxent <- cbind(da.maxent, predict(brick(en.a.0), Maxent, progress = "text")[])

      colu <- paste0(id.specie, "_maxent_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p000")
      colnames(da.maxent) <- c(colnames(da.maxent[, -ncol(da.maxent)]), colu)

      eMaxent <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maxent)
      idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
      eval.Maxent.sp <- tibble::tibble(species = id.specie,
                                             replica = ifelse(r < 10, paste0("0", r), r),
                                             algorithm = "maxent",
                                             threshold = eMaxent@t[idMaxent],
                                             auc = eMaxent@auc,
                                             tss = eMaxent@TPR[idMaxent] + eMaxent@TNR[idMaxent] - 1,
                                             file = paste0(id.specie, "_maxent_", a, "_r", ifelse(r < 10, paste0("0", r), r)))
      eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)


      for(tempox in tempo.ini:tempo.fim){

        periodo = ifelse(tempox < 100, ifelse(tempox<10, paste0("00", tempox), paste0("0", tempox)), tempox)

        tempox.lbl = paste0(tempox, 'kyr')
        o.tempox = as.numeric(LR04[LR04$Time==tempox,c('d18O')])
        o.errox = as.numeric(LR04[LR04$Time==tempox,c('Error')])

        print(paste0(tempox.lbl,': ',o.tempox) )
        print('calculando...')

        # calculo
        var.tempox = en.a.0 - (((en.a.0 - en.a.21) * (o.tempo1 - o.tempox)) / (o.tempo1 - o.tempo2))
        print(paste0('projetando: ', tempox.lbl))

        da.maxent <- cbind(da.maxent, colu = values(predict(var.tempox, Maxent, progress = "text")))

        colu <- paste0(id.specie, "_maxent_", a, "_r", ifelse(r < 10, paste0("0", r), r),
                       "_p", periodo)
        colnames(da.maxent) <- c(colnames(da.maxent[, -ncol(da.maxent)]), colu)

      }

      print(paste0("Yeh! The model witch AOGMC ", a," of ", id.specie, ", algorithm 'Maxent', replica ",
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))


      ## 5. svm
      SVM <- ksvm(pb ~ ., data = train)
      da.svm <- cbind(da.svm, predict(brick(en.a.0), SVM, progress = "text")[])

      colu <- paste0(id.specie, "_svm_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p000")
      colnames(da.svm) <- c(colnames(da.svm[, -ncol(da.svm)]), colu)


      eSVM <- evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = SVM)
      idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
      eval.SVM.sp <- tibble::tibble(species = id.specie,
                                       replica = ifelse(r < 10, paste0("0", r), r),
                                       algorithm = "svm",
                                       threshold = eSVM@t[idSVM],
                                       auc = eSVM@auc,
                                       tss = eSVM@TPR[idSVM] + eSVM@TNR[idSVM] - 1,
                                       file = paste0(id.specie, "_svm_", a, "_r", ifelse(r < 10, paste0("0", r), r)))
      eval.SVM <- rbind(eval.SVM, eval.SVM.sp)


      for(tempox in tempo.ini:tempo.fim){

        periodo = ifelse(tempox < 100, ifelse(tempox<10, paste0("00", tempox), paste0("0", tempox)), tempox)

        tempox.lbl = paste0(tempox, 'kyr')
        o.tempox = as.numeric(LR04[LR04$Time==tempox,c('d18O')])
        o.errox = as.numeric(LR04[LR04$Time==tempox,c('Error')])

        print(paste0(tempox.lbl,': ',o.tempox) )
        print('calculando...')

        # calculo
        var.tempox = en.a.0 - (((en.a.0 - en.a.21) * (o.tempo1 - o.tempox)) / (o.tempo1 - o.tempo2))
        print(paste0('projetando: ', tempox.lbl))

        da.svm <- cbind(da.svm, colu = values(predict(var.tempox, SVM, progress = "text")))

        colu <- paste0(id.specie, "_svm_", a, "_r", ifelse(r < 10, paste0("0", r), r), "_p", periodo)
        colnames(da.svm) <- c(colnames(da.svm[, -ncol(da.svm)]), colu)

      }

      print(paste0("Yeh! The model witch AOGMC ", a," of ", id.specie, ", algorithm 'SVM', replica ",
                   ifelse(r < 10, paste0("0", r), r), " it's done!"))


    } # ends for "r"


    # evaluations
    write_csv(eval.Bioclim, paste0("eval_", "bioclim_", a, "_", id.specie, ".csv"))
    write_csv(eval.Gower, paste0("eval_", "gower_", a, "_", id.specie, ".csv"))
    write_csv(eval.RandomForest, paste0("eval_", "randomforest_", a, "_", id.specie, ".csv"))
    write_csv(eval.Maxent, paste0("eval_", "maxent_", a, "_", id.specie, ".csv"))
    write_csv(eval.SVM, paste0("eval_", "svm_", a, "_", id.specie, ".csv"))

  } # ends for  "a"

  write_csv(da.bioclim, paste0("enm_", "bioclim_", id.specie, ".csv"))
  write_csv(da.gower, paste0("enm_", "gower_", id.specie, ".csv"))
  write_csv(da.randomforest, paste0("enm_", "randomforest_", id.specie, ".csv"))
  write_csv(da.maxent, paste0("enm_", "maxent_", id.specie, ".csv"))
  write_csv(da.svm, paste0("enm_", "svm_", id.specie, ".csv"))


} # end i

###----------------------------------------------------------------------------###

da <- fread("enm_maxent_Adiantum calcareum.csv")
dim(da)

da.1 <- da[, c(1, 2, 3)]
colnames(da.1)
gridded(da.1) <- ~x + y
da.1.r <- raster(da.1)

plot(da.1.r)
