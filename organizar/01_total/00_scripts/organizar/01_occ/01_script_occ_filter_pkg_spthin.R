# -------------------------------------------------------------------------
# occ - spthin
# mauricio vancine - mauricio.vancine@gmail.com
# 27-06-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(spThin)
library(tidyverse)

# informations
# https://cran.r-project.org/web/packages/spThin/vignettes/spThin_vignette.html

# directory
path <- "/home/mude/data/gitlab/r-enm/data/01_occ"
setwd(path)
dir.create("spthin")
setwd("spthin")

# data --------------------------------------------------------------------
data( Heteromys_anomalus_South_America )
head( Heteromys_anomalus_South_America )

table( Heteromys_anomalus_South_America$REGION )


# run thin - full dataset -------------------------------------------------
thinned_dataset_full <- spThin::thin(
  loc.data = Heteromys_anomalus_South_America, 
  lat.col = "LAT", 
  long.col = "LONG", 
  spec.col = "SPEC", 
  thin.par = 10, 
  reps = 100, 
  locs.thinned.list.return = TRUE, 
  write.files = TRUE, 
  max.files = 5, 
  out.dir = "hanomalus_thinned_full/", 
  out.base = "hanomalus_thinned", 
  write.log.file = TRUE,
  log.file = "hanomalus_thinned_full_log_file.txt"
)

str(thinned_dataset_full)

spThin::plotThin(thinned_dataset_full)

# run thin - by region ----------------------------------------------------
# Coastal mainland
spThin::thin(
  loc.data = Heteromys_anomalus_South_America[which( Heteromys_anomalus_South_America$REGION == "mainland"), ], 
  lat.col = "LAT", 
  long.col = "LONG", 
  spec.col = "SPEC", 
  thin.par = 10, 
  reps = 100, 
  locs.thinned.list.return = TRUE, 
  write.files = TRUE, 
  max.files = 5, 
  out.dir = "hanomalus_thinned_mainland/", 
  out.base = "hanomalus_thinned", 
  write.log.file = TRUE,
  log.file = "hanomalus_thinned_mainland_log_file.txt")

# Trinidad
spThin::thin( 
  loc.data = Heteromys_anomalus_South_America[which( Heteromys_anomalus_South_America$REGION == "trin" ), ], 
  lat.col = "LAT", long.col = "LONG", 
  spec.col = "SPEC", 
  thin.par = 10, 
  reps = 10, 
  locs.thinned.list.return = TRUE, 
  write.files = TRUE, 
  max.files = 5, 
  out.dir = "hanomalus_thinned_trin/", 
  out.base = "hanomalus_thinned", 
  write.log.file = TRUE,
  log.file = "hanomalus_thinned_trin_log_file.txt" )

# Margarita
thinned_dataset_mar <-
  spThin::thin( 
    loc.data = Heteromys_anomalus_South_America[which( Heteromys_anomalus_South_America$REGION == "mar"), ], 
    lat.col = "LAT", 
    long.col = "LONG", 
    spec.col = "SPEC", 
    thin.par = 10, 
    reps = 10, 
    locs.thinned.list.return = TRUE, 
    write.files = TRUE, 
    max.files = 5, 
    out.dir = "hanomalus_thinned_mar/", 
    out.base = "hanomalus_thinned", 
    write.log.file = TRUE,
    log.file = "hanomalus_thinned_mar_log_file.txt" )

# Tobago
spThin::thin(
  loc.data = Heteromys_anomalus_South_America[which(Heteromys_anomalus_South_America$REGION == "tobago"), ], 
  lat.col = "LAT", 
  long.col = "LONG", 
  spec.col = "SPEC", 
  thin.par = 10, 
  reps = 10, 
  locs.thinned.list.return = TRUE, 
  write.files = TRUE, 
  max.files = 5, 
  out.dir = "hanomalus_thinned_tobago/", 
  out.base = "hanomalus_thinned", 
  write.log.file = TRUE,
  log.file = "hanomalus_thinned_tobago_log_file.txt" )

# end ---------------------------------------------------------------------