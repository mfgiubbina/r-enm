# script description #
# script: occurences - integrate spocc
# package: spocc
# author:   mauricio vancine
# date:     06-05-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ"
setwd(path)
dir()

# spocc -------------------------------------------------------------------
# directory
setwd("spocc")
dir()

# occ
occ_spocc <- purrr::map_dfr(dir(pattern = ".csv"), readr::read_csv)
occ_spocc

# export
readr::write_csv(occ_spocc, "occ_spocc.csv")

# end ---------------------------------------------------------------------