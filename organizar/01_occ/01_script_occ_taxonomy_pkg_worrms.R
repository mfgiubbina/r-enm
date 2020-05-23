### script enm - occurrence - pkg worrms ###

# mauricio vancine
# 15-04-2019

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(worrms)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ/00_species_list"
setwd(path)
dir()

# import data -------------------------------------------------------------
# species list
sp_list <- readr::read_csv("lista_especies_marinhas_cetaceos_quelonios.csv") %>% 
  dplyr::select(species) %>% 
  dplyr::arrange(species) %>% 
  dplyr::pull()
sp_list

# taxonomy ----------------------------------------------------------------
# complete taxonomy
tax_worrms <- worrms::wm_records_names(name = sp_list) %>% 
  do.call("rbind", .)
tax_worrms

# end ---------------------------------------------------------------------