### script occ - pkg taxize - classification ###

# mauricio vancine
# 23-04-2019

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(taxize)
library(tidyverse)

# directory
path <- "/media/mude/data/gitlab/r-enm/data/01_occ"
setwd(path)
dir()

# import data -------------------------------------------------------------
# species list
sp_list <- readr::read_csv("terretrial_animal_species_list.csv") %>% 
  dplyr::select(species) %>% 
  dplyr::arrange(species) %>% 
  dplyr::pull()
sp_list

# classification ----------------------------------------------------------
# taxonomy
tax_itis <- taxize::classification(sp_list, db = "itis", return_id = FALSE) %>% 
  rbind %>%  
  tidyr::spread(key = rank, value = name) %>% 
  dplyr::select(query, kingdom, phylum, class, order, family, genus, species) %>% 
  dplyr::mutate(db = "itis")
tax_itis

tax_bold <- taxize::classification(sp_list, db = "bold", return_id = FALSE) %>% 
  rbind %>% 
  dplyr::filter(rank %in% c("phylum", "class", "order", "family", "genus", "species")) %>% 
  dplyr::select(query, name, rank) %>% 
  tidyr::spread(key = rank, value = name) %>% 
  dplyr::mutate(kingdom = "Animalia", db = "bold") %>% 
  dplyr::select(query, kingdom, phylum, class, order, family, genus, species, db)
tax_bold

# bind data
tax <- dplyr::bind_rows(tax_itis, tax_bold) %>% 
  dplyr::distinct(query, kingdom, phylum, class, order, family, genus, species, .keep_all = TRUE) %>% 
  dplyr::arrange(query)
tax

# export ------------------------------------------------------------------
# taxonomy
readr::write_csv(tax_itis, "terretrial_animal_taxonomic_classification.csv")

# end ---------------------------------------------------------------------