### script occ - pkg taxize - synonyms ###

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

# synonyms ----------------------------------------------------------------
# synonyms data
syn_data <- NULL

# synonyms
for(i in sp_list){
  
  # synonyms
  syn_i <- taxize::synonyms(x = i, db = "itis", accepted = TRUE)
  
  # conditional
  if(syn_i %>% taxize::synonyms_df() %>% nrow == 0){
    
    # data
    syn_data_i <- tibble::tibble(query = i, syn_name = NA)
    
  } else{
    
    # synonyms data
    syn_data_i <- syn_i %>% 
      taxize::synonyms_df() 
    
    # conditional
    if("message" %in% colnames(syn_data_i)){
      
      # no syn
      syn_data_i <- tibble::tibble(query = i, syn_name = "syns found")
      
    } else{
      
      # syn data
      syn_data_i <- syn_data_i %>% 
        dplyr::select(.id, syn_name) %>% 
        dplyr::rename(query = .id)
      
    }
    
  }
  
  # bind
  syn_data <- dplyr::bind_rows(syn_data, syn_data_i)
  
}

# view 
syn_data

# export ------------------------------------------------------------------
# synonyms
readr::write_csv(syn_data, "terretrial_animal_synonyms.csv")

# end ---------------------------------------------------------------------