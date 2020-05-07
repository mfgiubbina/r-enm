### script enm - dismo ###

## boxplot evaluate ##

# mauricio vancine
# 18-03-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(tidyverse)
library(wesanderson)

# directory
setwd("/media/mauricio/data/000_trabalho/00_empresas/aquaflora/05_distribuicao_especies/04_modelos/02_evaluation")
dir()


# import data -------------------------------------------------------------
# evaluate animals
setwd("animais")
dir()
eva.an <- purrr::map_df(dir(), readr::read_csv)
eva.an

# evaluate plants
setwd("..")
setwd("plantas")
dir()
eva.pl <- purrr::map_df(dir(), readr::read_csv)
eva.pl

# tables ------------------------------------------------------------------

# directry
setwd("..")
dir.create("tables")
setwd("tables")

# animals
ta.an.al <- eva.an %>% 
  dplyr::mutate(species = species %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")) %>% 
  dplyr::group_by(species, algorithm) %>% 
  dplyr::summarise(tss_mean = mean(tss) %>% round(3), 
                   tss_sd = sd(tss) %>% round(3))
ta.an.al

ta.an <- eva.an %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(tss_mean = mean(tss) %>% round(3), 
                   tss_sd = sd(tss) %>% round(3),
                   auc_mean = mean(auc) %>% round(3), 
                   auc_sd = sd(auc) %>% round(3))
ta.an

readr::write_csv(ta.an.al, "table_eval_an_alg.csv")
readr::write_csv(ta.an, "table_eval_an.csv")


# plants
ta.pl.al <- eva.pl %>% 
  dplyr::mutate(species = species %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")) %>% 
  dplyr::group_by(species, algorithm) %>% 
  dplyr::summarise(tss_mean = mean(tss) %>% round(3), 
                   tss_sd = sd(tss) %>% round(3),)
ta.pl.al

ta.pl <- eva.pl %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(tss_mean = mean(tss) %>% round(3), 
                   tss_sd = sd(tss) %>% round(3),
                   auc_mean = mean(auc) %>% round(3), 
                   auc_sd = sd(auc) %>% round(3))
ta.pl

readr::write_csv(ta.pl.al, "table_eval_pl_alg.csv")
readr::write_csv(ta.pl, "table_eval_pl.csv")


# plot --------------------------------------------------------------------

## animals
# species
sp.an <- eva.an$species %>% unique
sp.an

# directory
setwd("..")
dir.create("plots_animals")
setwd("plots_animals")

# plot
for(i in sp.an){
  
  # select specie
  eva.an.sp <- eva.an %>% 
    dplyr::filter(species == i)
  
  # auc - tss
  for(j in c("tss", "auc")){
  
    # information
    print(paste(i, j))
    
    # plot  
    ggplot(data = eva.an.sp) + 
      aes_string(x = "algorithm", y = j, color = "algorithm") +
      geom_boxplot(size = .5, fill = "gray90", color = "black") +
      geom_jitter(width = 0.2, size = 4, alpha = .7) +
      scale_color_manual(values = wesanderson::wes_palette(name = "Darjeeling1", n = eva.an$algorithm %>% unique %>% length, type = "discrete")) +
      labs(x = "Algoritmos", y = stringr::str_to_upper(j), title = i %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")) + 
      ylim(c(0, 1)) + 
      theme_bw() +
      geom_hline(yintercept = ifelse(j == "tss", .5, .7), color = "red") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold.italic", size = 20), 
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 15), 
            axis.title = element_text(size = 17))
    ggsave(paste0("boxplot_jitter_an_", j, "_", i, ".tiff"), he = 15, wi = 15, un = "cm", dpi = 300)
  
  }
  
}

## plants
# species
sp.pl <- eva.pl$species %>% unique
sp.pl

# directory
setwd("..")
dir.create("plots_plants")
setwd("plots_plants")

# plot
for(i in sp.pl){
  
  # select specie
  eva.pl.sp <- eva.pl %>% 
    dplyr::filter(species == i)
  
  # plot
  for(j in c("tss", "auc")){
    
    # information
    print(paste(i, j))
    
    ggplot(data = eva.pl.sp) + 
      aes_string(x = "algorithm", y = j, color = "algorithm") +
      geom_boxplot(size = .5, fill = "gray90", color = "black") +
      geom_jitter(width = 0.2, size = 4, alpha = .7) +
      scale_color_manual(values = wesanderson::wes_palette(name = "Darjeeling1", n = eva.an$algorithm %>% unique %>% length, type = "discrete")) +
      labs(x = "Algoritmos", y = stringr::str_to_upper(j), title = i %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")) + 
      ylim(c(0, 1)) + 
      theme_bw() +
      geom_hline(yintercept = ifelse(j == "tss", .5, .7), color = "red") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold.italic", size = 20), 
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 15), 
            axis.title = element_text(size = 17))
    ggsave(paste0("boxplot_jitter_pl_", j, "_", i, ".tiff"), he = 15, wi = 15, un = "cm", dpi = 300)
    
  }
  
}


# end ---------------------------------------------------------------------