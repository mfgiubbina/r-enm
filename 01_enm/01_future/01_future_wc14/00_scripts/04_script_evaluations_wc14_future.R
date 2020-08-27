#' ---
#' title: evaluation
#' authors: mauricio vancine
#' date: 2020-06-21
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(tidyverse)
library(wesanderson)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/01_future/01_future_wc14"
setwd(path)
dir()

# import evaluates --------------------------------------------------------
# directory
setwd("04_evaluations")

# import evaluations
eva <- dir(pattern = "00_eval_table_", recursive = TRUE) %>% 
  purrr::map_dfr(., col_types = cols(), readr::read_csv)
eva

# evaluation --------------------------------------------------------------
for(i in eva$species %>% unique){
  
  # information
  print(paste("Evaluation to", i))
  
  # directory
  setwd(path); setwd(paste0("04_evaluations/", i))
  
  # table
  eva_table <- eva %>% 
    dplyr::filter(species == i) %>% 
    dplyr::mutate(species = species %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")) %>% 
    dplyr::group_by(species, method) %>% 
    dplyr::summarise(tss_mean = mean(tss_spec_sens) %>% round(3), 
                     tss_sd = sd(tss_spec_sens) %>% round(3),
                     auc_mean = mean(auc) %>% round(3), 
                     auc_sd = sd(auc) %>% round(3))
  eva_table
  
  # export
  readr::write_csv(eva_table, paste0("02_eval_table_summary_", i, ".csv"))
  
  # boxplots
  for(j in c("tss_spec_sens", "auc")){
    
    # information
    print(paste(i, j))
    
    # plot  
    eva %>% 
      dplyr::filter(species == i) %>% 
      ggplot() + 
      aes_string(x = "method", y = j, color = "method") +
      geom_boxplot(size = .5, fill = "gray90", color = "black") +
      geom_jitter(width = 0.2, size = 4, alpha = .7) +
      scale_color_manual(values = wesanderson::wes_palette(name = "Darjeeling1", n = eva$method %>% unique %>% length, 
                                                           type = "continuous")) +
      labs(x = "Methods", 
           y = stringr::str_to_upper(j) %>% stringr::str_replace("_", " "), 
           title = i %>% stringr::str_to_title() %>% stringr::str_replace_all("_", " ")) + 
      ylim(c(-.01, 1.05)) + 
      theme_bw() +
      geom_hline(yintercept = ifelse(j == "tss_spec_sens", .5, .75), color = "red") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold.italic", size = 20), 
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 15), 
            axis.title = element_text(size = 17))
    ggsave(paste0("03_eval_plot_meth_", j, "_", i, ".png"), he = 15, wi = 20, un = "cm", dpi = 300)
    
  }
  
}

# end ---------------------------------------------------------------------