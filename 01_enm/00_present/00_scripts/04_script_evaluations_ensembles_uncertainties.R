#' ---
#' title: ensemble - weighted average and uncertainties - hierarchical anova
#' authors: mauricio vancine
#' date: 2020-05-09
#' ---

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(raster)
library(tidyverse)
library(vegan)
library(wesanderson)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
raster::beginCluster(n = 2)

# directory
path <- "/home/mude/data/github/r-enm/01_enm/00_present"
setwd(path)
dir()

# import evaluates --------------------------------------------------------
# directory
setwd("04_evaluation")

# import evaluations
eva <- dir(pattern = "00_evaluation_", recursive = TRUE) %>% 
  purrr::map_dfr(., readr::read_csv)
eva

# weighted average ensemble  ----------------------------------------------
# tss
tss_limit <- .5

# directories
setwd(path); dir.create("05_ensembles_uncertainties")

# ensemble
for(i in eva$species %>% unique){
  
  # information
  print(paste("Evaluation to", i))
  
  # directory
  setwd(path); setwd(paste0("04_evaluation/", i))
  
  # table
  eva_table <- eva %>% 
    dplyr::filter(species == i) %>% 
    dplyr::mutate(species = species %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")) %>% 
    dplyr::group_by(species, algorithm) %>% 
    dplyr::summarise(tss_mean = mean(tss_spec_sens) %>% round(3), 
                     tss_sd = sd(tss_spec_sens) %>% round(3),
                     auc_mean = mean(auc) %>% round(3), 
                     auc_sd = sd(auc) %>% round(3))
  eva_table
  
  # export
  readr::write_csv(eva_table, paste0("01_evaluation_summary_table_", i, ".csv"))
  
  # boxplots
  for(j in c("tss_spec_sens", "auc")){
    
    # information
    print(paste(i, j))
    
    # plot  
    eva %>% 
      dplyr::filter(species == i) %>% 
      ggplot() + 
      aes_string(x = "algorithm", y = j, color = "algorithm") +
      geom_boxplot(size = .5, fill = "gray90", color = "black") +
      geom_jitter(width = 0.2, size = 4, alpha = .7) +
      scale_color_manual(values = wesanderson::wes_palette(name = "Darjeeling1", n = eva$algorithm %>% unique %>% length, 
                                                           type = "continuous")) +
      labs(x = "Algorithms", 
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
    ggsave(paste0("02_boxplot_jitter_", j, "_", i, ".png"), he = 15, wi = 20, un = "cm", dpi = 300)
    
  }

  # ensemble ----------------------------------------------------------------
  # information
  print(paste("Ensemble to", i))
  
  # selection
  eva_i <- eva %>% 
    dplyr::filter(species == i, 
                  tss_spec_sens >= tss_limit)
  
  # tss
  tss_i <- eva_i %>% 
    dplyr::select(tss_spec_sens) %>% 
    dplyr::mutate(tss = (tss_spec_sens) ^ 2) %>% 
    dplyr::pull()
  
  # list algorithms
  alg <- eva_i$algorithm
  alg
  
  # list files
  enm_i_f <- eva_i %>% 
    dplyr::select(file) %>% 
    dplyr::pull()
  
  # directory
  setwd(path); setwd(paste0("03_enm/", i))
  
  # import
  enm_i_r <- dir(pattern = ".tif$") %>% 
    stringr::str_subset(paste(enm_i_f, collapse = "|")) %>% 
    raster::stack()
  
  
  # standardization ---------------------------------------------------------
  print("Standardization can take a looong time...")
  
  enm_i_st <- NULL
  
  for(j in alg %>% unique){
    
    # information
    print(j)
    
    # algoritm selection
    enm_i_r_alg_val <- enm_i_r[[grep(j, names(enm_i_r))]] %>% 
      raster::values()
    
    # standardization
    enm_i_alg_st <- vegan::decostand(enm_i_r_alg_val, "range", na.rm = TRUE)
    enm_i_st <- cbind(enm_i_st, enm_i_alg_st)
    
  }
  
  # weighted average ensemble -----------------------------------------------
  # information
  print(paste("Weighted average ensemble to ", i))
  
  # weighted average ensemble
  ens <- enm_i_r[[1]]
  ens[] <- apply(enm_i_st, 1, function(x){sum(x * tss_i) / sum(tss_i)})
  
  # directory
  setwd(path); setwd("05_ensembles_uncertainties"); dir.create(i); setwd(i)
  
  # export
  raster::writeRaster(x = ens, 
                      filename = paste0("ensemble_", i), 
                      format = "GTiff", 
                      options = c("COMPRESS=DEFLATE"), 
                      progress = "text",
                      overwrite = TRUE)
  
  
  # uncertainties - hierarchical anova --------------------------------------
  # information
  print(paste("Uncertainties to", i))
  
  # suitability
  sui <- enm_i_st
  
  # hierarchical anova
  sui_ms <- NULL
  
  for(p in 1:nrow(sui)){
    
    sui_p <- as.numeric(sui[p, ])

    if(any(is.na(sui_p))){
      
      sui_ms <- rbind(sui_ms, rep(NA, 2))
      
    } else{
      
        lm_model <- lm(sui_p ~ alg)
        anova_model <- anova(lm_model)
        sui_ms <- rbind(sui_ms, anova_model$"Mean Sq")
    }
    
  }
  
  # column names
  nrow(sui_ms)
  colnames(sui_ms) <- c("algorithm", "residuals")
  
  # proportion of variance explained by each component
  sui_ms_sum <- apply(sui_ms, 1, sum)
  sui_ms_prop <- sui_ms/sui_ms_sum
  
  # mapping uncertainties
  unc_r <- enm_i_r[[1]]
  unc <- raster::stack()
  
  for(r in 1:ncol(sui_ms_prop)){

      unc_r[] <- sui_ms_prop[, r]
      unc <- raster::stack(unc, unc_r)
  }
  
  # names
  names(unc) <- c("algorithm", "residuals")
  
  # table
  table_resume <- sui_ms_prop %>% 
    tibble::as_tibble() %>% 
    tidyr::drop_na() %>%
    tidyr::pivot_longer(cols = c("algorithm", "residuals"), names_to = "source", values_to = "values") %>% 
    dplyr::group_by(source) %>% 
    dplyr::summarise(median = median(values) %>% round(3),
                     min = min(values) %>% round(3),
                     max = max(values) %>% round(3))

  readr::write_csv(table_resume, paste0("00_total_sum_squares_anova_", i, ".csv"))
  
  # export
  raster::writeRaster(x = unc, 
                      filename = paste0("uncertainties_", names(unc), "_", i), 
                      bylayer = TRUE,
                      format = "GTiff", 
                      options = c("COMPRESS=DEFLATE"), 
                      progress = "text",
                      overwrite = TRUE)
  
  print("All right, finish!")
  
  # combine
  da_sui_unc <- tibble::tibble(sui = ens[], unc = unc$algorithm[])
  
  # uncertainties and suitability
  ggplot(data = da_sui_unc) +
    aes(x = sui, y = unc * 100) +
    stat_density2d(aes(alpha = ..level.., fill = ..level..), size = 2, bins = 10, geom = "polygon") + 
    scale_fill_gradient(low = "yellow", high = "red") +
    scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
    geom_point(color = "black", size = .5, alpha = .5, pch = 20) +
    geom_density2d(bins = 10) +
    labs(x = "Suitability", y = "Algorithms uncertainties (%)") +
    theme_bw() +
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 12))
  ggsave(paste0("01_uncertainties_and_suitability_", i, ".png"), wi = 25, he = 20, un = "cm", dpi = 300)
  
} 

# end ---------------------------------------------------------------------