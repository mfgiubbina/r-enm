# script description #
# script: occurences - download atlantic series
# author: mauricio vancine
# date:   06-05-2018

# atlantic series
# https://esajournals.onlinelibrary.wiley.com/doi/toc/10.1002/(ISSN)1939-9170.AtlanticPapers

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library("readxl")
library("tidyverse")

# directoty
path <- "/media/mude/Data/gitlab/r-enm/data/01_occ/"
setwd(path)
dir()

# directory
dir.create("atlantic_series")
setwd("atlantic_series")

# atlantic small mammals abu -------------------------------------------------------------
# article
# title: Abundance of small mammals in the Atlantic Forest (ASMAF): a data set for analyzing tropical community patterns
# https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecy.2005

# directory
dir.create("atlantic_small_mammals_abu")
setwd("atlantic_small_mammals_abu")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2005&file=ecy2005-sup-0002-MetadataS1.docx",
              "ecy2005-sup-0002-MetadataS1.docx")

# download data
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2005&file=ecy2005-sup-0001-DataS1.zip",
              "atlantic_small_mammals_abu.zip")

# unzip
unzip("atlantic_small_mammals_abu.zip")

# import sites
sm_ab_lo <- read.csv("Localities.csv", sep = ";") %>% 
  tibble::as_tibble() %>% 
  dplyr::select(SampleID, Latitude, Longitude, Study_year)
sm_ab_lo

# year
sm_ab_lo_yr <- stringr::str_split(sm_ab_lo$Study_year, "-")

sm_ab_lo_yr_fi <- NULL

for(i in sm_ab_lo_yr %>% length %>% seq){
  
  sm_ab_lo_yr_i <- sm_ab_lo_yr[[i]] %>% 
    paste0(collapse = "") %>% 
    stringr::str_sub(-4, -1) %>% 
    as.numeric()
  sm_ab_lo_yr_fi <- c(sm_ab_lo_yr_fi, sm_ab_lo_yr_i)
  
}

sm_ab_lo$Study_year <- sm_ab_lo_yr_fi

# import species
sm_ab_sp <- readr::read_csv2("Mammal_Communities.csv") %>% 
  dplyr::select(SampleID, Valid_Species)
sm_ab_sp

# join data
sm_ab <- dplyr::left_join(sm_ab_sp, sm_ab_lo, by = "SampleID") %>% 
  dplyr::rename() %>%
  dplyr::mutate(name = Valid_Species, 
                species = Valid_Species %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = Longitude, 
                latitude = Latitude, 
                year = Study_year,
                base = "atlantic_small_mammals_abu") %>% 
  dplyr::select(name, species, longitude, latitude, year, base)
sm_ab

# export data
setwd("..")
readr::write_csv(sm_ab, "atlantic_small_mammals_abu.csv")

# atlantic small mammals occ -------------------------------------------------------
# directory
dir.create("atlantic_small_mammals_occ")
setwd("atlantic_small_mammals_occ")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.1893&file=ecy1893-sup-0001-MetadataS1.docx",
              "ecy1893-sup-0001-MetadataS1.docx")

# download data
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.1893&file=ecy1893-sup-0002-DataS1.zip",
              "atlantic_small_mammals_occ.zip")

# unzip
unzip("atlantic_small_mammals_occ.zip")

# import sites
# need edit ATLANTIC_SM_Study_Site.csv: remove space from first column ID
# need fix line 69
sm_occ_lo <- readr::read_csv("ATLANTIC_SM_Study_Site.csv") %>% 
  dplyr::select(ID, Reference_number, Latitude, Longitude) %>% 
  dplyr::mutate(id = paste(ID, sub(" / ", "_", Reference_number), sep = "_")) %>% 
  dplyr::select(id, Latitude, Longitude)
sm_occ_lo

# import species
# need edit ATLANTIC_SM_Capture.csv: remove space from first column ID
sm_occ_sp <- read.csv("ATLANTIC_SM_Capture.csv") %>%
  tibble::as_tibble() %>% 
  dplyr::select(ID, Reference_number, Actual_species_name, Year_finish) %>% 
  dplyr::mutate(id = paste(ID, sub(" / ", "_", Reference_number), sep = "_")) %>% 
  dplyr::select(id, Actual_species_name, Year_finish)
sm_occ_sp

# join data
sm_occ <- dplyr::left_join(sm_occ_sp, sm_occ_lo, by = "id") %>% 
  dplyr:: select(Actual_species_name, Longitude, Latitude, Year_finish) %>%
  dplyr::mutate(name = Actual_species_name,
                species = Actual_species_name %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = as.numeric(Longitude), 
                latitude = as.numeric(Latitude), 
                year = Year_finish,
                base = "atlantic_small_mammals_occ") %>% 
  dplyr::select(name, species, longitude, latitude, year, base)
sm_occ

# export data
setwd("..")
readr::write_csv(sm_occ, "atlantic_small_mammals_occ.csv")

# atlantic bats -------------------------------------------------------
# directory
dir.create("atlantic_bats")
setwd("atlantic_bats")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2007&file=ecy2007-sup-0001-MetadataS1.docx",
              "ecy2007-sup-0001-MetadataS1.docx")

# download data
download.file("https://github.com/LEEClab/Atlantic_series/blob/master/ATLANTIC_BATS/DATASET/2018_02_d21/ATLANTIC_BATS_2019_comp.xlsx?raw=true",
              "ATLANTIC_BATS_2019_comp.xlsx")

# import data
ba_occ <- readxl::read_xlsx("ATLANTIC_BATS_2019_comp.xlsx") %>% 
  dplyr::select(1, 6, 7, 33:ncol(.))
ba_occ

# export data
setwd("..")
readr::write_csv(ba_occ, "atlantic_bats.csv")

# download data
# # download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2007&file=ecy2007-sup-0002-DataS1.zip",
# #               "atlantic_bats.zip")
# 
# # unzip
# # unzip("atlantic_bats.zip")
# 
# # import sites
# ba_lo <- readr::read_csv("ATLANTIC_BATS_Study_site.csv") %>%
#   dplyr::select(ID, Longitude, Latitude)
# ba_lo
# 
# # import species
# # need remove the '"' from file ATLANTIC_BATS_Capture.csv
# # need replace 'mist nets, active capture and harp trap' by 'mist nets active capture and harp trap' in ATLANTIC_BATS_Capture.csv
# # need replace 'lowland moist forests, with areas also covered by restinga and mangroves' by lowland moist forests with areas also covered by restinga and mangroves
# # need replace 'Capoeira, secondary forests, edges with surroundings including fields and periurban areas' by 'Capoeira secondary forests edges with surroundings including fields and periurban areas'
# # need replace 'Open fields, moist Chaco, Islands of Parana' by 'Open fields moist Chaco Islands of Parana'
# # need save the file as .csv
# ba_sp <- readr::read_csv("ATLANTIC_BATS_Capture_.csv") %>% 
#   dplyr::filter(Capture_number > 0) %>% 
#   dplyr::select(ID, Species, Year_finish) %>% 
#   dplyr::mutate(sp = str_replace(Species, "[.]", " ") %>% 
#                   stringr::str_trim("right") %>% 
#                   stringr::str_replace("sp ", "sp.")) %>% 
#   dplyr::select(ID, sp, Year_finish)
# ba_sp
# 
# # join data
# ba_occ <- dplyr::left_join(ba_sp, ba_lo, by = "ID") %>% 
#   dplyr::mutate(name = sp,
#                 species = sp %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
#                 longitude = as.numeric(Longitude), 
#                 latitude = as.numeric(Latitude), 
#                 year = Year_finish,
#                 base = "atlantic_bats") %>% 
#   dplyr::select(name, species, longitude, latitude, year, base)
# ba_occ


# atlantic large mammals --------------------------------------------------
# directory
dir.create("atlantic_large_mammals")
setwd("atlantic_large_mammals")

# import data
ma_la <- readr::read_csv("ATLANTIC_MAMMAL_MID_LARGE _assemblages_and_sites.csv") %>% 
  dplyr::mutate(name = Actual_species_Name,
                species = Actual_species_Name %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = as.numeric(Longitude), 
                latitude = as.numeric(Latitude), 
                year = Year_finish,
                base = "atlantic_large_mammals") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
ma_la

# export data
setwd("..")
readr::write_csv(ma_la, "atlantic_large_mammals.csv")

# atlantic camtrap -------------------------------------------------------
# directory
dir.create("atlantic_camtrap")
setwd("atlantic_camtrap")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.1998&file=ecy1998-sup-0002-MetadataS1.pdf",
              "ecy1998-sup-0002-MetadataS1.pdf")

# download data
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.1998&file=ecy1998-sup-0001-DataS1.zip",
              "atlantic_camtrap.zip")

# unzip
unzip("atlantic_camtrap.zip")

# import sites
ca_lo <- readr::read_csv("ATLANTIC_CAMTRAPS_1-0_LOCATION.csv") %>% 
  dplyr::select(location_id, X, Y)
ca_lo

# import sites names
ca_lo_na <- readr::read_csv("ATLANTIC_CAMTRAPS_1-0_SURVEY.csv") %>% 
  dplyr::select(location_id, survey_id, yearfinish)
ca_lo_na

# join site names
ca_lo <- dplyr::left_join(ca_lo_na, ca_lo, by = "location_id") %>% 
  dplyr::select(survey_id, X, Y, yearfinish)
ca_lo

# import species
ca_sp <- readr::read_csv("ATLANTIC_CAMTRAPS_1-0_RECORDS.csv") %>% 
  dplyr::select(survey_id, species_code, presence_absence) %>% 
  dplyr::filter(presence_absence == 1) %>% 
  dplyr::select(-presence_absence) 
ca_sp

# import species names
ca_sp_na <- readr::read_csv("ATLANTIC_CAMTRAPS_1-0_SPECIES.csv") %>% 
  dplyr::select(species_name, species_code)
ca_sp_na

# join data specie
ca_sp <- dplyr::left_join(ca_sp, ca_sp_na, by = "species_code") %>% 
  dplyr::select(survey_id, species_name)
ca_sp

# join data
ca_occ <- dplyr::left_join(ca_sp, ca_lo, by = "survey_id") %>% 
  dplyr::mutate(name = species_name,
                species = species_name %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = as.numeric(X), 
                latitude = as.numeric(Y), 
                year = yearfinish,
                base = "atlantic_camtrap") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
ca_occ

# export data
setwd("..")
readr::write_csv(ca_occ, "atlantic_camtrap.csv")

# atlantic primates -------------------------------------------------------
# directory
dir.create("atlantic_primates")
setwd("atlantic_primates")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2525&file=ecy2525-sup-0002-MetadataS1.pdf",
              "ecy2525-sup-0002-MetadataS1.pdf")

# download data
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2525&file=ecy2525-sup-0001-DataS1.zip",
              "atlantic_primates.zip")

# unzip
unzip("atlantic_primates.zip")

# import communities
pr_co <- read.csv("Dataset/ATLANTIC-PR_Community.csv", sep = ";") %>%
  tibble::as_tibble() %>% 
  dplyr::select(SPECIES, LONGITUDE_X, LATITUDE_Y, COL_END_YR)
pr_co

# import quantitative
pr_qu <- read.csv("Dataset/ATLANTIC-PR_Quantitative.csv", sep = ";") %>%
  tibble::as_tibble() %>% 
  dplyr::select(SPECIES, LONGITUDE_X, LATITUDE_Y, COL_END_YR)
pr_qu

# import occurrences
pr_oc <- read.csv("Dataset/ATLANTIC-PR_Occurrence.csv", sep = ";") %>%
  tibble::as_tibble() %>% 
  dplyr::select(SPECIES, LONGITUDE_X, LATITUDE_Y, COL_END_YR)
pr_oc

# bind data
pr_occ <- dplyr::bind_rows(pr_co, pr_qu, pr_oc) %>% 
  dplyr::mutate(name = SPECIES,
                species = SPECIES %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = as.numeric(LONGITUDE_X), 
                latitude = as.numeric(LATITUDE_Y), 
                year = COL_END_YR,
                base = "atlantic_primates") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
pr_occ

# export data
setwd("..")
readr::write_csv(pr_occ, "atlantic_primates.csv")

# atlantic non‐volant mammals ---------------------------------------------
# directory
dir.create("atlantic_non_volant_mammals")
setwd("atlantic_non_volant_mammals")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2107&file=ecy2107-sup-0001-MetadataS1.pdf",
              "ecy2107-sup-0001-MetadataS1.pdf")

# download data
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2107&file=ecy2107-sup-0002-DataS1.zip",
              "atlantic_non_volant_mammals.zip")

# unzip
unzip("atlantic_non_volant_mammals.zip")

# import sites
ma_non_lo <- readr::read_csv("Mammals_UPRB_study_sites.csv") %>% 
  dplyr::select(site, longitude, latitude, year_finish) %>% 
  dplyr::mutate(site = paste0("site_", 1:52))
ma_non_lo

# import species
ma_non_sp <- readr::read_csv("Mammals_UPRB_species.csv") %>% 
  dplyr::select(species:site_52) %>% 
  tidyr::gather(key = site, value = pres, -species) %>% 
  dplyr::filter(pres == 1) %>% 
  dplyr::select(2:1)
ma_non_sp

# join data
ma_non_occ <- dplyr::left_join(ma_non_sp, ma_non_lo, by = "site") %>% 
  dplyr::mutate(name = species,
                species = species %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                year = year_finish,
                base = "atlantic_non_volant_mammals") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
ma_non_occ

# export data
setwd("..")
readr::write_csv(ma_non_occ, "atlantic_non_volant_mammals.csv")

# atlantic mammals traits -------------------------------------------------
# directory
dir.create("atlantic_mammals_traits")
setwd("atlantic_mammals_traits")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2106&file=ecy2106-sup-0001-MetadataS1.pdf",
              "ecy2106-sup-0001-MetadataS1.pdf")

# download data
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2106&file=ecy2106-sup-0002-DataS1.zip",
              "atlantic_mammals_traits.zip")

# unzip
unzip("atlantic_mammals_traits.zip")

# import data
ma_tr_occ <- readr::read_csv("ATLANTIC_TR_all_data.csv") %>% 
  dplyr::mutate(name = binomial,
                species = binomial %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                base = "atlantic_mammals_traits") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
ma_tr_occ

# export data
setwd("..")
readr::write_csv(ma_tr_occ, "atlantic_mammals_traits.csv")

# atlantic frugyvory ------------------------------------------------------
# directory
dir.create("atlantic_frugyvory")
setwd("atlantic_frugyvory")

# download
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.1818&file=ecy1818-sup-0001-MetadataS1.pdf",
              "ecy1818-sup-0001-MetadataS1.pdf")

# download data
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.1818&file=ecy1818-sup-0002-DataS1.zip",
              "atlantic_frugyvory.zip")

# unzip
unzip("atlantic_frugyvory.zip")

# import data
fu_occ <- readr::read_csv("ATLANTIC_frugivory.csv") %>% 
  dplyr::mutate(name = Frugivore_Species,
                species = Frugivore_Species %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = Longitude, 
                latitude = Latitude,
                year = NA,
                base = "atlantic_frugyvory") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
fu_occ

# export data
setwd("..")
readr::write_csv(fu_occ, "atlantic_frugyvory.csv")

# atlantic birds ----------------------------------------------------------
# directory
dir.create("atlantic_birds")
setwd("atlantic_birds")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2119&file=ecy2119-sup-0001-MetadataS1.pdf",
              "ecy2119-sup-0001-MetadataS1.pdf")

# download data
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2119&file=ecy2119-sup-0002-DataS1.zip",
              "atlantic_birds.zip")

# unzip
unzip("atlantic_birds.zip")

# import quantitative data
bi_quan <- read_csv("DataS1/DataS1/ATLANTIC_BIRDS_quantitative.csv") %>%
  dplyr::select(Species, Longitude_x, Latitude_y, Year_finish)
bi_quan

# import qualitative data
bi_qual <- read_csv("DataS1/DataS1/ATLANTIC_BIRDS_qualitative.csv") %>%
  dplyr::select(Species, Longitude_x, Latitude_y, Year)
bi_qual

# bind data
bi_occ <- dplyr::bind_rows(bi_quan, bi_qual) %>% 
  dplyr::mutate(name = Species,
                species = Species %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = Longitude_x, 
                latitude = Latitude_y,
                year = Year,
                base = "atlantic_birds") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
bi_occ

# export data
setwd("..")
readr::write_csv(bi_occ, "atlantic_birds.csv")

# atlantic traits birds ---------------------------------------------------
# directory
dir.create("atlantic_traits_birds")
setwd("atlantic_traits_birds")

# download data
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2647&file=ecy2647-sup-0001-DataS1.zip",
              "atlantic_traits_birds.zip")

# unzip
unzip("atlantic_traits_birds.zip")

# import data
bi_tr_occ <- readr::read_csv("ATLANTIC_BIRD_TRAITS_completed_2018_11_d05.csv") %>% 
  dplyr::mutate(name = Binomial,
                species = Binomial %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = Longitude_decimal_degrees, 
                latitude = Latitude_decimal_degrees,
                year = Year,
                base = "atlantic_traits_birds") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
bi_tr_occ

# export data
setwd("..")
readr::write_csv(bi_tr_occ, "atlantic_traits_birds.csv")

# atlantic amphibians -----------------------------------------------------
# directory
dir.create("atlantic_amphibians")
setwd("atlantic_amphibians")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2392&file=ecy2392-sup-0002-MetadataS1.pdf",
              "ecy2392-sup-0002-MetadataS1.pdf")

# download
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2392&file=ecy2392-sup-0001-DataS1.zip",
              "atlantic_amphibians.zip")

# unzip
unzip("atlantic_amphibians.zip")

# import sites
am_si <- readr::read_csv("ATLANTIC_AMPHIBIANS_sites.csv") %>%
  dplyr::select(id, longitude, latitude, year_finish)
am_si

# import species
am_sp <- readr::read_csv("ATLANTIC_AMPHIBIANS_species.csv") %>%
  dplyr::select(id, valid_name) %>% 
  na.omit
am_sp

# join data
am_occ <- dplyr::left_join(am_sp, am_si, by = "id") %>% 
  dplyr::mutate(name = valid_name,
                species = valid_name %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                year = year_finish,
                base = "atlantic_amphibians") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
am_occ

# export data
setwd("..")
readr::write_csv(am_occ, "atlantic_amphibians.csv")

# atlantic butterflies ----------------------------------------------------
# directory
dir.create("atlantic_butterflies")
setwd("atlantic_butterflies")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2507&file=ecy2507-sup-0002-MetadataS1.pdf",
              "ecy2507-sup-0002-MetadataS1.pdf")

# download
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2507&file=ecy2507-sup-0001-DataS1.zip",
              "atlantic_butterflies.zip")

# unzip
unzip("atlantic_butterflies.zip")

# import sites
bu_si <- read.csv("ATLANTIC_BUTTERFLIES_sites.csv", sep = ";") %>%
  tibble::as_tibble() %>% 
  dplyr::select(sites_ID, Longitude, Latitude)
bu_si

# import species
bu_sp <- read.csv("ATLANTIC_BUTTERFLIES_species.csv", sep = ";") %>%
  tibble::as_tibble() %>% 
  dplyr::select(sites_ID, Species)
bu_sp

# join data
bu_occ <- dplyr::left_join(bu_sp, bu_si, by = "sites_ID") %>% 
  dplyr::mutate(name = Species,
                species = Species %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = Longitude, 
                latitude = Latitude,
                year = NA,
                base = "atlantic_butterflies") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
bu_occ

# export data
setwd("..")
readr::write_csv(bu_occ, "atlantic_butterflies.csv")

# atlantic epiphytes ------------------------------------------------------
# directory
dir.create("atlantic_epiphytes")
setwd("atlantic_epiphytes")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2541&file=ecy2541-sup-0001-MetadataS1.pdf",
              "ecy2541-sup-0001-MetadataS1.pdf")

# download
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2541&file=ecy2541-sup-0002-DataS1.zip",
              "atlantic_epiphytes.zip")

# unzip
unzip("atlantic_epiphytes.zip")

# import sites
ep_ab <- read.table("DataS1_Abundance.txt", header = TRUE, sep = "\t") %>%
  tibble::as_tibble() %>% 
  dplyr::select(EPIPHYTE_SPECIES, LONGITUDE_X, LATITUDE_Y, YEAR_FINISH)
ep_ab

ep_oc <- read.table("DataS1_Occurrence.txt", header = TRUE, sep = "\t") %>%
  tibble::as_tibble() %>%
  dplyr::select(EPIPHYTE_SPECIES, LONGITUDE_X, LATITUDE_Y, YEAR_FINISH) %>% 
  dplyr::mutate(YEAR_FINISH = YEAR_FINISH %>% as.character %>% as.numeric)
ep_oc

# integrate
ep_occ <- dplyr::bind_rows(ep_ab, ep_oc) %>% 
  dplyr::mutate(name = EPIPHYTE_SPECIES,
                species = EPIPHYTE_SPECIES %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = LONGITUDE_X, 
                latitude = LATITUDE_Y,
                year = YEAR_FINISH,
                base = "atlantic_epiphytes") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
ep_occ

# export data
setwd("..")
readr::write_csv(ep_occ, "atlantic_epiphytes.csv")

# jaguar movement database ------------------------------------------------
# directory
dir.create("jaguar_moviment")
setwd("jaguar_moviment")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2379&file=ecy2379-sup-0002-MetadataS1.pdf",
              "ecy2379-sup-0002-MetadataS1.pdf")

# download data
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2379&file=ecy2379-sup-0001-DataS1.zip",
              "jaguar_moviments.zip")

# unzip
unzip("jaguar_moviments.zip")

# import sites
ja_occ <- readr::read_csv("jaguar_movement_data.csv") %>% 
  dplyr::mutate(name = individual.taxon.canonical.name,
                species = individual.taxon.canonical.name %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = location.long, 
                latitude = location.lat,
                year = lubridate::mdy_hm(timestamp) %>% lubridate::year(),
                base = "jaguar_movement") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
ja_occ

# export data
setwd("..")
readr::write_csv(ja_occ, "jaguar_moviment_data.csv")

# brazil road kill --------------------------------------------------------
# directory
dir.create("brazil_road_kill")
setwd("brazil_road_kill")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2464&file=ecy2464-sup-0002-MetadataS1.pdf",
              "ecy2464-sup-0002-MetadataS1.pdf")

# download
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2464&file=ecy2464-sup-0001-DataS1.zip",
              "brazil_road_kill.zip")

# unzip
unzip("brazil_road_kill.zip")

# import sites
br_road_kill_occ <- readr::read_csv("Brazil_Roadkill_20180527.csv") %>%
  dplyr::mutate(name = Scientific_name,
                species = Scientific_name %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = Long, 
                latitude = Lat,
                year = Year,
                base = "brazil_road_kill") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
br_road_kill_occ

# export data
setwd("..")
readr::write_csv(br_road_kill_occ, "brazil_road_kill.csv")

# neotropical xenarthrans -------------------------------------------------
# directory
dir.create("neotropical_xenarthrans")
setwd("neotropical_xenarthrans")

# download metadata
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2663&file=ecy2663-sup-0002-MetadataS1.pdf",
              "ecy2663-sup-0002-MetadataS1.pdf")

# download data
download.file("https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2663&file=ecy2663-sup-0001-DataS1.rar",
              "neotropical_xenarthrans.rar")

# unrar
dir()

# import
neo_xen_qual <- readr::read_csv("NEOTROPICAL_XENARTHRANS_QUALITATIVE.csv") %>%
  dplyr::select(SPECIES, LONG_X, LAT_Y)
neo_xen_qual

neo_xen_quan <- readr::read_csv("NEOTROPICAL_XENARTHRANS_QUANTITATIVE.csv") %>%
  dplyr::select(SPECIES, LONG_X, LAT_Y)
neo_xen_quan

# bind data
neo_xen <- dplyr::bind_rows(neo_xen_qual, neo_xen_quan) %>% 
  dplyr::mutate(name = SPECIES,
                species = SPECIES %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"), 
                longitude = LONG_X, 
                latitude = LAT_Y,
                year = NA,
                base = "neotropical_xenarthrans") %>%
  dplyr::select(name, species, longitude, latitude, year, base)
neo_xen

# export data
setwd("..")
readr::write_csv(neo_xen, "neotropical_xenarthrans.csv")

# end ---------------------------------------------------------------------