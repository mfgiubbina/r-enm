# -------------------------------------------------------------------------
# sdm maxent - kuenm

# mauricio vancine
# 16-10-2019

# packages
devtools::install_github("marlonecobos/kuenm")
library(kuenm)

# directory
setwd("/home/mude/data/gitlab/r-enm/data/kuenm")

# data
download.file(url = "https://kuscholarworks.ku.edu/bitstream/handle/1808/26376/ku.enm_example_data.zip?sequence=3&isAllowed=y", 
              destfile = "ku.enm_example_data.zip", mode = "wb",
              quiet = FALSE) # donwload the zipped example folder in documents

unzip(zipfile = "ku.enm_example_data.zip") # unzip the example folder in documents
unlink("ku.enm_example_data.zip") # erase zip file

setwd("ku.enm_example_data/A_americanum") # set the working directory
dir() # check what is in your working directory

# Preparing variables to be used in arguments
file_name <- "aame_enm_process"
file_name

kuenm_start(file.name = file_name)

# Calibration of models
# Variables with information to be used as arguments. Change "YOUR/DIRECTORY" by your actual directory.
occ_joint <- "aame_joint.csv"
occ_tra <- "aame_train.csv"
M_var_dir <- "M_variables"
batch_cal <- "Candidate_models"
out_dir <- "Candidate_Models"
reg_mult <- c(seq(0.1, 1, 0.1), seq(2, 6, 1), 8, 10)
f_clas <- "all"
args <- NULL # e.g., "maximumbackground=20000" for increasing the number of pixels in the bacground or
# note that some arguments are fixed in the function and should not be changed
maxent_path <- "/home/mude/data/gitlab/r-enm/data/kuenm/ku.enm_example_data/A_americanum"
wait <- FALSE
run <- TRUE

kuenm_cal(occ.joint = occ_joint, 
          occ.tra = occ_tra, 
          M.var.dir = M_var_dir, 
          batch = batch_cal,
          out.dir = out_dir, 
          reg.mult = reg_mult, 
          f.clas = f_clas, 
          args = args,
          maxent.path = maxent_path, 
          wait = wait, 
          run = run)
