# Overview: This script accesses, standardizes, and saves landfire disturbance data
# Author: Tyler L. McIntosh

rm(list=ls()) #Ensure empty workspace if running from beginning

#################################################
#####          USER-SET PARAMETERS         ######
#################################################

computing <- "cyverse" #Sets computing location and file setup; "cyverse" or "local"
cyverse_directory <- "~/data-store/data/iplant/home/shared/earthlab/macrosystems/disturbance-stack" # Set this if operating on cyverse. If local

#################################################

# Manage packages ----

#This code section loads a personal utilities package (tlmr), and then uses it for package management
if (!requireNamespace("tlmr", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")  # Install 'devtools' if it's not available
  }
  devtools::install_github('TylerLMcIntosh/tlm-r-utility', force = TRUE)
}
library(tlmr)
tlmr::install_and_load_packages(c(
  "here",
  "terra",
  "tictoc"
))

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}
if (!requireNamespace("tlmr", quietly = TRUE)) {
  pak::pkg_install("github_username/tlmr")
  
}



# Directory management ----

#Set directories
if(computing == "local") {
  HOME <- here::here()
} else if(computing == "cyverse") {
  HOME <- cyverse_directory
}



