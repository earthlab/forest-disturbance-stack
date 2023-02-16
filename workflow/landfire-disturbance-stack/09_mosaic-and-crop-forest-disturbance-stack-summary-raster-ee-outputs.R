# Mosaic hotter drought data

library(terra)
library(dplyr)
library(USAboundaries)
library(here)

ee_data_dir <- "D:/google-drive_uc-davis/My Drive/ee/"
ard_dir <- here::here("data", "ard")

template_r <- terra::rast(here::here("data", "out", "forest-disturbance-stack", "western-conus", "forest-disturbance-stack_western-conus_1999.tif"))

western_conus <- 
  USAboundaries::us_states(resolution = "high", states = c("California", "Washington", "Oregon", "Nevada", "Idaho", "Montana", "Arizona", "New Mexico", "Colorado", "Utah", "Wyoming")) %>% 
  sf::st_transform(sf::st_crs(template_r)) %>% 
  sf::st_geometry() %>% 
  terra::vect()

ard_files <- list.files(ee_data_dir, pattern = "fds-summary-raster", full.names = TRUE)

out_basenames <- gsub(x = ard_files, pattern = "-[0-9]+-[0-9]+.tif", replacement = "")
out_basenames <- unique(gsub(x = out_basenames, pattern = ee_data_dir, replacement = ""))

for (i in seq_along(out_basenames)) {
  
  tiles <- ard_files[grep(x = ard_files, pattern = out_basenames[i])]
  out_fname <- here::here(ard_dir, paste0(out_basenames[i], ".tif"))
  
  fds_summary_r <- 
    lapply(X = tiles, FUN = terra::rast) %>% 
    terra::sprc() %>% 
    terra::merge() %>% 
    terra::mask(mask = western_conus, filename = out_fname, datatype = "INT1U", overwrite = TRUE)

}