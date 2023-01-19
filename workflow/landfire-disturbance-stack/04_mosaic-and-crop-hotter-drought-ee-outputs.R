# Mosaic hotter drought data

library(terra)
library(dplyr)

ee_data_dir <- "D:/google-drive_uc-davis/My Drive/ee/"

hotter_drought_dir <- "data/out/hotter-drought/western-conus"
dir.create(hotter_drought_dir, recursive = TRUE, showWarnings = FALSE)

years <- 1999:2020

drought_files <- list.files(ee_data_dir, pattern = "hammond-hotter-drought", full.names = TRUE)

for (i in seq_along(years)) {
  
  tiles <- drought_files[grep(x = drought_files, pattern = years[i])]
  out_fname <- file.path(hotter_drought_dir, paste0("hammond-hotter-drought_", years[i], ".tif"))
  
  lf_r <- terra::rast(file.path("data", "out", "landfire-disturbance", "western-conus", paste0("landfire-disturbance_western-conus_", years[i], ".tif")))
  
  hotter_drought_cropped_r <- 
    lapply(X = tiles, FUN = terra::rast) %>% 
    terra::sprc() %>% 
    terra::merge() %>% 
    terra::crop(y = lf_r, filename = out_fname, datatype = "INT1U", overwrite = TRUE)

}