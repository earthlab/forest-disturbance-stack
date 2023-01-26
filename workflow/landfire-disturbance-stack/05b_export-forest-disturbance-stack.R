library(terra)
library(dplyr)

fnames <- sort(list.files(here::here("data", "out", "forest-disturbance-stack", "western-conus"), full.names = TRUE))
lyr_names <- stringr::str_sub(fnames, start = -8, end = -5)

forest_disturbance_stack <- terra::rast(fnames) %>% setNames(lyr_names)

# Full stack written to disk
# https://stackoverflow.com/questions/73865059/geotiff-raster-data-to-delta-lake-parquet-format

terra::writeRaster(x = forest_disturbance_stack, 
                   filename = here::here("data", "ard", "forest-disturbance-stack_western-conus.pqt"), 
                   datatype = "INT1U",
                   overwrite = TRUE,
                   driver = "Parquet")

library(terra)
x <- rast(ncol=10, nrow=10, val=1:100)
terra::gdal()
writeRaster(x, "file.pqt", filetype="Parquet")
