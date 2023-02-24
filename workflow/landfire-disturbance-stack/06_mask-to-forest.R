# mask the data to forests and export
library(dplyr)
library(terra)
library(here)

fnames <- sort(list.files(here::here("data", "out", "forest-disturbance-stack", "western-conus"), full.names = TRUE))
lyr_names <- stringr::str_sub(fnames, start = -8, end = -5)

forest_disturbance_stack <- terra::rast(fnames) %>% setNames(lyr_names)

if(!file.exists(here::here("data", "out", "bps-forest-raster-attribute-table.csv"))) {
  rat <- readr::read_csv(here::here("data", "raw", "landfire-biophysical-settings", "LF2020_BPS_220_CONUS", "CSV_Data", "LF20_BPS_220.csv"))
  
  # Go to https://www.landfirereview.org/search.php?q=&hPP=20&idx=lf_landfire_dev&p=0&dFR%5Bvegetation_type%5D%5B0%5D=Forest%20and%20Woodland&is_v=1
  # "Forest and Woodland" should be selected
  # Click "Download All Search Results Documents
  # Extract zipped folder and rename to "bps-model-docs_forest-and-woodlands"
  # Store unzipped folder in "data/raw/landfire-biophysical-settings/bsp-model-docs_forest-and-woodlands"
  
  bps_forest_models <- 
    list.files(here::here("data", "raw", "landfire-biophysical-settings", "bps-model-docs_forest-and-woodlands")) %>% 
    tibble::as_tibble() %>% 
    setNames("basename") %>%
    dplyr::mutate(BPS_MODEL = gsub(x = basename, pattern = ".docx", replacement = ""),
                  BPS_CODE = as.numeric(substr(x = basename, start = 1, stop = 5)),
                  map_zones = substr(x = BPS_MODEL, start = 7, stop = nchar(BPS_MODEL)),
                  forest = 1)
  
  rat <-
    rat %>% 
    dplyr::left_join(bps_forest_models) %>% 
    dplyr::mutate(forest = ifelse(is.na(forest), yes = 0, no = forest))
  
  data.table::fwrite(x = rat, file = here::here("data", "out", "bps-forest-raster-attribute-table.csv"))
}

rat <- data.table::fread(input = here::here("data", "out", "bps-forest-raster-attribute-table.csv"))

forest_rat <-
  rat %>% 
  dplyr::filter(forest == 1) %>% 
  dplyr::select(VALUE, forest)

landfire_bps <- 
  terra::rast(here::here("data", "raw", "landfire-biophysical-settings", "LF2020_BPS_220_CONUS", "Tif", "LC20_BPS_220.tif")) %>% 
  terra::crop(y = forest_disturbance_stack)

terra::writeRaster(x = landfire_bps, filename = here::here("data", "out", "landfire-bps_western-conus.tif"))

lf_bps <- terra::rast(here::here("data", "out", "landfire-bps_western-conus.tif"))

forest_mask <- 
  lf_bps %>% 
  terra::classify(rcl = as.matrix(forest_rat),
                  others = NA,
                  filename = here::here("data", "ard", "landfire-bps-derived-forest-mask.tif"),
                  datatype = "INT1U",
                  overwrite = TRUE)


masked_forest_disturbance_stack <-
  forest_disturbance_stack %>% 
  terra::mask(mask = forest_mask,
              filename = here::here("data", "ard", "masked-forest-disturbance-stack_western-conus.tif"),
              overwrite = TRUE,
              datatype = "INT1U")

# Full stack written to disk
terra::writeRaster(x = forest_disturbance_stack, 
                   filename = here::here("data", "ard", "forest-disturbance-stack_western-conus.tif"), 
                   overwrite = TRUE,
                   datatype = "INT1U")