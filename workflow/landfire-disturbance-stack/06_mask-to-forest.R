# mask the data to forests and export
library(dplyr)
library(terra)
library(here)

fnames <- sort(list.files(here::here("data", "out", "forest-disturbance-stack", "western-conus"), full.names = TRUE))
lyr_names <- stringr::str_sub(fnames, start = -8, end = -5)

forest_disturbance_stack <- terra::rast(fnames) %>% setNames(lyr_names)

rat <- readr::read_csv(here::here("data", "raw", "landfire-biophysical-settings", "LF2020_BPS_220_CONUS", "CSV_Data", "LF20_BPS_220.csv"))

bps_forest_models <- 
  list.files(here::here("data", "raw", "landfire-biophysical-settings", "bps-model-docs_forest-and-woodlands")) %>% 
  tibble::as_tibble() %>% 
  setNames("basename") %>%
  dplyr::mutate(BPS_MODEL = gsub(x = basename, pattern = ".docx", replacement = ""),
                BPS_CODE = as.numeric(substr(x = basename, start = 1, stop = 5)),
                map_zones = substr(x = BPS_MODEL, start = 7, stop = nchar(BPS_MODEL)),
                forest = 1)


# %>%
#   dplyr::rowwise() %>% 
#   dplyr::mutate(ZONE = c((strsplit(x = map_zones, split = "_")))) %>% 
#   tidyr::unnest(cols = "ZONE")

rat <-
  rat %>% 
  dplyr::left_join(bps_forest_models) %>% 
  dplyr::mutate(forest = ifelse(is.na(forest), yes = 0, no = forest))

forest_rat <-
  rat %>% 
  dplyr::filter(forest == 1) %>% 
  dplyr::select(VALUE, forest)

landfire_bps <- 
  terra::rast(here::here("data", "raw", "landfire-biophysical-settings", "LF2020_BPS_220_CONUS", "Tif", "LC20_BPS_220.tif")) %>% 
  terra::crop(y = forest_disturbance_stack)

terra::writeRaster(x = landfire_bps, filename = here::here("data", "out", "landfire-bps_western-conus.tif"))

lf_bps <- terra::rast(here::here("data", "out", "landfire-bps_western-conus.tif"))

# forest_mask <- 
#   lf_bps %>% 
#   terra::subst(from = forest_rat$VALUE,
#                to = forest_rat$forest,
#                others = NA,
#                filename = here::here("data", "ard", "landfire-bps-derived-forest-mask.tif"),
#                datatype = "INT1U",
#                overwrite = TRUE)
# 
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


# Fire or not
# Insect/disease or not
# any drought or not
# 4 threshold drought or not
# 5 threshold drought or not
# 6 threshold drought or not

# fire AND any drought
# fire AND drought4
# fire AND drought5
# fire AND drought6

# insect_disease AND any drought
# insect_disease AND drought4
# insect_disease AND drought5
# insect_disease AND drought6

# fire OR insect/disease

# fire OR any drought
# fire OR drought4
# fire OR drought5
# fire OR drought6

# insect_disease OR any drought
# insect_disease OR drought4
# insect_disease OR drought5
# insect_disease OR drought6

# fire OR any drought OR insect_disease
# fire OR drought4 OR insect_disease
# fire OR drought5 OR insect_disease
# fire OR drought6 OR insect_disease