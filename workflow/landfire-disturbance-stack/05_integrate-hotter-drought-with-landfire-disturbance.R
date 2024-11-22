library(dplyr)
library(terra)
library(here)

dir.create(here::here("data", "ard", "western-conus"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("data", "out", "forest-disturbance-stack", "western-conus"), showWarnings = FALSE, recursive = TRUE)

# Raster Attribute Table that describes the mapping of values in Landfire and Hotter Drought raster
# to a new value
rat <-
  expand.grid(lf = c(NA, 11:14, 21:24, 31:34), hd = c(0, 4:6)) %>% 
  dplyr::mutate(no_lf_bool = as.numeric(is.na(lf)),
                fire_bool = as.numeric(lf %in% 11:14),
                insect_disease_bool = as.numeric(lf %in% 21:24),
                other_lf_bool = as.numeric(lf %in% 31:34),
                no_drought_bool = as.numeric(hd == 0),
                drought4_bool = as.numeric(hd == 4),
                drought5_bool = as.numeric(hd == 5),
                drought6_bool = as.numeric(hd == 6)) %>% 
  dplyr::mutate(new_val = dplyr::case_when(no_drought_bool & no_lf_bool ~ 0, # no drought, no landfire disturbance --> 0
                                           no_drought_bool & fire_bool ~ 1, # no drought, fire --> 1
                                           no_drought_bool & insect_disease_bool ~ 2, # no drought, insects_disease --> 2
                                           no_drought_bool & other_lf_bool ~ 3, # no drought, other Landfire disturbance --> 3
                                           drought4_bool & no_lf_bool ~ 4, # 4 drought thresholds exceeded, no landfire disturbance --> 4
                                           drought4_bool & fire_bool ~ 5, # 4 drought thresholds exceeded, fire --> 5
                                           drought4_bool & insect_disease_bool ~ 6, # 4 drought thresholds exceeded, insects_disease --> 6
                                           drought4_bool & other_lf_bool ~ 7, # 4 drought thresholds exceeded, other Landfire disturbance --> 7
                                           drought5_bool & no_lf_bool ~ 8, # 5 drought thresholds exceeded, no landfire disturbance --> 8
                                           drought5_bool & fire_bool ~ 9, # 5 drought thresholds exceeded, fire --> 9
                                           drought5_bool & insect_disease_bool ~ 10, # 5 drought thresholds exceeded, insects_disease --> 10
                                           drought5_bool & other_lf_bool ~ 11, # 5 drought thresholds exceeded, other Landfire disturbance --> 11
                                           drought6_bool & no_lf_bool ~ 12, # 6 drought thresholds exceeded, no landfire disturbance --> 12
                                           drought6_bool & fire_bool ~ 13, # 6 drought thresholds exceeded, fire --> 13
                                           drought6_bool & insect_disease_bool ~ 14, # 6 drought thresholds exceeded, insects_disease --> 14
                                           drought6_bool & other_lf_bool ~ 15 # 6 drought thresholds exceeded, other Landfire disturbance --> 15
  ))

rat
years <- 1999:2020

xx <- terra::rast(list.files(path = here::here("data", "out", "landfire-disturbance", "western-conus"), pattern = "landfire", full.names = TRUE))
x <- terra::rast(here::here("data", "out", "landfire-disturbance", "western-conus", paste0("landfire-disturbance_western-conus_", years[1], ".tif")))
y <- terra::rast(here::here("data", "out", "landfire-disturbance", "western-conus", paste0("landfire-disturbance_western-conus_", years[17], ".tif")))
z <- terra::rast(here::here("data", "out", "hotter-drought", "western-conus", paste0("hammond-hotter-drought_", years[5], ".tif")))
zz <- z |> terra::crop(xx)
c <- tigris::states()

ext(x)
ext(z)
library(mapview)
library(sf)


for (i in seq_along(years)) {
  # Read target year's landfire data
  lf <- terra::rast(here::here("data", "out", "landfire-disturbance", "western-conus", paste0("landfire-disturbance_western-conus_", years[i], ".tif")))
  # Read target year's hotter drought data
  hd <- terra::rast(here::here("data", "out", "hotter-drought", "western-conus", paste0("hammond-hotter-drought_", years[i], ".tif")))  
  
  # We've already ensured that these two data layers are compatible, so we stack them into
  # a two-band raster here
  s <- c(hd, lf)
  
  # output file name
  out_fname <- file.path("data", "out", "forest-disturbance-stack", "western-conus", paste0("forest-disturbance-stack_western-conus_", years[i], ".tif"))
  
  # simple effort of "multiple layer input to single layer output" to create the new categories
  # as defined above in the RAT
  # Write to disk at the same time, and ensure that the data type is as lightweight as possible
  out <- terra::subst(x = s, 
                      from = as.matrix(rat[, c("hd", "lf")]), 
                      to = rat$new_val, 
                      filename = out_fname,
                      datatype = "INT1U",
                      overwrite = TRUE)
}
