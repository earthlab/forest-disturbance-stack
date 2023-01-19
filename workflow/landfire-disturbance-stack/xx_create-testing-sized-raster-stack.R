# Create small raster stack for testing

library(here)
library(dplyr)
library(terra)
library(USAboundaries)
library(sf)

dir.create(here::here("data", "ard", "mariposa"), 
           showWarnings = FALSE, 
           recursive = TRUE)

ard_files <- list.files(path = here::here("data", "ard", "western-conus"), 
                        pattern = "forest-disturbance-stack", 
                        full.names = TRUE)

s <- terra::rast(sort(ard_files))
names(s) <- 1999:2020

mariposa <- 
  USAboundaries::us_counties(resolution = "high", states = "California")[, -9] %>% 
  dplyr::filter(namelsad == "Mariposa County") %>% 
  sf::st_transform(5070) %>% 
  terra::vect()

mariposa_r <- terra::crop(x = s, 
                          y = mariposa, 
                          filename = here::here("data", "ard", "mariposa", "forest-disturbance-stack_mariposa.tif"),
                          overwrite = TRUE)
  
