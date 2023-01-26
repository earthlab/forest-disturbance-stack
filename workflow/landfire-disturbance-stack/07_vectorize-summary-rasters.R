# Vectorize the multi-disturbance boolean rasters

library(terra)
library(here)
library(dplyr)
library(USAboundaries)

template_r <- terra::rast(here::here("data", "out", "forest-disturbance-stack", "western-conus", "forest-disturbance-stack_western-conus_1999.tif"))

western_conus <- 
  USAboundaries::us_states(resolution = "high", states = c("California", "Washington", "Oregon", "Nevada", "Idaho", "Montana", "Arizona", "New Mexico", "Colorado", "Utah", "Wyoming")) %>% 
  sf::st_transform(sf::st_crs(template_r)) %>% 
  dplyr::mutate(area_km2 = as.numeric(sf::st_area(.)) / 1000000)

western_conus_area_km2 <- sum(western_conus$area_km2)

lf_drought_multi_r <- terra::rast(here::here("data", "ard", "fds-summary-raster_fire_OR_insect-disease_OR_drought6_multi-bool.tif"))
lf_multi_r <- terra::rast(here::here("data", "ard", "fds-summary-raster_fire_OR_insect-disease_multi-bool.tif"))

lf_drought_multi_p <- 
  terra::as.polygons(lf_drought_multi_r) %>% 
  terra::disagg() %>% 
  setNames(nm = "lf_drought_multi_disturbance")

terra::writeVector(x = lf_drought_multi_p, filename = here::here("data", "ard", "fds-summary-poly_fire_OR_insect-disease_OR_drought6_multi-bool.gpkg"))

multi <-
  lf_drought_multi_p %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(lf_drought_multi_disturbance == 1) %>% 
  dplyr::mutate(area_km2 = as.numeric(sf::st_area(.)) / 1000000)

sum(multi$area) / western_conus_area_km2

lf_multi_p <- 
  terra::as.polygons(lf_multi_r) %>% 
  terra::disagg() %>% 
  setNames(nm = "lf_multi_disturbance")

terra::writeVector(x = lf_multi_p, filename = here::here("data", "ard", "fds-summary-poly_fire_OR_insect-disease_multi-bool.gpkg"))

multi_lf <-
  lf_multi_p %>% 
  sf::st_as_sf() %>% 
  dplyr::filter(lf_multi_disturbance == 1) %>% 
  dplyr::mutate(area_km2 = as.numeric(sf::st_area(.)) / 1000000)

sum(multi_lf$area) / western_conus_area_km2

png(filename = here::here("figs", "multiple-fire_OR_insect-disease.png"), width = 10, height = 10, units = "in", res = 600)
plot(lf_multi_p)
dev.off()