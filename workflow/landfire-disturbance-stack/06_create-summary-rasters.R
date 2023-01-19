# Some summary rasters of the forest disturbance stack

library(here)
library(dplyr)
library(terra)
library(USAboundaries)
library(sf)

# # Full menu
# # Fire or not
# # Insect/disease or not
# # any drought or not
# # 4 threshold drought or not
# # 5 threshold drought or not
# # 6 threshold drought or not
# 
# # fire AND any drought
# # fire AND drought4
# # fire AND drought5
# # fire AND drought6
# 
# # insect_disease AND any drought
# # insect_disease AND drought4
# # insect_disease AND drought5
# # insect_disease AND drought6
# 
# # fire OR insect/disease
# 
# # fire OR any drought
# # fire OR drought4
# # fire OR drought5
# # fire OR drought6
# 
# # insect_disease OR any drought
# # insect_disease OR drought4
# # insect_disease OR drought5
# # insect_disease OR drought6
# 
# # fire OR any drought OR insect_disease
# # fire OR drought4 OR insect_disease
# # fire OR drought5 OR insect_disease
# # fire OR drought6 OR insect_disease
# 
# # Reduced menu
# # Fire or not
# # Insect/disease or not
# # 6 threshold drought or not
# # fire AND drought6
# # insect_disease AND drought6
# # fire OR insect/disease
# # fire OR drought6
# # insect_disease OR drought6
# # fire OR drought6 OR insect_disease
# 
# lapply(fnames, FUN = function(x) {
#   
#   year <- stringr::str_sub(x, start = -8, end = -5)
#   
#   fire_fname <- here::here("data", "out", "forest-disturbance-stack", "western-conus", "fire", 
#                            paste0("forest-disturbance-stack_western-conus_fire_", year, ".tif"))
#   
#   insect_disease_fname <- here::here("data", "out", "forest-disturbance-stack", "western-conus", "insect-disease", 
#                                      paste0("forest-disturbance-stack_western-conus_insect-disease_", year, ".tif"))
#   
#   drought6_fname <- here::here("data", "out", "forest-disturbance-stack", "western-conus", "drought6", 
#                                paste0("forest-disturbance-stack_western-conus_drought6_", year, ".tif"))
#   
#   fire_AND_drought6_fname <- here::here("data", "out", "forest-disturbance-stack", "western-conus", "fire_AND_drought6", 
#                                         paste0("forest-disturbance-stack_western-conus_fire_AND_drought6_", year, ".tif"))
#   
#   insect_disease_AND_drought6_fname <- here::here("data", "out", "forest-disturbance-stack", "western-conus", "insect_disease_AND_drought6", 
#                                         paste0("forest-disturbance-stack_western-conus_insect_disease_AND_drought6_", year, ".tif"))
#   
#   fire_OR_insect_disease_fname <- here::here("data", "out", "forest-disturbance-stack", "western-conus", "fire_OR_insect-disease", 
#                                              paste0("forest-disturbance-stack_western-conus_fire_OR_insect-disease_", year, ".tif"))
#   
#   
#   # simple effort of "multiple layer input to single layer output" to create the new categories
#     # as defined above in the RAT
#     # Write to disk at the same time, and ensure that the data type is as lightweight as possible
#     out <- terra::subst(x = s, 
#                         from = as.matrix(rat[, c("hd", "lf")]), 
#                         to = rat$new_val, 
#                         filename = out_fname,
#                         datatype = "INT1U",
#                         overwrite = TRUE)
#   
# })
# 

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
  )) %>% 
  dplyr::group_by(new_val, no_lf_bool, fire_bool, insect_disease_bool, other_lf_bool, no_drought_bool, drought4_bool, drought5_bool, drought6_bool, hd) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(orig_lf_vals = paste(data[[1]]$lf, collapse = "; ")) %>% 
  dplyr::rename(orig_hd_vals = hd) %>% 
  dplyr::select(-data) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(drought_description = dplyr::case_when(as.logical(no_drought_bool) ~ "no drought",
                                                       as.logical(drought4_bool) ~ "4 drought thresholds exceeded",
                                                       as.logical(drought5_bool) ~ "5 drought thresholds exceeded",
                                                       as.logical(drought6_bool) ~ "6 drought thresholds exceeded"),
                landfire_description = dplyr::case_when(as.logical(no_lf_bool) ~ "no landfire disturbance",
                                                        as.logical(fire_bool) ~ "fire disturbance",
                                                        as.logical(insect_disease_bool) ~ "insect/disease disturbance",
                                                        as.logical(other_lf_bool) ~ "other landfire disturbance"),
                description = paste(drought_description, landfire_description, sep = "; ")) %>% 
  dplyr::select(new_val, description, orig_lf_vals, orig_hd_vals, tidyselect::everything())

rat

# disturbance_stack <- terra::rast(x = here::here("data", "ard", "mariposa", "forest-disturbance-stack_mariposa.tif"))

fnames <- sort(list.files(here::here("data", "out", "forest-disturbance-stack", "western-conus"), full.names = TRUE))
lyr_names <- stringr::str_sub(fnames, start = -8, end = -5)
disturbance_stack <- terra::rast(fnames) %>% setNames(lyr_names)

###

fire_OR_insect_disease_OR_drought6_count <- 
  terra::app(x = disturbance_stack,
             fun = function(x) {
               sum(x %in% rat$new_val[rat$drought6_bool == 1 | rat$fire_bool == 1 | rat$insect_disease_bool == 1]) # count of 6-threshold drought OR fire OR insect/disease
             })

terra::writeRaster(x = fire_OR_insect_disease_OR_drought6_count,
                   filename = here::here("data", "ard", "western-conus", "fire_OR_insect-disease_OR_drought6_count.tif"),
                   datatype = "INT1U")

###

fire_OR_insect_disease_OR_drought6_multi <- fire_OR_insect_disease_OR_drought6_count > 1

terra::writeRaster(x = fire_OR_insect_disease_OR_drought6_multi,
                   filename = here::here("data", "ard", "western-conus", "fire_OR_insect-disease_OR_drought6_multi.tif"),
                   datatype = "INT1U")

###

fire_count <- 
  terra::app(x = disturbance_stack,
             fun = function(x) {
               sum(x %in% rat$new_val[rat$fire_bool == 1])
             })

terra::writeRaster(x = fire_count,
                   filename = here::here("data", "ard", "western-conus", "fire_count.tif"),
                   datatype = "INT1U")

###

insect_disease_count <- 
  terra::app(x = disturbance_stack,
             fun = function(x) {
               sum(x %in% rat$new_val[rat$insect_disease_bool == 1])
             })

terra::writeRaster(x = insect_disease_count,
                   filename = here::here("data", "ard", "western-conus", "insect-disease_count.tif"),
                   datatype = "INT1U")

###

fire_OR_insect_disease_count <-
  terra::app(x = disturbance_stack,
             fun = function(x) {
               sum(x %in% rat$new_val[rat$fire_bool == 1 | rat$insect_disease_bool == 1]) # count of fire OR insect/disease
             })

terra::writeRaster(x = fire_OR_insect_disease_count,
                   filename = here::here("data", "ard", "western-conus", "fire_OR_insect-disease_count.tif"),
                   datatype = "INT1U")

###

fire_AND_insect_disease_multi <-
  terra::app(x = disturbance_stack,
             fun = function(x) {
               sum(x %in% rat$new_val[rat$fire_bool == 1]) >= 1 & sum(x %in% rat$new_val[rat$insect_disease_bool == 1]) >= 1 
               })

terra::writeRaster(x = fire_AND_insect_disease_multi,
                   filename = here::here("data", "ard", "western-conus", "fire_AND_insect-disease_multi.tif"),
                   datatype = "INT1U")

###

fire_OR_insect_disease_multi <- fire_OR_insect_disease_count > 1

terra::writeRaster(x = fire_OR_insect_disease_multi,
                   filename = here::here("data", "ard", "western-conus", "fire_OR_insect-disease_multi.tif"),
                   datatype = "INT1U")

###


drought6_count <-
  terra::app(x = disturbance_stack,
             fun = function(x) {
               sum(x %in% rat$new_val[rat$drought6_bool == 1])
             })

terra::writeRaster(x = drought6_count,
                   filename = here::here("data", "ard", "western-conus", "drought6_count.tif"),
                   datatype = "INT1U")

###

drought6_multi <- drought6_count > 1

terra::writeRaster(x = drought6_multi,
                   filename = here::here("data", "ard", "western-conus", "drought6_multi.tif"),
                   datatype = "INT1U")

###

min_interval_no_fire_OR_insect_disease_OR_drought6 <-
  terra::app(x = disturbance_stack,
             fun = function(x) {
               fire_OR_insect_disease_OR_drought_bool <- as.numeric(x %in% rat$new_val[rat$drought6_bool == 1 | rat$fire_bool == 1 | rat$insect_disease_bool == 1])
               rle_out <- rle(fire_OR_insect_disease_OR_drought_bool)
               zero_seq <- rle_out$lengths[rle_out$values == 0]
               back_to_back <- any(rle_out$lengths[rle_out$values == 1] > 1)
               
               out <- ifelse(back_to_back | length(zero_seq) == 0, yes = 0, no = min(zero_seq))
               
               return(out)
             })

terra::writeRaster(x = min_interval_no_fire_OR_insect_disease_OR_drought6,
                   filename = here::here("data", "ard", "western-conus", "min-return-interval_fire_OR_insect-disease_OR_drought6.tif"),
                   datatype = "INT1U")

###

min_interval_no_fire_OR_insect_disease <-
  terra::app(x = disturbance_stack,
             fun = function(x) {
               fire_OR_insect_disease_bool <- as.numeric(x %in% rat$new_val[rat$fire_bool == 1 | rat$insect_disease_bool == 1])
               rle_out <- rle(fire_OR_insect_disease_bool)
               zero_seq <- rle_out$lengths[rle_out$values == 0]
               back_to_back <- any(rle_out$lengths[rle_out$values == 1] > 1)
               
               out <- ifelse(back_to_back | length(zero_seq) == 0, yes = 0, no = min(zero_seq))
               
               return(out)
             })

terra::writeRaster(x = min_interval_no_fire_OR_insect_disease,
                   filename = here::here("data", "ard", "western-conus", "min-return-interval_fire_OR_insect-disease.tif"),
                   datatype = "INT1U")

###

max_interval_no_fire_OR_insect_disease_OR_drought6 <-
  terra::app(x = disturbance_stack,
             fun = function(x) {
               fire_OR_insect_disease_OR_drought_bool <- as.numeric(x %in% rat$new_val[rat$drought6_bool == 1 | rat$fire_bool == 1 | rat$insect_disease_bool == 1])
               rle_out <- rle(fire_OR_insect_disease_OR_drought_bool)
               zero_seq <- rle_out$lengths[rle_out$values == 0]
               back_to_back <- any(rle_out$lengths[rle_out$values == 1] > 1)
               
               out <- ifelse(length(zero_seq) == 0, yes = 0, no = max(zero_seq))
               
               return(out)
             })

terra::writeRaster(x = max_interval_no_fire_OR_insect_disease_OR_drought6,
                   filename = here::here("data", "ard", "western-conus", "max-return-interval_fire_OR_insect-disease_OR_drought6.tif"),
                   datatype = "INT1U")

###

max_interval_no_fire_OR_insect_disease <-
  terra::app(x = disturbance_stack,
             fun = function(x) {
               fire_OR_insect_disease_bool <- as.numeric(x %in% rat$new_val[rat$fire_bool == 1 | rat$insect_disease_bool == 1])
               rle_out <- rle(fire_OR_insect_disease_bool)
               zero_seq <- rle_out$lengths[rle_out$values == 0]
               back_to_back <- any(rle_out$lengths[rle_out$values == 1] > 1)
               
               out <- ifelse(length(zero_seq) == 0, yes = 0, no = max(zero_seq))
               
               return(out)
             })

terra::writeRaster(x = max_interval_no_fire_OR_insect_disease,
                   filename = here::here("data", "ard", "western-conus", "max-return-interval_fire_OR_insect-disease.tif"),
                   datatype = "INT1U")

###

drought4_count <-
  terra::app(x = disturbance_stack,
             fun = function(x) {
               sum(x %in% rat$new_val[rat$drought4_bool == 1])
             })

terra::writeRaster(x = drought4_count,
                   filename = here::here("data", "ard", "western-conus", "drought4_count.tif"),
                   datatype = "INT1U")

###

drought5_count <-
  terra::app(x = disturbance_stack,
             fun = function(x) {
               sum(x %in% rat$new_val[rat$drought5_bool == 1])
             })

terra::writeRaster(x = drought5_count,
                   filename = here::here("data", "ard", "western-conus", "drought5_count.tif"),
                   datatype = "INT1U")

###


plot(any_drought_count, col = viridis::viridis(22))
plot(fire_OR_insect_disease_OR_drought_count, col = viridis::viridis(22))
plot(min_interval_no_fire_OR_insect_disease, col = viridis::viridis(22))
plot(max_interval_no_fire_OR_insect_disease, col = viridis::viridis(22))

plot(min_interval_no_fire_OR_insect_disease_OR_drought, col = viridis::viridis(22))
plot(max_interval_no_fire_OR_insect_disease_OR_drought, col = viridis::viridis(22))

plot(disturbance_count, col = viridis::viridis(15))
plot(fire_OR_insect_disease_OR_drought6_count, col = viridis::viridis(6))
plot(fire_insect_disease_count, col = viridis::viridis(6))
plot(any_lf_count, col = viridis::viridis(10))
plot(drought6_count, col = viridis::viridis(4))

# disturbance_count <- 
#   terra::app(x = disturbance_stack,
#              fun = function(x) {
#                sum(x %in% rat$new_val[rat$no_drought_bool == 0 | rat$no_lf_bool == 0]) # some kind of drought OR fire OR insect_disease
#              })
# 
# fire_OR_insect_disease_OR_drought_count <- 
#   terra::app(x = disturbance_stack,
#              fun = function(x) {
#                sum(x %in% rat$new_val[rat$no_drought_bool == 0 | rat$fire_bool == 1 | rat$insect_disease_bool == 1]) # some kind of drought OR fire OR insect/disease
#              })
# 
# fire_OR_insect_disease_OR_drought4_count <- 
#   terra::app(x = disturbance_stack,
#              fun = function(x) {
#                sum(x %in% rat$new_val[rat$drought4_bool == 1 | rat$fire_bool == 1 | rat$insect_disease_bool == 1]) # 4-threshold drought OR fire OR insect/disease
#              })
# 
# fire_OR_insect_disease_OR_drought5_count <- 
#   terra::app(x = disturbance_stack,
#              fun = function(x) {
#                sum(x %in% rat$new_val[rat$drought5_bool == 1 | rat$fire_bool == 1 | rat$insect_disease_bool == 1]) # 5-threshold drought OR fire OR insect/disease
#              })


# min_interval_no_fire_OR_insect_disease_OR_drought <-
#   terra::app(x = disturbance_stack,
#              fun = function(x) {
#                fire_OR_insect_disease_OR_drought_bool <- as.numeric(x %in% rat$new_val[rat$no_drought_bool == 0 | rat$fire_bool == 1 | rat$insect_disease_bool == 1])
#                rle_out <- rle(fire_OR_insect_disease_OR_drought_bool)
#                zero_seq <- rle_out$lengths[rle_out$values == 0]
#                back_to_back <- any(rle_out$lengths[rle_out$values == 1] > 1)
#                
#                out <- ifelse(back_to_back | length(zero_seq) == 0, yes = 0, no = min(zero_seq))
#                
#                return(out)
#              })
# 
# max_interval_no_fire_OR_insect_disease_OR_drought <-
#   terra::app(x = disturbance_stack,
#              fun = function(x) {
#                fire_OR_insect_disease_OR_drought_bool <- as.numeric(x %in% rat$new_val[rat$no_drought_bool == 0 | rat$fire_bool == 1 | rat$insect_disease_bool == 1])
#                rle_out <- rle(fire_OR_insect_disease_OR_drought_bool)
#                zero_seq <- rle_out$lengths[rle_out$values == 0]
#                back_to_back <- any(rle_out$lengths[rle_out$values == 1] > 1)
#                
#                out <- ifelse(length(zero_seq) == 0, yes = 0, no = max(zero_seq))
#                
#                return(out)
#              })
# 
# min_interval_no_fire_OR_insect_disease_OR_drought4 <-
#   terra::app(x = disturbance_stack,
#              fun = function(x) {
#                fire_OR_insect_disease_OR_drought_bool <- as.numeric(x %in% rat$new_val[rat$drought4_bool == 1 | rat$fire_bool == 1 | rat$insect_disease_bool == 1])
#                rle_out <- rle(fire_OR_insect_disease_OR_drought_bool)
#                zero_seq <- rle_out$lengths[rle_out$values == 0]
#                back_to_back <- any(rle_out$lengths[rle_out$values == 1] > 1)
#                
#                out <- ifelse(back_to_back | length(zero_seq) == 0, yes = 0, no = min(zero_seq))
#                
#                return(out)
#              })
# 
# max_interval_no_fire_OR_insect_disease_OR_drought4 <-
#   terra::app(x = disturbance_stack,
#              fun = function(x) {
#                fire_OR_insect_disease_OR_drought_bool <- as.numeric(x %in% rat$new_val[rat$drought4_bool == 1 | rat$fire_bool == 1 | rat$insect_disease_bool == 1])
#                rle_out <- rle(fire_OR_insect_disease_OR_drought_bool)
#                zero_seq <- rle_out$lengths[rle_out$values == 0]
#                back_to_back <- any(rle_out$lengths[rle_out$values == 1] > 1)
#                
#                out <- ifelse(length(zero_seq) == 0, yes = 0, no = max(zero_seq))
#                
#                return(out)
#              })
# 
# min_interval_no_fire_OR_insect_disease_OR_drought5 <-
#   terra::app(x = disturbance_stack,
#              fun = function(x) {
#                fire_OR_insect_disease_OR_drought_bool <- as.numeric(x %in% rat$new_val[rat$drought5_bool == 1 | rat$fire_bool == 1 | rat$insect_disease_bool == 1])
#                rle_out <- rle(fire_OR_insect_disease_OR_drought_bool)
#                zero_seq <- rle_out$lengths[rle_out$values == 0]
#                back_to_back <- any(rle_out$lengths[rle_out$values == 1] > 1)
#                
#                out <- ifelse(back_to_back | length(zero_seq) == 0, yes = 0, no = min(zero_seq))
#                
#                return(out)
#              })
# 
# max_interval_no_fire_OR_insect_disease_OR_drought5 <-
#   terra::app(x = disturbance_stack,
#              fun = function(x) {
#                fire_OR_insect_disease_OR_drought_bool <- as.numeric(x %in% rat$new_val[rat$drought5_bool == 1 | rat$fire_bool == 1 | rat$insect_disease_bool == 1])
#                rle_out <- rle(fire_OR_insect_disease_OR_drought_bool)
#                zero_seq <- rle_out$lengths[rle_out$values == 0]
#                back_to_back <- any(rle_out$lengths[rle_out$values == 1] > 1)
#                
#                out <- ifelse(length(zero_seq) == 0, yes = 0, no = max(zero_seq))
#                
#                return(out)
#              })

