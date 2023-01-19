library(dplyr)
library(terra)

dir.create("data/out/landfire-disturbance/conus", showWarnings = FALSE, recursive = TRUE)
dir.create("data/tmp", showWarnings = FALSE, recursive = TRUE)
dir.create("data/ard/landfire-disturbance/conus", showWarnings = FALSE, recursive = TRUE)

years <- 1999:2020
destfiles <- here::here("data", "raw", "landfire-disturbance", "conus", paste0("landfire-disturbance_conus_", years, ".zip"))

# Find all the geoTIFF files (use the $ to signify "end of line" in the matching,
# otherwise you get files that end in .tif.vat.db or .tif.ovr as well)
tifs <- 
  list.files(here::here("data", "raw", "landfire-disturbance", "conus"), 
             pattern = ".tif$", 
             recursive = TRUE,
             full.names = TRUE)

csvs <- 
  list.files(here::here("data", "raw", "landfire-disturbance", "conus"), 
             pattern = ".csv$", 
             recursive = TRUE,
             full.names = TRUE)

relevant_tifs <-
  tibble::tibble(tif = tifs,
                 year = substr(x = gsub(pattern = here::here("data", "raw", "landfire-disturbance", "conus"), replacement = "", x = tif),
                               start = 23, stop = 26)) %>% 
  dplyr::select(year, tif)

# CSV files are the raster attribute tables
relevant_csvs <-
  tibble::tibble(csv = csvs,
                 year = substr(x = gsub(pattern = here::here("data", "raw", "landfire-disturbance", "conus"), replacement = "", x = csv),
                               start = 23, stop = 26)) %>% 
  dplyr::select(year, csv)

# put all relevant files together in the same data frame
relevant_files <-
  tibble::tibble(destfile = destfiles,
                 year = substr(x = destfile, start = nchar(destfile) - 7, stop = nchar(destfile) - 4)) %>% 
  dplyr::select(year, destfile) %>% 
  merge(relevant_tifs, by = "year") %>% 
  merge(relevant_csvs, by = "year") %>% 
  dplyr::mutate(tmp_path = here::here('data', 'tmp', paste0('landfire-disturbance_conus_', year, ".tif"))) %>% 
  dplyr::mutate(out_path = here::here('data', 'out', 'landfire-disturbance', 'conus', paste0('landfire-disturbance_conus_', year, ".tif"))) %>% 
  dplyr::mutate(ard_path = here::here('data', 'ard', 'landfire-disturbance', 'conus', paste0('landfire-disturbance_conus_', year, '.tif')))

write.csv(x = relevant_files, file = here::here("data", "out", "landfire-disturbance", "file-directory_landfire-disturbance_conus.csv"), row.names = FALSE)

# Rasters have different extents
# terra::rast(tifs[1:9])
# terra::rast(tifs[10:12])
# terra::rast(tifs[13])
# terra::rast(tifs[14:18])
# terra::rast(tifs[19:22])
# terra::rast(tifs[19])
# 
# We might like to extend each of those
# extents to make them match the greatest possible extent
new_ext <- terra::ext(terra::rast(relevant_files$tif[1]))
for (i in seq_along(relevant_files$tif)[-1]) {
  this_ext <- terra::ext(terra::rast(tifs[i]))
  new_ext <- terra::union(x = new_ext, y = this_ext)
}

# Move .tif files with to break association with .dbf raster attribute tables
# and extend the rasters if needed
(start <- Sys.time())
for(i in 1:nrow(relevant_files)) {
  
  file.copy(from = relevant_files$tif[i], to = relevant_files$tmp_path[i])
  tmp <- terra::rast(relevant_files$tmp_path[i])
  terra::extend(x = tmp, y = new_ext, filename = relevant_files$out_path[i])
  unlink(x = relevant_files$tmp_path[i])
  
}
(end <- Sys.time())
(difftime(end, start, units = "mins"))

# Make the raster attribute tables per year easier to use
rat <- lapply(seq_along(relevant_files$year),
              FUN = function(i) {
                
                rat_year <-
                  read.csv(file = relevant_files$csv[i]) %>%
                  dplyr::rename_with(.fn = tolower) %>%
                  dplyr::rename_with(.fn = ~"year", .cols = tidyselect::contains("year")) %>%
                  dplyr::rename_with(.fn = ~"description", .cols = tidyselect::contains("descriptio")) %>%
                  dplyr::rename_with(.fn = ~"type_confidence", .cols = tidyselect::contains("type_confi")) %>%
                  dplyr::rename_with(.fn = ~"sev_confidence", .cols = tidyselect::contains("sev_confi")) %>%
                  dplyr::mutate(year_check = relevant_files$year[i])
                
                return(rat_year)
              }) %>%
  data.table::rbindlist(fill = TRUE) %>%
  dplyr::mutate(red = ifelse(red > 1, yes = r / 256, no = red),
                green = ifelse(green > 1, yes = g / 256, no = green),
                blue = ifelse(blue > 1, yes = b / 256, no = blue)) %>%
  dplyr::mutate(year = year_check) %>% # The 2008 data have the year recorded as 2009
  dplyr::select(-year_check) %>%
  dplyr::mutate(dplyr::across(.cols = tidyselect::where(fn = is.character), .fns = function(x) return(ifelse(x %in% c("NA", "", "N/A"), yes = NA, no = x))))

write.csv(x = rat, file = here::here("data", "out", "landfire-disturbance", "raster-attribute-table_landfire-disturbance_conus.csv"), row.names = FALSE)

# # If there are counts here that are greater than 2, then more than 1 value is being used to represent the same unique combination of attributes
# # across years
# # That's the case
# rat %>% 
#   group_by(dist_type, severity, confidence, source1, source2, source3, description, r, g, b, red, green, blue, type_confidence, sev_confidence, source4, sev_source) %>% 
#   summarize(n = length(unique(value))) %>% 
#   arrange((n)) %>% 
#   print(n = 100)
# 
# # As an example, a value of 1088 and 1087 are being used to represent the same set of attributes
# rat %>% 
#   filter(dist_type == "Biological", severity == "High", source1 == "LANDFIRE Events Geodatabase", is.na(source2), type_confidence == "Medium", sev_confidence == "Medium")

# Still need to figure out whether the same value is being used to represent multiple sets of attributes in different years
# Seems like that might be true, so probably best to re-code the raster based on attributes we care about

# # 38 minutes to reclassify whole CONUS raster for 1999
# (start <- Sys.time())
# test <- terra::rast(relevant_files$out_path[1])
# terra::classify(x = test, rcl = as.matrix(rat_year[, c("value", "new_val")]), filename = relevant_files$ard_path[1])
# (end <- Sys.time())
# (difftime(end, start, units = "mins"))

