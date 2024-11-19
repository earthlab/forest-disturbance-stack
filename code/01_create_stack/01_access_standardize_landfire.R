# Overview: This script accesses, standardizes, and saves landfire disturbance data
# Author: Tyler L. McIntosh

rm(list=ls()) #Ensure empty workspace if running from beginning

#################################################
#####          USER-SET PARAMETERS         ######
#################################################

cyverse <- TRUE #Sets computing location and file setup
cyverse_directory <- "~/data-store/data/iplant/home/shared/earthlab/macrosystems/disturbance-stack" # Set this if operating on cyverse. If local

#################################################

# Manage packages ----

if(!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

source(here::here("code", "functions.R"))

install_and_load_packages(
  package_list = c(
    "here",
    "terra",
    "sf",
    "tidyverse",
    "curl",
    "mapview",
    "gdalcubes",
    "tictoc",
    "tigris",
    "tmap",
    "stars",
    "furrr",
    "rlandfire",
    "pbapply"),
  auto_install = "y"
)

## Cyverse data store access if applicable ----
cyverse_data_dir <- paste0(cyverse_directory, "/data")
# Copy previous data over from data store if on cyverse and project has been run before
if(cyverse) {
  if(dir.exists(cyverse_data_dir)) {
    system(paste0("cp -r ", cyverse_data_dir, here::here()))
  }
}


# Set up necessary data directories
dir_ensure(here::here("data"))
dir_raw <- here::here("data/raw")
dir_derived <- here::here("data/derived")
dir_derived_ard <- here::here("data/derived/ard")
dir_raw_landfire <- here::here("data/raw/landfire")

dir_figs <- here::here("figs")
dir_ensure(dir_raw)
dir_ensure(dir_derived)
dir_ensure(dir_figs)
dir_ensure(dir_raw_landfire)
dir_ensure(dir_derived_ard)


# Area of interest
westernStates <- c("WA", "OR", "CA", "ID", "NV", "MT", "WY", "UT", "CO", "AZ", "NM")

west <- tigris::states(cb = FALSE) |>
  dplyr::filter(STUSPS %in% westernStates) |>
  sf::st_transform(5070)
west_file <- here::here(dir_raw, "west.gpkg")
sf::st_write(west, west_file, append = FALSE)

yrs <- seq(1999,2020)

# ACCESS LANDFIRE DATA ----

# IT IS MUCH FASTER TO DO THIS IN PARALLEL SEPARATELY FOR EACH FILE
# tic()
# #rlandfire::viewProducts()
# #LFSP product table: https://lfps.usgs.gov/helpdocs/productstable.html
# lf_response <- rlandfire::landfireAPI(
#   products = c("DIST1999", "DIST2000"),
#   aoi = rlandfire::getAOI(west),
#   projection = 5070,
#   resolution = 30,
#   path = here::here(dir_raw, "west_dist_test.zip"),
#   verbose = TRUE
# )
# toc() #866 sec for two, 150 sec for one


pull_lf_dist <- function(yr, aoi) {
  out_file <- here::here(dir_raw_landfire, paste0("west_dist_", as.character(yr), ".zip"))
  
  if(!file.exists(out_file)) {
    if(is.character(aoi)) {
      aoi <- sf::st_read(aoi)
    }
    
    lf_response <- rlandfire::landfireAPI(
      products = c(paste0("DIST", as.character(yr))),
      aoi = rlandfire::getAOI(west),
      projection = 5070,
      resolution = 30,
      path = out_file,
      verbose = TRUE
    )
    #if(lf_response$status == "Failed" | lf_response$status == "Timed out")
  }
}

parallel::detectCores()
future::plan(multisession, workers = length(yrs))
tic()
furrr::future_walk(.x = yrs,
                  .f = pull_lf_dist,
                  aoi = west_file)
toc()


#Unzip all files
zips <- list.files(path = dir_raw_landfire,
                   pattern = "\\.zip$",
                   full.names = TRUE)

furrr::future_walk(.x = zips,
                   .f = ensure_unzip)

#Get all tifs
tifs <- list.files(path = dir_raw_landfire,
                   pattern = "\\.tif$",
                   full.names = TRUE,
                   recursive = TRUE)


#Access CSVs
download_unzip_file(url = "https://landfire.gov/sites/default/files/CSV/Disturbance1999-2023.zip",
                    extract_to = dir_raw_landfire,
                    keep_zip = FALSE)

csvs <- list.files(path = dir_raw_landfire,
                   pattern = "\\.csv$")

csv_files <- list.files(
  path = dir_raw_landfire,
  pattern = "(DIST|disturb).*\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
) %>%
  tibble::tibble(file = .) |>
  filter(!grepl("^.*\\/LF2[1-3]", file) & !grepl("vdist", file)) |>
  pull(file)


# Code from Mike
dir.create("data/out/landfire-disturbance/conus", showWarnings = FALSE, recursive = TRUE)
dir.create("data/tmp", showWarnings = FALSE, recursive = TRUE)
dir.create("data/ard/landfire-disturbance/conus", showWarnings = FALSE, recursive = TRUE)

relevant_tifs <- tibble::tibble(
  tif = tifs,
  year = yrs
)

# Pair CSV files with their respective years
relevant_csvs <- tibble::tibble(
  csv = csv_files,
  year = yrs 
)


# Combine all relevant files into a single data frame
relevant_files <- tibble::tibble(
  destfile = destfiles,
  year = as.character(years)
) %>%
  merge(relevant_tifs, by = "year") %>%
  merge(relevant_csvs, by = "year") %>%
  mutate(
    tmp_path = here::here("data/tmp", paste0("landfire-disturbance_conus_", year, ".tif")),
    out_path = here::here("data/out/landfire-disturbance/conus", paste0("landfire-disturbance_conus_", year, ".tif")),
    ard_path = here::here("data/ard/landfire-disturbance/conus", paste0("landfire-disturbance_conus_", year, ".tif"))
  )


# Process raster attribute tables

process_rat <- function(file) {
  df <- read.csv(file = file)
  df <- df |>
    dplyr::rename_with(.fn = tolower) |>
    dplyr::rename_with(.fn = ~"year", .cols = tidyselect::contains("year")) |>
    dplyr::rename_with(.fn = ~"description", .cols = tidyselect::contains("descriptio"))
  if(any(grepl("type_confi", names(df), ignore.case = TRUE))) {
    df <- df |> dplyr::rename_with(.fn = ~"type_confidence", .cols = tidyselect::contains("type_confi"))
  }
  if(any(grepl("sev_confi", names(df), ignore.case = TRUE))) {
    df <- df |> dplyr::rename_with(.fn = ~"sev_confidence", .cols = tidyselect::contains("sev_confi"))
  }
  if(any(grepl("^Confidence", names(df), ignore.case = TRUE))) {
    df <- df |> dplyr::rename_with(.fn = ~"type_confidence", .cols = tidyselect::starts_with("Confidence"))
  }
  return(df)
}

rat <- purrr::map(.x = relevant_csvs$csv,
                  .f = process_rat) |>
  data.table::rbindlist(fill = TRUE, use.names = TRUE) |>
  dplyr::mutate(
    red = ifelse(red > 1, r / 256, red),
    green = ifelse(green > 1, g / 256, green),
    blue = ifelse(blue > 1, b / 256, blue),
  ) %>%
  dplyr::mutate(across(where(is.character), ~ifelse(. %in% c("NA", "", "N/A"), NA, .)))



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
  terra::extend(x = tmp, y = new_ext, filename = relevant_files$out_path[i], overwrite = TRUE)
  unlink(x = relevant_files$tmp_path[i])
  
}
(end <- Sys.time())
(difftime(end, start, units = "mins"))