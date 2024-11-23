# mask the data to forests and export
install.packages("rstac")

library(dplyr)
library(terra)
library(here)
library(sf)
library(rstac)

fnames <- sort(list.files(here::here("data", "out", "forest-disturbance-stack", "western-conus"), full.names = TRUE))
lyr_names <- stringr::str_sub(fnames, start = -8, end = -5)
forest_disturbance_stack <- terra::rast(fnames) %>% setNames(lyr_names)

fds_ext <- st_set_crs(st_as_sf(as.polygons(terra::ext(forest_disturbance_stack))), terra::crs(forest_disturbance_stack))


sr <- tlmr::access_data_epa_l3_ecoregions_vsi() |>
  dplyr::filter(US_L3NAME == "Southern Rockies") |>
  dplyr::group_by(US_L3NAME) |>
  dplyr::summarize(geometry = sf::st_union(geometry)) |>
  sf::st_transform("EPSG:4326")


usa <- tigris::states()
west <- usa[usa$STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "WY", "NV", "AZ", "CO", "NM", "UT"),] 
wy <- usa[usa$STUSPS %in% c("WY"),]




#' Access LCMAP v13 Data via STAC
#'
#' This function retrieves LCMAP (Land Change Monitoring, Assessment, and Projection) data for a specified year
#' and area of interest (AOI) using the STAC API. It downloads the LCMAP raster data and returns it as a `SpatRaster` object.
#'
#' @param year Integer or character representing the year of interest.
#' @param aoi An `sf` object representing the area of interest (AOI). Must have a valid CRS, which will be transformed to EPSG:4326.
#'
#' @return A `SpatRaster` object representing the LCMAP data for the specified years and AOI with each year in a layer named "lcmap_[YEAR]"
#' 
#' @details
#' The function queries the Microsoft Planetary Computer's STAC API for the USGS LCMAP dataset. It constructs the correct 
#' vsicurl URL to stream the data and processes it using GDAL's `warp` function. The returned raster is cropped to the 
#' specified AOI.
#' 
#' @source
#' STAC API: \url{https://planetarycomputer.microsoft.com/api/stac/v1}
#' 
#' @references
#' Code adapted from: \url{https://stacspec.org/en/tutorials/1-download-data-using-r/}
#'
#' @importFrom rstac stac stac_search get_request assets_url
#' @importFrom sf st_transform st_bbox st_crs gdal_utils
#' @importFrom terra rast
#' @export
#' @examples
#' \dontrun{
#' aoi <- sf::st_as_sfc(sf::st_bbox(c(xmin = -120, xmax = -119, ymin = 34, ymax = 35), crs = 4326))
#' lcmap_raster <- access_data_lcmap_v13_stac_single_year(2000, aoi)
#' }
access_data_lcmap_v13_stac_single_year <- function(year, aoi) {
  
  # Convert numeric year to character string
  if (is.numeric(year)) {
    year <- as.character(year)
  }
  
  # Ensure that the year is valid
  yearNum <- as.numeric(year)
  if (yearNum < 1985 || yearNum > 2021) {
    stop("'year' must be from 1985-2021")
  }
  
  # Ensure the AOI is a valid sf object and transform to EPSG:4326
  if (!inherits(aoi, "sf")) {
    stop("'aoi' must be a valid 'sf' object.")
  }
  aoi <- sf::st_transform(aoi, "EPSG:4326")
  bbox4326 <- sf::st_bbox(aoi)
  
  # Perform STAC query
  stac_query <- tryCatch({
    rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1") |>
      rstac::stac_search(
        collections = "usgs-lcmap-conus-v13",
        bbox = bbox4326,
        datetime = paste0(year, "-01-01/", year, "-12-31")
      ) |>
      rstac::get_request()
  }, error = function(e) {
    stop("Failed to query the STAC API: ", e$message)
  })
  
  # Ensure query returned results
  if (length(stac_query$features) == 0) {
    stop("No data found for the given date range and AOI.")
  }
  
  
  # Helper function to create a vsicurl URL
  make_lcmap_vsicurl_url <- function(base_url) {
    paste0(
      "/vsicurl", 
      "?pc_url_signing=yes",
      "&pc_collection=usgs-lcmap-conus-v13",
      "&url=",
      base_url
    )
  }
  
  # Extract the LCMAP primary raster URL (lcpri)
  lcpri_url <- tryCatch({
    make_lcmap_vsicurl_url(rstac::assets_url(stac_query, "lcpri"))
  }, error = function(e) {
    stop("Failed to retrieve 'lcpri' asset URL: ", e$message)
  })
  
  print(lcpri_url)
  # Prepare output file
  out_file <- tempfile(fileext = ".tif")
  
  # Use GDAL to download and process the raster data
  tryCatch({
    sf::gdal_utils(
      "warp",
      source = lcpri_url,
      destination = out_file,
      options = c(
        "-t_srs", sf::st_crs(aoi)$wkt,
        "-te", sf::st_bbox(aoi)
      )
    )
  }, error = function(e) {
    stop("GDAL warp process failed: ", e$message)
  })
  
  # Load the processed raster and return
  tryCatch({
    raster_output <- terra::rast(out_file)
  }, error = function(e) {
    stop("Failed to create raster from the downloaded file: ", e$message)
  })
  
  # Set layer names in the format "lcmap_year"
  names(raster_output) <- paste0("lcmap_", year)
  
  return(raster_output)
}


#' Access LCMAP v13 Data via STAC
#'
#' This function retrieves LCMAP (Land Change Monitoring, Assessment, and Projection) data for a specified range of years
#' and area of interest (AOI) using the STAC API. It downloads the LCMAP raster data and returns it as a `SpatRaster` object.
#'
#' @param earliest_year Integer or character representing the earliest year of interest.
#' @param latest_year Integer or character representing the latest year of interest. Must be equal to or greater than the earliest year.
#' @param aoi An `sf` object representing the area of interest (AOI). Must have a valid CRS, which will be transformed to EPSG:4326.
#'
#' @return A `SpatRaster` object representing the LCMAP data for the specified years and AOI, with each year in a layer named "lcmap_[YEAR]"
#' 
#' @details
#' The function queries the Microsoft Planetary Computer's STAC API for the USGS LCMAP dataset. It constructs the correct 
#' vsicurl URL to stream the data and processes it using GDAL's `warp` function. The returned raster is cropped to the 
#' specified AOI.
#' 
#' @source
#' STAC API: \url{https://planetarycomputer.microsoft.com/api/stac/v1}
#' 
#' @references
#' Code adapted from: \url{https://stacspec.org/en/tutorials/1-download-data-using-r/}
#'
#' @importFrom terra rast
#' @importFrom purrr map
#' @export
#' @examples
#' \dontrun{
#' aoi <- sf::st_as_sfc(sf::st_bbox(c(xmin = -120, xmax = -119, ymin = 34, ymax = 35), crs = 4326))
#' lcmap_raster <- access_data_lcmap_v13_stac_year_range(2000, 2003, aoi)
#' }
access_data_lcmap_v13_stac_year_range <- function(earliest_year, latest_year, aoi) {
  # Validate year inputs
  if (!is.numeric(earliest_year) && !is.character(earliest_year)) {
    stop("'earliest_year' must be numeric or a character string representing a year.")
  }
  if (!is.numeric(latest_year) && !is.character(latest_year)) {
    stop("'latest_year' must be numeric or a character string representing a year.")
  }
  
  # Convert character strings to numeric years 
  if (is.character(earliest_year)) {
    earliest_year <- as.numeric(earliest_year)
  }
  if (is.character(latest_year)) {
    latest_year <- as.numeric(latest_year)
  }
  
  # Ensure earliest_year <= latest_year
  if (earliest_year > latest_year) {
    stop("'earliest_year' must be less than or equal to 'latest_year'.")
  }
  
  # Ensure that the years are valid
  if (earliest_year < 1985 || latest_year > 2021) {
    stop("The year range requested must be from 1985-2021")
  }
  
  # Ensure the AOI is a valid sf object
  if (!inherits(aoi, "sf")) {
    stop("'aoi' must be a valid 'sf' object.")
  }
  
  years <- seq(earliest_year, latest_year)
  
  
  dats <- years |>
    purrr::map(~ access_data_lcmap_v13_stac_single_year(.x, aoi)) |>
    terra::rast()
  
  return(dats)
  
}


tictoc::tic()
lcmap <- access_data_lcmap_v13_stac_year_range(earliest_year = 2019, latest_year = 2020, aoi = wy)
tictoc::toc()

tictoc::tic()
lcmap <- access_data_lcmap_v13_stac_single_year(2020, aoi = wy)
tictoc::toc()

test_url <- "https://landcoverdata.blob.core.windows.net/lcmap/CU/V13/009006/2020/LCMAP_CU_009006_2020_20220721_V13_CCDC/LCMAP_CU_009006_2020_20220630_V13_LCPRI.tif"
httr::GET(test_url)


tictoc::tic()
lcms <- access_data_lcms_conus_v20239_vsi_year_range(earliest_year = 2019, latest_year = 2020) %>%
  terra::project(terra::crs(forest_disturbance_stack))
tictoc::toc()







forest_lcmap_mask <- any(lcmap == 4) |> as.numeric() #4 = Tree Cover for primary land cover
# "Tree-covered land where the tree cover density is greater
# than 10%. Cleared or harvested trees (i.e., clearcuts) will
# be mapped according to current cover (e.g., Barren,
#                                       Grass/Shrub).

forest_lcms_mask <- any(lcms %in% 1:5) |> as.numeric()





