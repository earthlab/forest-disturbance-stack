# Mosaic hotter drought data
install.packages("here")
library(terra)
library(dplyr)
library(googledrive)
library(purrr)
library(here)

driveFolder <- 'GEE_Exports'
googledrive::drive_auth()  

cyverse <- TRUE
if(cyverse) {
  system("cp -r ~/data-store/data/iplant/home/shared/earthlab/macrosystems/disturbance-stack/data ~/forest-disturbance-stack/")
}


#' Download a file from Google Drive to a local directory
#'
#' This function downloads a file from a Google Drive path to a specified local path.
#'
#' @param gDrivePath A character string. The path or name of the file on Google Drive.
#' @param localPath A character string. The local path where the file will be saved.
#' @param overwrite A logical value indicating whether to overwrite the file if it already exists at the local path. Defaults to `TRUE`.
#'
#' @details This function retrieves a file's ID from Google Drive using the provided `gDrivePath` and downloads it to the local directory specified by `localPath`. The file will be overwritten if `overwrite` is set to `TRUE` (default).
#' 
#' @return The downloaded file will be saved to the specified `localPath`.
#' 
#' @note You must be authenticated with Google Drive via the `googledrive` package for this function to work.
#' 
#' @importFrom googledrive drive_get drive_download as_id
#' 
#' @examples
#' \dontrun{
#' # Example usage:
#' download_data_from_gdrive("path/to/file/on/drive", "path/to/local/file.csv")
#' }
#' 
#' @export
download_data_from_gdrive <- function(gDrivePath, localPath) {
  # Validate inputs
  if (missing(gDrivePath) || missing(localPath)) {
    stop("Both 'gDrivePath' and 'localPath' must be provided.")
  }
  if (!is.character(gDrivePath) || !nzchar(gDrivePath)) {
    stop("'gDrivePath' must be a non-empty string.")
  }
  if (!is.character(localPath) || !nzchar(localPath)) {
    stop("'localPath' must be a non-empty string.")
  }
  
  # Retrieve file ID from GDrive
  f <- googledrive::drive_get(gDrivePath)
  id <- f$id
  nm <- f$name
  
  googledrive::drive_download(googledrive::as_id(id), path = localPath, overwrite = TRUE)
}

# A function to read a good fire data file
download.hotdrought.csv.from.gdrive <- function(file_name) {
  path <- paste0("~/", driveFolder, "/", file_name)
  csv <- download_data_from_gdrive(gDrivePath = path, localPath = here::here('data', 'tmp', file_name))
  return(csv)
}

folder <- drive_get(driveFolder)

# Check if the folder was found
if (nrow(folder) == 0) {
  stop("Folder not found. Check the name or ID.")
}

# List files in the folder that start with "hammond"
files_in_folder <- drive_ls(
  path = as_id(folder$id), # Use the folder's ID
  pattern = "^hammond"     # Regex for files starting with "hammond"
)


purrr::map(.x = files_in_folder$name,
           .f = download.hotdrought.csv.from.gdrive)


#ee_data_dir <- "D:/google-drive_uc-davis/My Drive/ee/"

hotter_drought_dir <- "data/out/hotter-drought/western-conus"
dir.create(hotter_drought_dir, recursive = TRUE, showWarnings = FALSE)

years <- 1999:2020

drought_files <- list.files(here::here('data', 'tmp'), pattern = "hammond-hotter-drought", full.names = TRUE)

for (i in seq_along(years)) {
  
  tiles <- drought_files[grep(x = drought_files, pattern = years[i])]
  out_fname <- file.path(hotter_drought_dir, paste0("hammond-hotter-drought_", years[i], ".tif"))
  
  lf_r <- terra::rast(file.path("data", "out", "landfire-disturbance", "western-conus", paste0("landfire-disturbance_western-conus_", years[i], ".tif")))
  
  hotter_drought_cropped_r <- 
    lapply(X = tiles, FUN = terra::rast) %>% 
    terra::sprc() %>% 
    terra::merge() %>% 
    terra::crop(y = lf_r, filename = out_fname, datatype = "INT1U", overwrite = TRUE)

}