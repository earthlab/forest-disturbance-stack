
#' Install and Load Required Packages Using pak
#'
#' This function checks if the specified packages (both CRAN and GitHub) are installed and loads them. 
#' If any packages are missing, it installs them automatically.
#' It uses the `pak` package for faster and more efficient package installation.
#'
#' @param package_list A list of package names to check and install (non-string, e.g., `c(dplyr, here)`).
#' GitHub packages should be specified as `username/repo` in strings.
#' @param auto_install A character ("y" or "n", default is "n"). If "y", installs all required packages 
#' without asking for user permission. If "n", asks for permission from the user.
#' @return No return value. Installs and loads the specified packages as needed.
#' @examples
#' \dontrun{
#' install_and_load_packages(c(dplyr, here, "username/repo"))
#' }
#' @importFrom pak pkg_install
#' @export
install_and_load_packages <- function(package_list, auto_install = "n") {
  # Convert non-string package names to strings
  package_list <- lapply(package_list, function(pkg) {
    if (is.symbol(pkg)) {
      deparse(substitute(pkg))
    } else {
      pkg
    }
  })
  
  # # Check if 'renv' is installed; if not, skip the 'renv' check
  # if (requireNamespace("renv", quietly = TRUE) && renv::is_active()) {
  #   cat("renv is active. Only loading packages...\n")
  #   for (pkg in package_list) {
  #     package_name <- if (grepl("/", pkg)) unlist(strsplit(pkg, "/"))[2] else pkg
  #     if (!require(package_name, character.only = TRUE)) {
  #       cat("Failed to load package:", package_name, "\n")
  #     }
  #   }
  #   return(invisible())
  # }
  
  # Check if pak is installed; install if not
  if (!requireNamespace("pak", quietly = TRUE)) {
    cat("The 'pak' package is required for fast installation of packages, installing now.\n")
    install.packages("pak")
  }
  
  # Initialize lists to store missing CRAN and GitHub packages
  missing_cran_packages <- c()
  missing_github_packages <- c()
  
  # # Helper function to get user input
  # get_user_permission <- function(prompt_msg) {
  #   if (auto_install == "y") {
  #     return("y")
  #   } else {
  #     return(tolower(readline(prompt = prompt_msg)))
  #   }
  # }
  
  # Check for missing packages
  for (pkg in package_list) {
    if (grepl("/", pkg)) { # GitHub package
      package_name <- unlist(strsplit(pkg, "/"))[2]
      package_loaded <- require(package_name, character.only = TRUE, quietly = TRUE)
    } else { # CRAN package
      package_loaded <- require(pkg, character.only = TRUE, quietly = TRUE)
    }
    if (!package_loaded) {
      if (grepl("/", pkg)) {
        missing_github_packages <- c(missing_github_packages, pkg)
      } else {
        missing_cran_packages <- c(missing_cran_packages, pkg)
      }
    }
  }
  
  # Install missing CRAN packages using pak::pkg_install
  if (length(missing_cran_packages) > 0) {
    # cat("The following CRAN packages are missing: ", paste(missing_cran_packages, collapse = ", "), "\n")
    # response <- get_user_permission("\nDo you want to install the missing CRAN packages? (y/n): ")
    # if (response == "y") {
    pak::pkg_install(missing_cran_packages, upgrade = TRUE)
    # } else {
    #   cat("Skipping installation of missing CRAN packages.\n")
    # }
  }
  
  # Install missing GitHub packages using pak::pkg_install
  if (length(missing_github_packages) > 0) {
    # cat("The following GitHub packages are missing: ", paste(missing_github_packages, collapse = ", "), "\n")
    # response <- get_user_permission("\nDo you want to install the missing GitHub packages? (y/n): ")
    # if (response == "y") {
    pak::pkg_install(missing_github_packages, upgrade = TRUE)
    # } else {
    #   cat("Skipping installation of missing GitHub packages.\n")
    # }
  }
  
  # Load all packages after checking for installation
  for (pkg in package_list) {
    if (grepl("/", pkg)) { # GitHub package
      package_name <- unlist(strsplit(pkg, "/"))[2]
      if (!require(package_name, character.only = TRUE)) {
        cat("Failed to load GitHub package:", package_name, "\n")
      }
    } else { # CRAN package
      if (!require(pkg, character.only = TRUE)) {
        cat("Failed to load CRAN package:", pkg, "\n")
      }
    }
  }
  
  cat("All specified packages installed and loaded.\n")
}


#' Ensure Directory Exists
#'
#' This function checks if a directory exists at the specified path, and if not, creates a new directory.
#'
#' @param path A character string specifying the path to the new directory.
#' @return The function does not return any value. It creates a directory if it does not already exist.
#' @examples
#' # Ensure a directory named "data" exists
#' dir_ensure("data")
#'
#' @export
dir_ensure <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
    message("Directory created: ", path)
  } else {
    message("Directory already exists: ", path)
  }
}



ensure_unzip <- function(zip_path) {
  # Check if the zip file exists
  if(!file.exists(zip_path)) stop("Zip file doesn't exist")
  
  # Define the output directory by removing the .zip extension from the path
  output_dir <- sub("\\.zip$", "", zip_path)
  
  # Check if the directory already exists, implying it's already unzipped
  if (dir.exists(output_dir)) {
    message("File already unzipped.")
    return(output_dir)  # Return the directory path if already unzipped
  }
  
  # Unzip the file into the output directory
  unzip(zip_path, exdir = output_dir)
  message("File unzipped successfully.")
  
  return(output_dir)  # Return the directory path after unzipping
}



#' Download and Unzip a File
#'
#' Downloads a ZIP file from a specified URL and extracts its contents to a specified directory.
#' Optionally, the ZIP file can be retained after extraction.
#'
#' @param url Character. The URL of the ZIP file to download.
#' @param extract_to Character. The directory where the contents should be extracted.
#' @param keep_zip Logical. If `TRUE`, retains the ZIP file after extraction. Defaults to `FALSE`.
#'
#' @return Invisible `NULL`. The function is used for its side effects of downloading and extracting files.
#'
#' @details The function downloads a ZIP file from a URL and extracts its contents to a specified directory.
#' If `keep_zip` is set to `FALSE`, the ZIP file will be deleted after extraction.
#'
#' @importFrom utils download.file unzip
#' @export
#'
#' @examples
#' \dontrun{
#' download_unzip_file("https://example.com/data.zip", "path/to/extract", keep_zip = TRUE)
#' }
download_unzip_file <- function(url, extract_to, keep_zip = FALSE) {
  # Validate URL and extraction path
  if (!is.character(url) || length(url) != 1) stop("`url` must be a single character string.")
  if (!is.character(extract_to) || length(extract_to) != 1) stop("`extract_to` must be a single character string.")
  if (!is.logical(keep_zip) || length(keep_zip) != 1) stop("`keep_zip` must be a single logical value.")
  
  # Ensure the extraction directory exists
  if (!dir.exists(extract_to)) dir.create(extract_to, recursive = TRUE)
  
  # Determine the path to save the ZIP file
  zip_path <- if (keep_zip) {
    # Save the ZIP file to the specified extraction directory
    file.path(extract_to, basename(url))
  } else {
    # Use a temporary file path for the ZIP file
    tempfile(fileext = ".zip")
  }
  
  # Ensure temporary file cleanup if there's an error and keep_zip is FALSE
  on.exit({
    if (!keep_zip && file.exists(zip_path)) {
      unlink(zip_path)
    }
  }, add = TRUE)
  
  # Attempt to download the ZIP file
  tryCatch({
    download.file(url, zip_path, mode = "wb")
  }, error = function(e) {
    stop("Failed to download the file from the specified URL: ", e$message)
  })
  
  # Attempt to unzip the file to the specified extraction directory
  tryCatch({
    unzip(zip_path, exdir = extract_to)
  }, error = function(e) {
    stop("Failed to unzip the file: ", e$message)
  })
  
  # Delete the ZIP file if 'keep_zip' is FALSE
  if (!keep_zip) {
    unlink(zip_path)
  }
  
  gc()
  
  invisible(NULL)
}
