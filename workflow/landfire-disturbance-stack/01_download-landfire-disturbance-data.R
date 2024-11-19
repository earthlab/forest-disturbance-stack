# Download the LANDFIRE disturbance data to see whether it is of high enough
# quality to use for our purposes
library(pbapply)
library(terra)
library(dplyr)
library(here)

dir.create("data/raw/landfire-disturbance/conus", showWarnings = FALSE, recursive = TRUE)

landfire_dl_urls <- c("https://landfire.gov/data-downloads/US_Disturbance/US_DIST1999.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2000.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2001.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2002.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2003.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2004.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2005.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2006.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2007.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2008.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2009.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2010.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2011.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2012.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2013.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/US_DIST2014.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/LF2015_Dist_200_CONUS.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/LF2016_Dist_200_CONUS.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/LF2017_Dist_220_CONUS.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/LF2018_Dist_220_CONUS.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/LF2019_Dist_220_CONUS.zip",
                      "https://landfire.gov/data-downloads/US_Disturbance/LF2020_Dist_220_CONUS.zip")

years <- 1999:2020
destfiles <- here::here("data", "raw", "landfire-disturbance", "conus", paste0("landfire-disturbance_conus_", years, ".zip"))

zips_dl <- list.files(path = here::here("data", "raw", "landfire-disturbance", "conus"), pattern = ".zip", full.names = TRUE)


if (length(zips_dl) == 0 | !all(zips_dl %in% destfiles)) {
  mapply(FUN = function(url, destfile) {download.file(url = url, 
                                                      destfile = destfile,
                                                      mode = "wb")},
         url = landfire_dl_urls, 
         destfile = destfiles)
  
  mapply(FUN = function(destfile, exdir) {unzip(zipfile = destfile, exdir = exdir)},
         destfile = destfiles, 
         exdir = gsub(pattern = ".zip", replacement = "", x = destfiles))
}
