# Download the LANDFIRE disturbance data to see whether it is of high enough
# quality to use for our purposes
library(pbapply)
library(terra)
library(dplyr)

dir.create("data/raw/landfire-disturbance/", recursive = TRUE, showWarnings = FALSE)

landfire_dl_urls <- c("https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST1999.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2000.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2001.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2002.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2003.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2004.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2005.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2006.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2007.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2008.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2009.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2010.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2011.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2012.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2013.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2014.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-LF2015_Dist_200_CONUS.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-LF2016_Dist_200_CONUS.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-LF2017_Dist_220_CONUS.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-LF2018_Dist_220_CONUS.zip&TYPE=landfire",
                      "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-LF2019_Dist_220_CONUS.zip&TYPE=landfire")

destfiles <- file.path("data", "raw", "landfire-disturbance", paste0("landfire_", 1999:2019, ".zip"))

mapply(FUN = function(url, destfile) {download.file(url = url, 
                                                    destfile = destfile)},
       url = landfire_dl_urls, 
       destfile = destfiles)

mapply(FUN = function(destfile, exdir) {unzip(zipfile = destfile, exdir = exdir)},
       destfile = destfiles, 
       exdir = gsub(pattern = ".zip", replacement = "", x = destfiles))

# Find all the geoTIFF files (use the $ to signify "end of line" in the matching,
# otherwise you get files that end in .tif.vat.db or .tif.ovr as well)
tifs <- 
  list.files(file.path("data", "raw", "landfire-disturbance"), 
             pattern = ".tif$", 
             recursive = TRUE,
             full.names = TRUE)

csvs <- 
  list.files(file.path("data", "raw", "landfire-disturbance"), 
             pattern = ".csv$", 
             recursive = TRUE,
             full.names = TRUE)

# spat_metadata <- 
#   list.files(file.path("data", "raw", "landfire-disturbance"), 
#              pattern = ".shp$", 
#              recursive = TRUE,
#              full.names = TRUE)


