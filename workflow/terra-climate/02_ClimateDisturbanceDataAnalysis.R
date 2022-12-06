#Climate disturbance data analysis
#Tyler McIntosh, 9/9/2022

#This analysis is based off of Hammond et al 2022.
#Data inputs for this script were wrangled in GEE,
#copy of script found in RProject workflow document 02_ClimateDataWrangling.js.

# Analysis per Hammond et al 2022:
# "For each climate variable (TMAX, VPD, CWD, SOIL M, PPT, PDSI, we calculated the anomaly 
# (for example, TMAX of the warmest month in the year of mortality - TMAX average for the 
# 61 values of the same month from 1958–2019). To provide cross-comparison between metrics
# with different scales and across disparate climates, anomalies were standardized into 
# zscores, such that time series had a mean of 0 and a standard deviation of 1 based on 
# using the entire period of record 1958–2019."

# Variable averages for each month across the time period, as well as max/mins/month in 
# which occurred all part of GEE outputs.

#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

############ SETUP WORKSPACE ############

###Standard libraries, unneeded libraries commented out

#Standard libraries
library(tidyverse) #Tidyverse!
library(here) #Relative path best practices
#library(kableExtra) #Table creation
#library(knitr) #For use with R markdown

#Geographic libraries
library(terra) #New raster data package, documentation pdf here: https://cran.r-project.org/web/packages/terra/terra.pdf
#library(sf) #New vector data package
#library(tmap) #Thematic mapping
#library(tmaptools) #Supports Tmap
#library(leaflet) #For interactive web mapping
#library(landscapemetrics) #Fragstats alternative

#Statistics
#library(modeest) #Modes of data
#library(moments) #Skewness & kurtosis

###Clean workspace
rm(list=ls()) #Ensure empty workspace
here() #Check here location

################## LOAD DATASETS #################
#Get file names of severity data
monthFileNames <- list.files(path = here("data", "climate", "GEE_terraclimate_prewrangled"), 
                               pattern ="MonthlyMeans*", full.names = TRUE)

yearVarFileNames <- list.files(path = here("data", "climate", "GEE_terraclimate_prewrangled"), 
                               pattern ="AllVariables*", full.names = TRUE)

#Create SpatRasterDataset of monthly rasters
monthMeans <- sds(monthFileNames) #Can load list of file names directly to spatrasterdataset
names(monthMeans) <- month.name

#Create SpatRasterDataset of yearly variable rasters
yearVarData <- sds(yearVarFileNames)
names(yearVarData) <- as.character(seq(1958, 2021))

#Compare rasters of both types to ensure same geometries
compareGeom(monthMeans$January, yearVarData$`1958`)


################ CALCULATE ANOMALY ###############

#Function to calculate anomalies when provided with a single raster from yearVarData (data for one year)
calc.year.anomalies <- function(year) {
  #Set up raster to store outputs from calculations
  extent = ext(year$pr_min)
  dims = dim(year$pr_min)
  projection = crs(year$pr_min)
  outputs <- rast(nrows = dims[1], ncols = dims[2], nlyrs = 6)
  ext(outputs) <- extent
  crs(outputs) <- projection
  names(outputs) <- c('pr_anom', 'pdsi_anom', 'soil_anom', 'tmmx_anom', 'vpd_anom', 'def_anom')
  
  #Calculate anomalies for all variables for all months of the year and store in 'outputs' variable
  for (month in 1:12) {
    outputs$pr_anom <- ifel(year$pr_month == month, year$pr_min - monthMeans[month]$pr_mean, outputs$pr_anom)
    outputs$pdsi_anom <- ifel(year$pdsi_month == month, year$pdsi_min - monthMeans[month]$pdsi_mean, outputs$pdsi_anom)
    outputs$soil_anom <- ifel(year$soil_month == month, year$soil_min - monthMeans[month]$soil_mean, outputs$soil_anom)
    outputs$tmmx_anom <- ifel(year$tmmx_month == month, year$tmmx_max - monthMeans[month]$tmmx_mean, outputs$tmmx_anom)
    outputs$vpd_anom <- ifel(year$vpd_month == month, year$vpd_max - monthMeans[month]$vpd_mean, outputs$vpd_anom)
    outputs$def_anom <- ifel(year$def_month == month, year$def_max - monthMeans[month]$def_mean, outputs$def_anom)
    print(paste("Month", month, "done"))
  }
  print("Year is done")
  return(outputs)
}

#Run function
anomalies <- lapply(yearVarData, calc.year.anomalies) %>% sds()
names(anomalies) <- as.character(seq(1958, 2021))

#Load output anomaly geotiffs
anomalies <- sds(list.files(path = here("data", "climate", "anomaly_outputs"), 
                            pattern ="*anomalies.tif", full.names = TRUE))
names(anomalies) <- as.character(seq(1958, 2021))



############## STANDARDIZE ANOMALIES ################

#Calculate standardized z-score = ((value - set_mean) / set_standard_deviation)

set.seed(0)
r <- rast(nrows=10, ncols=10, nlyrs=3)
values(r) <- runif(ncell(r) * nlyr(r))
values(r) <- rep(1:3, each = ncell(r))
plot(r)
x <- mean(r)
plot(x)
# note how this returns one layer
x <- sum(c(r[[1]], r[[2]]), 5)
plot(x)
# and this returns three layers
y <- sum(r, r[[2]], 5)
plot(y)

anomalies$`1958`$pr_anom

length(anomalies)

compiled <- anomalies$`1958`$pr_anom
for (yr in 2:length(anomalies)) {
  year <- anomalies[yr]
  compiled <- sds(compiled, year$pr_anom)
}


#Function to get the mean and sd of a set of years each containing one variable
get.mean.sd <- function(allyr) {
  for (i in 1:length(allyr)) {
    yr <- as.character(seq(1958, 2021))[i]
    names(allyr[[i]]) <- yr
  }
  allyr <- rast(allyr)
  mean <- mean(allyr)
  names(mean) <- "mean"
  sd <- stdev(allyr)
  names(sd) <- "sd"
  return(rast(c(mean, sd)))
}

precip <- map(as.list(anomalies), ~.x$pr_anom) %>% get.mean.sd(pr_anom)




# allPr <- map(as.list(anomalies), ~.x$pr_anom) #Here we have to turn anomalies back into a list in order to map over it and select the bands by variable name
# for (i in 1:length(allPr)) {
#   yr <- as.character(seq(1958, 2021))[i]
#   names(test[[i]]) <- yr
# }
# allV <- rast(test)
# mean <- mean(allV)
# names(mean) <- "mean"
# sd <- stdev(allV)
# names(sd) <- "sd"
# r <- rast(c(mean, sd))



###################### WRITE ALL OUTPUTS ##################

#Write all rasters as geotiffs
for (i in  1:length(names(anomalies))) {
  writeRaster(anomalies[i], here("data", "climate", "anomaly_outputs", 
                                 paste(names(anomalies)[i], '_anomalies.tif', sep="")))
  # writeRaster(anomaliesStandard[i], here("data", "climate", "anomaly_outputs", 
  #                                        paste(names(anomaliesStandard)[i], '_anomalies_Standard.tif', sep="")))
}

print("RASTERS WRITTEN")
# 
# #Load output geotiffs if starting from new load
# anomalies <- sds(list.files(path = here("data", "climate", "anomaly_outputs"), 
#                             pattern ="*anomalies.tif", full.names = TRUE))
# names(anomalies) <- as.character(seq(1958, 2021))
# 
# anomaliesStandard <- sds(list.files(path = here("data", "climate", "anomaly_outputs"), 
#                             pattern ="*Standard.tif", full.names = TRUE))
# names(anomaliesStandard) <- as.character(seq(1958, 2021))








