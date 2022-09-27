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
#MISTAKE MADE IN GEE SCRIPT: MONTHS ARE (1, 2, ...12) RATHER THAN (01, 02, ... 12) IN FILENAMES
#FIX ORDER OF FILES


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

#calculate anomaly for one year first, test method. Then apply over entire list

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


############## STANDARDIZE ANOMALIES ################

#Create empty vectors
allPrAnom <- c()
allPdsiAnom <- c()
allSoilAnom <- c()
allTmmxAnom <- c()
allVpdAnom <- c()
allDefAnom <- c()

#Collate all values for each variable across all years
for (i in 1:length(anomalies)) {
  allPrAnom <- append(allPrAnom, values(anomalies[i]$pr_anom, na.rm=TRUE))
  allPdsiAnom <- append(allPdsiAnom, values(anomalies[i]$pdsi_anom, na.rm=TRUE))
  allSoilAnom <- append(allSoilAnom, values(anomalies[i]$soil_anom, na.rm=TRUE))
  allTmmxAnom <- append(allTmmxAnom, values(anomalies[i]$tmmx_anom, na.rm=TRUE))
  allVpdAnom <- append(allVpdAnom, values(anomalies[i]$vpd_anom, na.rm=TRUE))
  allDefAnom <- append(allDefAnom, values(anomalies[i]$def_anom, na.rm=TRUE))
}

#Calculate overarching means & standard deviations for each variable, to use when calculating z-scores
anomAvgs <- c(mean(allPrAnom), mean(allPdsiAnom), mean(allSoilAnom), mean(allTmmxAnom), mean(allVpdAnom), mean(allDefAnom))
names(anomAvgs) <- c('pr', 'pdsi', 'soil', 'tmmx', 'vpd', 'def')
anomSds <- c(sd(allPrAnom), sd(allPdsiAnom), sd(allSoilAnom), sd(allTmmxAnom), sd(allVpdAnom), sd(allDefAnom))
names(anomSds) <- c('pr', 'pdsi', 'soil', 'tmmx', 'vpd', 'def')


#Standardize values in each raster to z-scores based on overarching means & standard deviations
standardize.anomalies <- function(year) {
  for (i in 1:length(names(year))) { #FOR each variable we have calculated anomalies for
    year[[i]] <- ((year[[i]] - anomAvgs[i]) / anomSds[i]) #Calculate standardized z-score = ((value - set_mean) / set_standard_deviation)
  }
  return(year)
  # year$pr_anom <- (year$pr_anom - anomAvgs['pr']) / anomSds['pr']
  # year$pdsi_anom <- (year$pdsi_anom - anomAvgs[2]) / anomSds['pdsi']
  # year$soil_anom <- (year$soil_anom - anomAvgs[3]) / anomSds['soil']
  # year$tmmx_anom <- (year$tmmx_anom - anomAvgs[4]) / anomSds['tmmx']
  # year$vpd_anom <- (year$vpd_anom - anomAvgs[5]) / anomSds['vpd']
  # year$def_anom <- (year$def_anom - anomAvgs[6]) / anomSds['def']
}

#Apply standardiation function to all years
anomaliesStandard <- lapply(anomalies, standardize.anomalies) %>% sds()
names(anomaliesStandard) <- as.character(seq(1958, 2021))


#Write all rasters as geotiffs
for (i in  1:length(names(anomalies))) {
  writeRaster(anomalies[i], here("data", "climate", "anomaly_outputs", 
                                 paste(names(anomalies)[i], '_anomalies.tif', sep="")))
  writeRaster(anomaliesStandard[i], here("data", "climate", "anomaly_outputs", 
                                         paste(names(anomaliesStandard)[i], '_anomalies_Standard.tif', sep="")))
}

#Load output geotiffs if starting from new load
anomalies <- sds(list.files(path = here("data", "climate", "anomaly_outputs"), 
                            pattern ="*anomalies.tif", full.names = TRUE))
names(anomalies) <- as.character(seq(1958, 2021))

anomaliesStandard <- sds(list.files(path = here("data", "climate", "anomaly_outputs"), 
                            pattern ="*Standard.tif", full.names = TRUE))
names(anomaliesStandard) <- as.character(seq(1958, 2021))









################# DEBUGGING SPACE ##############


##########CHECK MONTH AVG DATA IS GOOD
monthFileNames

#Location points used in GEE, put into 2-column matrix form as required by terra::extract
wypt <- t(as.matrix(c(-110.786763, 43.432451)))
copt <- t(as.matrix(c(-105.24249040058604,40.00981039217258)))
capt <- t(as.matrix(c(-122.170020, 37.428193)))
wapt <- t(as.matrix(c(-122.486757, 48.733972)))

#Function to extract data from monthly avgs
extract.monthavgs.to.point <- function(pt) {
  anoms <- monthMeans %>% 
    lapply(extract, pt, method = "simple") %>%
    bind_rows() %>% 
    cbind(1:12) %>% 
    rename(month = `1:12`)
  return(anoms)
}

#Run function for all points
wy_test <- wypt %>% extract.monthavgs.to.point()
co_test <- copt %>% extract.monthavgs.to.point()
ca_test <- capt %>% extract.monthavgs.to.point()
wa_test <- wapt %>% extract.monthavgs.to.point()

##########CHECK YEAR MIN/MAX/MONTH AVG DATA IS GOOD

#Function to extract data from full dataset anomalies
extract.yearvardata.to.point <- function(pt) {
  anoms <- yearVarData %>% 
    lapply(extract, pt, method = "simple") %>%
    bind_rows() %>% 
    cbind(1958:2021) %>% 
    rename(year = `1958:2021`)
  return(anoms)
}

wy_test <- wypt %>% extract.yearvardata.to.point()
co_test <- copt %>% extract.yearvardata.to.point() ##### CO_test here is full of NANs?!?!
ca_test <- capt %>% extract.yearvardata.to.point()
wa_test <- wapt %>% extract.yearvardata.to.point()

plot(yearVarData$`1958`$pr_min)
points(copt)
points(wypt)
points(capt)
points(wapt)

plot(monthMeans$January$tmmx_mean)
points(copt)
points(wypt)
points(capt)
points(wapt)

###################

# 
# print(end$`1958`$pr_anom)
# print(endStandard$`1958`$pr_anom)
# 
# 
# plot(end$`1958`$pdsi_anom)
# plot(endStandard$`1958`$pdsi_anom)
# 


####################
# #Test rasters
# precip <- rast(nrows=2, ncols=2, xmin=0, xmax=2, ymin=0, ymax=2)
# values(precip) <- c(3,6,2,9)
# plot(precip)
# pmonth <- rast(nrows=2, ncols=2, xmin=0, xmax=2, ymin=0, ymax=2)
# values(pmonth) <- c(1,2,2,1)
# plot(pmonth)
# v <- sds(precip, pmonth)
# names(v) <- c('p', 'pm')
# 
# m1av <- rast(nrows=2, ncols=2, xmin=0, xmax=2, ymin=0, ymax=2)
# values(m1av) <- c(2, 3, 3, 2)
# plot(m1av)
# m2av <- rast(nrows=2, ncols=2, xmin=0, xmax=2, ymin=0, ymax=2)
# values(m2av) <- c(20, 30, 30, 20)
# plot(m2av)
# av <- sds(m1av, m2av)
# names(av) <- c('j', 'f')
# 
# test <- rast(nrows=dim(m1av)[1], ncols = dim(m1av)[2])
# ext(test) <- ext(m1av)
# for (month in 1:2) {
#   test <- ifel(v$pm == month, v$p - av[month], test)
# }
# 
# plot(test)



