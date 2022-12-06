#Climate disturbance data analysis - test anomaly calculations for a single set of points
#Tyler McIntosh, 9/9/2022


#Data inputs for this script were wrangled in GEE,
#copy of script found in RProject workflow document 02_ClimateDataWrangling.js.

#Anomaly data to test against was calculated in R, script found in RProject workflow document 
#03_ClimateDisturbanceDataAnalysis.R

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
library(radiant.data) #For calculation of SD w/o bessel correction (sdpop)

###Clean workspace
rm(list=ls()) #Ensure empty workspace
here() #Check here location
options(digits = 10) #Set standard decimal print output

################## LOAD DATASETS #################

#Load CSV of testing data & perform preparatory manipulations
rawPtData <- read.csv(here("data", "climate", "locations_pull_raw_terra_data.csv"))
rawPtData <- rawPtData %>% 
  rename("yearmonth" = "month") %>% 
  mutate(yearmonth = as.character(yearmonth)) %>%
  mutate(year = substr(yearmonth, 1, 4)) %>%
  mutate(month = substr(yearmonth, 5, 6))
glimpse(rawPtData)


################ PERFORM CSV ANALYSIS TO CHECK AGAINST GEE ANOMALIES #################

#Location points used in GEE, put into 2-column matrix form as required by terra::extract
capt <- t(as.matrix(c(-122.170020, 37.428193)))
copt <- t(as.matrix(c(-105.242490, 40.009810)))
wapt <- t(as.matrix(c(-122.486757, 48.733972)))
wypt <- t(as.matrix(c(-110.786763, 43.432451)))

#Split into datasets for each location
ca <- rawPtData %>% filter(plot_id == "StanfordCA")
co <- rawPtData %>% filter(plot_id == "BoulderCO")
wa <- rawPtData %>% filter(plot_id == "BellinghamWA")
wy <- rawPtData %>% filter(plot_id == "JacksonWY")

#Create list to map over & clean up
rawPtData <- list(ca, co, wa, wy)
names(rawPtData) <- c('ca', 'co', 'wa', 'wy')
rm(wy, co, ca, wa)

#Variable names & years to map/iterate over
varNames <- c('tmmx', 'vpd', 'def', 'pr', 'soil', 'pdsi')
posVars <- c('tmmx', 'vpd', 'def')
negVars <- c('pr', 'soil', 'pdsi')
years <- as.character(c(1958:2021))

####### FUNCTIONS #######
#Function to calculate monthly averages
calc.month.avgs <- function(dats) {
  x <- dats %>% dplyr::group_by(month) %>% 
    dplyr::summarise(
      tmmxavg = mean(tmmx),
      vpdavg = mean(vpd),
      defavg = mean(def),
      pravg = mean(pr),
      soilavg = mean(soil),
      pdsiavg = mean(pdsi)
    )
  return(x)
}

#Function to pull the minimum and maximum averages for each variable,
#as well as the associated month
min.max.month <- function(dats) {
  minMax <- data.frame()
  for(variable in varNames) {
    varAvgName <- paste(variable, 'avg', sep='')
    min <- dats %>%
      select(month, {{ varAvgName }} ) %>%
      slice_min(.data[[varAvgName]]) %>%
      cbind(c(variable)) %>%
      cbind(c('min'))
    names(min) <- c('month', 'value', 'var', 'minmax')
    max <- dats %>%
      select(month, {{ varAvgName }} ) %>%
      slice_max(.data[[varAvgName]]) %>%
      cbind(c(variable)) %>%
      cbind(c('max'))
    names(max) <- c('month', 'value', 'var', 'minmax')
    minMax <- rbind(minMax, rbind(min, max))
  }
  return(minMax)
}

#Function to calculate anomalies given the raw data and minMax data for a single location
calculate.anomalies <- function(rawDats, minMax) {
  #Initialize data frame for storage of all anomalies
  anomalies <- data.frame(year = character(), 
                          tmmx = double(),
                          vpd = double(),
                          def = double(),
                          pr = double(),
                          soil = double(),
                          pdsi = double())
  #Calculate anomalies
  for (yr in years) { #For each year
    print(paste("Calculating anomalies for", yr))
    rawDatsThisYr <- rawDats %>%
      filter(year == yr)
    storage <- c(as.double(yr)) #store this year's anomalies. Use double type
    for (v in posVars) { #For each positive variable
      vMax <- minMax %>%
        filter(var == v) %>%
        filter(minmax == 'max')
      mon <- vMax$month[1]
      avgMax <- vMax$value[1]
      rawD <- rawDatsThisYr %>%
        filter(month == mon)
      vals <- rawD %>% select( {{ v }} )
      val <- as.double(vals[1])
      anom <- (val - avgMax)
      storage <- storage %>% append(anom)
    }
    for (v in negVars) { #For each negative variable
      vMin <- minMax %>%
        filter(var == v) %>%
        filter(minmax == 'min')
      mon <- vMin$month[1]
      avgMin <- vMin$value[1]
      rawD <- rawDatsThisYr %>%
        filter(month == mon)
      vals <- rawD %>% select( {{ v }} )
      val <- as.double(vals[1])
      anom <- (val - avgMin)
      storage <- storage %>% append(anom)
    }
    anomalies <- rbind(anomalies, storage)
    names(anomalies) <- c('year', 'tmmx', 'vpd', 'def', 'pr', 'soil', 'pdsi')
  }
  return(anomalies)
}

#Function to calculate Z-scores for a vector
standardize.vector <- function(vec) {
  avg <- mean(vec)
  deviation <- sd(vec)
  zScores <- (vec - avg) / deviation
}

#Function to calculate Z-scores for a vector w/o bessel correction
standardizeN.vector <- function(vec) {
  avg <- mean(vec)
  deviation <- sdpop(vec)
  zScores <- (vec - avg) / deviation
}

#Function to calculate column-wise Z-scores for all variables included in 'varNames' in a dataframe
standardize.dataframe <- function(dats) {
  zScores <- dats %>% 
    select(all_of(varNames)) %>% #select only variables
    map(standardize.vector) %>% #standardize each column
    data.frame() %>% #coerce back to data.frame from list
    as_tibble() %>%
    cbind(year = years)
  return(zScores)
}

#Function to calculate column-wise Z-scores for all variables included in 'varNames' in a dataframe
#W/o bessel correction
standardizeN.dataframe <- function(dats) {
  zScores <- dats %>% 
    select(all_of(varNames)) %>% #select only variables
    map(standardizeN.vector) %>% #standardize each column
    data.frame() %>% #coerce back to data.frame from list
    as_tibble() %>%
    cbind(year = years)
  return(zScores)
}

####### CALL FUNCTIONS #######
#Calculate monthly averages
monthAvgs <- rawPtData %>% map(calc.month.avgs)

#Pull month min max information
monthMinMax <- monthAvgs %>% map(min.max.month)

#Calculate anomalies
allAnomalies <- list(rawPtData, monthMinMax) %>% pmap(calculate.anomalies)

#Standardize anomalies
allAnomaliesStandardized <- allAnomalies %>% map(standardize.dataframe)
allAnomaliesStandardizedN <- allAnomalies %>% map(standardizeN.dataframe)


######### COMPARE ANOMALIES ######

#Function to clean up data from GEE
clean.up.dats <- function(dats) {
  d <- dats %>%
    select(first, plot_id, variable, year) %>%
    rename(value = first, location = plot_id) %>%
    spread(variable, value)
  return(d)
}

#Function to compare datasets
compare.dats <- function(dats1, dats2) {
  print(typeof(dats1$tmmx[1]))
  print(typeof(dats2$tmmx[1]))
  print(paste('tmmx', identical(dats1$tmmx, dats2$tmmx)))
  print(paste('vpd', identical(dats1$vpd, dats2$vpd)))
  print(paste('pr', identical(dats1$pr, dats2$pr)))
  print(paste('pdsi', identical(dats1$pdsi, dats2$pdsi)))
  print(paste('def', identical(dats1$def, dats2$def)))
  print(paste('soil', identical(dats1$soil, dats2$soil)))
}

#Read data in
allGEERawAnoms <- read.csv(here("data", "climate", "GEE_locations_calculated_raw_anomalies.csv"))
allGEEZAnoms <- read.csv(here("data", "climate", "GEE_locations_calculated_z_anomalies.csv"))

#Clean data
cleanRawAnoms <- allGEERawAnoms %>% clean.up.dats()
cleanZAnoms <- allGEEZAnoms %>% clean.up.dats()

#Run comparisons
print("Raw Anomaly Comparisons")
compare.dats(cleanRawAnoms %>% filter(location == 'JacksonWY'), allAnomalies$wy)
compare.dats(cleanRawAnoms %>% filter(location == 'BoulderCO'), allAnomalies$co)
compare.dats(cleanRawAnoms %>% filter(location == 'StanfordCA'), allAnomalies$ca)
compare.dats(cleanRawAnoms %>% filter(location == 'BellinghamWA'), allAnomalies$wa)

#Standardized anomalies are NOT the same -- slight differences?
print("Standardized Anomaly Comparisons")
compare.dats(cleanZAnoms %>% filter(location == 'JacksonWY'), allAnomaliesStandardized$wy)
compare.dats(cleanZAnoms %>% filter(location == 'BoulderCO'), allAnomaliesStandardized$co)
compare.dats(cleanZAnoms %>% filter(location == 'StanfordCA'), allAnomaliesStandardized$ca)
compare.dats(cleanZAnoms %>% filter(location == 'BellinghamWA'), allAnomaliesStandardized$wa)

#Check w/o bessel correction in SD
print("w/o Bessel Standardized Anomaly Comparisons")
compare.dats(cleanZAnoms %>% filter(location == 'JacksonWY'), allAnomaliesStandardizedN$wy)
compare.dats(cleanZAnoms %>% filter(location == 'BoulderCO'), allAnomaliesStandardizedN$co)
compare.dats(cleanZAnoms %>% filter(location == 'StanfordCA'), allAnomaliesStandardizedN$ca)
compare.dats(cleanZAnoms %>% filter(location == 'BellinghamWA'), allAnomaliesStandardizedN$wa)
