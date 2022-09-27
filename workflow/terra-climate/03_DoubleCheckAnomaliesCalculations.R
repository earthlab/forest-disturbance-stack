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

###Clean workspace
rm(list=ls()) #Ensure empty workspace
here() #Check here location

################## LOAD DATASETS #################


#Load output anomaly geotiffs
anomalies <- sds(list.files(path = here("data", "climate", "anomaly_outputs"), 
                            pattern ="*anomalies.tif", full.names = TRUE))
names(anomalies) <- as.character(seq(1958, 2021))

anomaliesStandard <- sds(list.files(path = here("data", "climate", "anomaly_outputs"), 
                                    pattern ="*Standard.tif", full.names = TRUE))
names(anomaliesStandard) <- as.character(seq(1958, 2021))


#Load CSV of testing data & perform preparatory manipulations
rawPtData <- read.csv(here("data", "climate", "locations_pull_raw_terra_data.csv"))
rawPtData <- rawPtData %>% 
  rename("yearmonth" = "month") %>% 
  mutate(yearmonth = as.character(yearmonth)) %>%
  mutate(year = substr(yearmonth, 1, 4)) %>%
  mutate(month = substr(yearmonth, 5, 6))
glimpse(rawPtData)

#Location points used in GEE, put into 2-column matrix form as required by terra::extract
wypt <- t(as.matrix(c(-110.786763, 43.432451)))
copt <- t(as.matrix(c(-105.24249040058604,40.00981039217258)))
capt <- t(as.matrix(c(-122.170020, 37.428193)))
wapt <- t(as.matrix(c(-122.486757, 48.733972)))

#Split into datasets for each location
wy <- rawPtData %>% filter(plot_id == "JacksonWY")
co <- rawPtData %>% filter(plot_id == "BoulderCO")
ca <- rawPtData %>% filter(plot_id == "StanfordCA")
wa <- rawPtData %>% filter(plot_id == "BellinghamWA")
rm(rawPtData)


#Function to calculate monthly averages
calc.month.avgs <- function(dats) {
  x <- dats %>% group_by(month) %>% 
    summarise(
      tmmxavg = mean(tmmx),
      vpdavg = mean(vpd),
      defavg = mean(def),
      pravg = mean(pr),
      soilavg = mean(soil),
      pdsiavg = mean(pdsi)
    )
}

#pull months that min/max occurred in
#######NOTE: Some years have multiple maximum values, resulting in multiple months being pulled
#######NEED: To see what is happening in GEE when this happens.
#######QUESTION: What is the best option here? Randomly select one? Month of largest anomaly?

#Functions to get min/max values in a year as well as the month that they occurred in
#For min (neg) variables: soil, pr, pdsi
min.month <- function(dats, varNm) {
  x <- dats %>% group_by(year) %>%
    slice_min(order_by = {{varNm}}) %>%
    select(year, month, {{varNm}})
  return(x)
}

#For max (pos) variables: tmmx, vpd, def
max.month <- function(dats, varNm) {
  x <- dats %>% group_by(year) %>%
    slice_max(order_by = {{varNm}}) %>%
    select(year, month, {{varNm}})
  return(x)
}

#Function to join monthly averages, calculate anomalies, and filter to maximum anomaly months.
#Requires input dataset from min.month or max.month functions, input data from calc.month.avgs,
#and name of variable & variable average columns
add.anomaly <- function(dats, monthAvgs, varNm, varNmAvg) {
  #join monthly averages and calculate anomalies
  x <- dats %>%
    left_join(select(monthAvgs, month, {{varNmAvg}})) %>%
    mutate(anomaly = {{varNm}} - {{varNmAvg}}) %>% #Calculate anomaly
    mutate(anomAbs = abs(anomaly)) #Get absolute value for filtering to max anomaly
  x <- x %>% group_by(year) %>%
    slice_max(order_by = anomAbs) %>% #slice to maximum anomaly
    slice_head(n=1)  %>% #if multiple anomalies are equal, slice to the first one as it no longer matters which is selected
    select(year, anomaly) #select only year & anomaly, drop all other columns
}

#Function to calculate anomalies and bind them all together
get.all.anomalies <- function(dats) {
  #Averages
  avgs <- calc.month.avgs(dats)
  
  #Positive variables
  tmmx_anom <- add.anomaly(max.month(dats, tmmx), avgs, tmmx, tmmxavg) %>% 
    rename(tmmx_anom = anomaly)
  vpd_anom <- add.anomaly(max.month(dats, vpd), avgs, vpd, vpdavg) %>% 
    rename(vpd_anom = anomaly)
  def_anom <- add.anomaly(max.month(dats, def), avgs, def, defavg) %>% 
    rename(def_anom = anomaly)
  
  #Negative variables
  soil_anom <- add.anomaly(min.month(dats, soil), avgs, soil, soilavg) %>% 
    rename(soil_anom = anomaly) %>% 
    mutate(soil_anom = soil_anom * -1)
  pr_anom <- add.anomaly(min.month(dats, pr), avgs, pr, pravg) %>% 
    rename(pr_anom = anomaly) %>% 
    mutate(pr_anom = pr_anom * -1)
  pdsi_anom <- add.anomaly(min.month(dats, pdsi), avgs, pdsi, pdsiavg) %>% 
    rename(pdsi_anom = anomaly) %>% 
    mutate(pdsi_anom = pdsi_anom * -1)
  
  #Join all on year column
  anoms <- tmmx_anom %>%
    left_join(vpd_anom) %>%
    left_join(def_anom) %>%
    left_join(soil_anom) %>%
    left_join(pr_anom) %>%
    left_join(pdsi_anom)
}

#For testing purposes
wy_avgs <- calc.month.avgs(wy)
ca_avgs <- calc.month.avgs(ca)
co_avgs <- calc.month.avgs(co)
wa_avgs <- calc.month.avgs(wa)
co_pr_mmm <- min.month(co, pr)

tmmx_wy_anom <- add.anomaly(max.month(wy, tmmx), wy_avgs, tmmx, tmmxavg)
soil_wy_anom <- add.anomaly(min.month(wy, soil), wy_avgs, soil, soilavg)

#Run for all locations
wy_all <- get.all.anomalies(wy)
co_all <- get.all.anomalies(co)
ca_all <- get.all.anomalies(ca)
wa_all <- get.all.anomalies(wa)

#Extract data from full dataset anomalies
plot(anomalies$`1958`$tmmx_anom)
points(wypt)
points(capt)
points(copt)
points(wapt)

#Function to extract data from full dataset anomalies
extract.anomalies.to.point <- function(pt) {
  anoms <- anomalies %>% 
    lapply(extract, pt, method = "simple") %>%
    bind_rows() %>% 
    cbind(1958:2021) %>% 
    rename(year = `1958:2021`)
  return(anoms)
}

#Run function for all points
wy_test <- wypt %>% extract.anomalies.to.point()
co_test <- copt %>% extract.anomalies.to.point()
ca_test <- capt %>% extract.anomalies.to.point()
wa_test <- wapt %>% extract.anomalies.to.point()
















