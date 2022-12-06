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
# 
# anomaliesStandard <- sds(list.files(path = here("data", "climate", "anomaly_outputs"), 
#                                     pattern ="*Standard.tif", full.names = TRUE))
# names(anomaliesStandard) <- as.character(seq(1958, 2021))


#Load CSV of testing data & perform preparatory manipulations
rawPtData <- read.csv(here("data", "climate", "locations_pull_raw_terra_data.csv"))
rawPtData <- rawPtData %>% 
  rename("yearmonth" = "month") %>% 
  mutate(yearmonth = as.character(yearmonth)) %>%
  mutate(year = substr(yearmonth, 1, 4)) %>%
  mutate(month = substr(yearmonth, 5, 6))
glimpse(rawPtData)


################ PERFORM CSV ANALYSIS TO CHECK GEOTIFFS AGAINST #################

#Location points used in GEE, put into 2-column matrix form as required by terra::extract
wypt <- t(as.matrix(c(-110.786763, 43.432451)))
copt <- t(as.matrix(c(-105.242490, 40.009810)))
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
#Uses fx add.anomaly
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
    rename(soil_anom = anomaly) #%>% 
  #  mutate(soil_anom = soil_anom * -1)
  pr_anom <- add.anomaly(min.month(dats, pr), avgs, pr, pravg) %>% 
    rename(pr_anom = anomaly) #%>% 
  #  mutate(pr_anom = pr_anom * -1)
  pdsi_anom <- add.anomaly(min.month(dats, pdsi), avgs, pdsi, pdsiavg) %>% 
    rename(pdsi_anom = anomaly) #%>% 
  #  mutate(pdsi_anom = pdsi_anom * -1)
  
  #Join all on year column
  anoms <- tmmx_anom %>%
    left_join(vpd_anom) %>%
    left_join(def_anom) %>%
    left_join(soil_anom) %>%
    left_join(pr_anom) %>%
    left_join(pdsi_anom)
}

#For testing purposes
wyCsvAvgs <- calc.month.avgs(wy)
caCsvAvgs <- calc.month.avgs(ca)
coCsvAvgs <- calc.month.avgs(co)
waCsvAvgs <- calc.month.avgs(wa)

#Run for all locations
wyCsvAnoms <- get.all.anomalies(wy)
coCsvAnoms <- get.all.anomalies(co)
caCsvAnoms <- get.all.anomalies(ca)
waCsvAnoms <- get.all.anomalies(wa)

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
wyGeotAnoms <- wypt %>% extract.anomalies.to.point()
coGeotAnoms <- copt %>% extract.anomalies.to.point()
caGeotAnoms <- capt %>% extract.anomalies.to.point()
waGeotAnoms <- wapt %>% extract.anomalies.to.point()


############### COMPARE ANOMALIES ###############

#Function to test anomalies from both analyses
test.comparison.at.point <- function(csvpt, geotpt) {
  print(paste("TMMX compare: ", sum(csvpt$tmmx_anom == geotpt$tmmx_anom), " True of ", length(csvpt$year), sep=""))
  print(csvpt$tmmx_anom == geotpt$tmmx_anom)
  print(paste("VPD compare: ", sum(csvpt$vpd_anom == geotpt$vpd_anom), " True of ", length(csvpt$year), sep=""))
  print(csvpt$vpd_anom == geotpt$vpd_anom)
  print(paste("DEF compare: ", sum(csvpt$def_anom == geotpt$def_anom), " True of ", length(csvpt$year), sep=""))
  print(csvpt$def_anom == geotpt$def_anom)
  print(paste("SOIL compare: ", sum(csvpt$soil_anom == geotpt$soil_anom), " True of ", length(csvpt$year), sep=""))
  print(csvpt$soil_anom == geotpt$soil_anom)
  print(paste("PR compare: ", sum(csvpt$pr_anom == geotpt$pr_anom), " True of ", length(csvpt$year), sep=""))
  print(csvpt$pr_anom == geotpt$pr_anom)
  print(paste("PDSI compare: ", sum(csvpt$pdsi_anom == geotpt$pdsi_anom), " True of ", length(csvpt$year), sep=""))
  print(csvpt$pdsi_anom == geotpt$pdsi_anom)
}

#Run for each site - TAKEAWAY: CO = NA issue, others show that there are the multiple-month min/max issue
test.comparison.at.point(wyCsvAnoms, wyGeotAnoms) #Soil many false
soilWy <- min.month(wy, soil) #due to 'what happens when there are equal min/max' issue - notice multiple entries for each year
view(soilWy)
#How big are the anomaly differences caused by the month min/max issue?
examineWySoil <- cbind(1958:2021, wyCsvAnoms$soil_anom, wyGeotAnoms$soil_anom) %>% 
  as.tibble() %>% 
  rename("Year" = V1, "ManualWCorrection" = V2, "FromGeotiffsNoCorrection" = V3)
view(examineWySoil) #Actually fairly large. The CSV data (corrected for maximum anomaly) does have significantly higher anomalies

test.comparison.at.point(waCsvAnoms, waGeotAnoms)

test.comparison.at.point(caCsvAnoms, caGeotAnoms) #PR many false
prCa <- min.month(ca, pr) #due to 'what happens when there are equal min/max' issue - notice multiple entries for each year
view(prCa)
#How big are the anomaly differences caused by the month min/max issue?


test.comparison.at.point(coCsvAnoms, coGeotAnoms) #coGeoTAnoms is full of NAs!

#Check that point is over what has been exported for use
plot(anomalies$`1958`$tmmx_anom, xlim=c(-105.2, -105.5), ylim=c(39.9, 40.1))
points(copt) #Appears so
extract(anomalies$`1958`$tmmx_anom, copt)

leaflet() %>% addTiles() %>% addRasterImage(anomalies$`1958`$tmmx_anom) %>% addMarkers(copt)



################ PERFORM GEOTIFF ANALYSIS TO CHECK STEPS #################


##########CHECK MONTH AVG DATA IS GOOD
monthFileNames <- list.files(path = here("data", "climate", "GEE_terraclimate_prewrangled"),
                             pattern ="MonthlyMeans*", full.names = TRUE)
monthFileNames #The order was wrong before, correct now

#Create SpatRasterDataset of monthly rasters
monthMeans <- sds(monthFileNames) #Can load list of file names directly to spatrasterdataset
names(monthMeans) <- month.name

#Function to extract data from monthly avgs
extract.monthavgs.to.point <- function(pt) {
  loc <- monthMeans %>%
    lapply(extract, pt, method = "simple") %>%
    bind_rows() %>%
    cbind(1:12) %>%
    rename(month = `1:12`)
  return(loc)
}

#Run function for all points
wyGeotAvgs <- wypt %>% extract.monthavgs.to.point()
coGeotAvgs <- copt %>% extract.monthavgs.to.point() ##### CO here is full of NANs
caGeotAvgs <- capt %>% extract.monthavgs.to.point()
waGeotAvgs <- wapt %>% extract.monthavgs.to.point()


##########CHECK YEAR MIN/MAX/MONTH AVG DATA IS GOOD
yearVarFileNames <- list.files(path = here("data", "climate", "GEE_terraclimate_prewrangled"),
                               pattern ="AllVariables*", full.names = TRUE)
#Create SpatRasterDataset of yearly variable rasters
yearVarData <- sds(yearVarFileNames)
names(yearVarData) <- as.character(seq(1958, 2021))

#Function to extract data from full dataset anomalies
extract.yearvardata.to.point <- function(pt) {
  anoms <- yearVarData %>%
    lapply(extract, pt, method = "simple") %>%
    bind_rows() %>%
    cbind(1958:2021) %>%
    rename(year = `1958:2021`)
  return(anoms)
}

wyGeotRaw <- wypt %>% extract.yearvardata.to.point()
coGeotRaw <- copt %>% extract.yearvardata.to.point() ##### CO here is full of NANs?!?!
caGeotRaw <- capt %>% extract.yearvardata.to.point()
waGeotRaw <- wapt %>% extract.yearvardata.to.point()




#Examine the min/max month issue
view(soilWy)
view(wyGeotRaw)
view(examineWySoil)
#It appears that the GEE min/max reducers are pulling whichever month is numerically first
#Would need to fix this in GEE, or pull raw TerraClimate with R...



#Plot anomaly changes 
precipCa <- min.month(ca, pr) #due to 'what happens when there are equal min/max' issue - notice multiple entries for each year

examineCaPr <- cbind(1958:2021, caCsvAnoms$pr_anom, caGeotAnoms$pr_anom) %>% 
  as.tibble() %>% 
  rename("Year" = V1, "SelectHighAnom" = V2, "AutoSelect" = V3)

#Create tidy dataset
tidyCaPr <- examineCaPr %>% pivot_longer(
                                          cols = c(SelectHighAnom, AutoSelect),
                                          names_to = "Type",
                                          values_to = "Anomaly"
                                          )
#Plot
ggplot(tidyCaPr) + 
  aes(x = Year, y = Anomaly, fill=Type) +
  geom_col(position = "dodge") + 
  labs(title = "Impact of multi-month max/min treatment in anomaly calculations")
ggsave(filename = "multi_month_minmax_anomalies.jpeg", path = here("figs"))

write.csv(precipCa, file = here("data", "climate", "CA_Precip_Mins.csv"))
