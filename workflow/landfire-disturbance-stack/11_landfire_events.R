
require(here)
require(sf)

landfireEvents <- sf::st_read(here::here('data', 'raw', 'LF_Public_Events_1999_2022', 'LF_Public_Events_1999_2022.gdb'),
                     layer = "CONUS_230_PublicModelReadyEvents")

landfireFireEvents <- landfireEvents |> dplyr::filter(Event_Type == "Wildfire" | 
                                                      Event_Type == "Wildland Fire Use" |
                                                      Event_Type == "Prescribed Fire" |
                                                      Event_Type == "Wildland Fire" |
                                                      Event_Type == "Fire")

rm(landfireEvents)

head(landfireFireEvents)



#Function to write a shapefile to a new, file-specific directory and add a zipped version
#    shp = the sf file to write to shapefile
#    location = path of directory to create the new file-specific subdirectory in
#    filename = name of file WITHOUT .shp
#    zipOnly = TRUE / FALSE, should the original (unzipped) files be removed?

# Example use:
# st_write_shp(shp = prepped_for_parks_etal,
#              location = here("data/derived"),
#              filename = "career_lba_for_parks_v1",
#              zipOnly = TRUE)
st_write_shp <- function(shp, location, filename, zipOnly) {
  
  #Create subdirectory
  outDir <- here::here(location, filename)
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  
  
  
  #Write file
  sf::st_write(shp,
               here::here(outDir, paste(filename, ".shp", sep = "")),
               append = FALSE) #overwrite
  
  #Get all shapefile components
  allShpNms <- list.files(here::here(outDir),
                          pattern = paste(filename, "*", sep = ""),
                          full.names = TRUE)
  
  #Zip together
  zip::zip(zipfile = here::here(outDir, paste(filename, ".zip", sep="")),
           files = allShpNms,
           mode = "cherry-pick")
  
  
  #Remove raw files if desired
  if(zipOnly == TRUE) {
    file.copy(here(outDir, paste(filename, ".zip", sep="")), here::here(location, paste(filename, ".zip", sep="")))
    unlink(here(outDir), recursive = TRUE)          
  }
  
}

st_write_shp(shp = landfireFireEvents,
             location = here::here("data/derived"),
             filename = "landfire_fire_events",
             zipOnly = TRUE)



x <- sf::st_read(here::here("data/derived"))


# fireStack <- terra::rast(here::here('data','derived','fire_dist_stack_west.tif'))
# 
# x <- fireStack$fire_disturbance_1999
# xx <- landfireFireEvents |> dplyr::filter(year == 1999)


#PUT THEM OVER TOP OF ONE ANOTHER AND MAKE SURE THEY DON'T MISS ANYTHING

#CREATE UNIQUE IDENTIFIER RASTERS


head(landfireEvents)

