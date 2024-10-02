library(terra)


# A function to mosaic all outputs in a directory into a single tif file
# This function assumes that data has been output by Google Earth Engine and was too large to export as a single file.
# This means that each input tif will have -##########-########## identifiers at the end of the file name. 
# The function assumes that each unique naming pattern is meant to be put into a different band of the resulting tif
# e.g. if a tif name is: landfire_fire_events_1999_CBI_bc-0000000000-0000000000.tif, that
# landfire_fire_events_1999_CBI_bc is meant to be the band name
# PARAMETERS
# inDir : directory holding GEE files
# outDir : directory to write combined tif to
# outFile : name of new combined tif, e.g. 'cbi.tif'
# dataType : the datatype to write out the file as, as a string (e.g. "INT1U", "FLT4S" etc)
# cores : the number of cores to use for parallel processing
mosaic.gee.outputs <- function(inDir, outDir, outFile, dataType, cores) {
  # List all TIFF files in the input directory
  tifFiles <- list.files(inDir, pattern = "\\.tif$", full.names = TRUE)
  
  # Extract unique patterns (band names) from file names
  uniquePatterns <- unique(sapply(tifFiles, function(x) {
    sub("-\\d+-\\d+\\.tif$", "", basename(x))
  }))
  
  # Function to mosaic all with same pattern
  mosaic.all.pattern <- function(pattern) {
    # Find all files that match the current pattern
    patternFiles <- tifFiles[grepl(pattern, tifFiles)]
    
    # Read and mosaic files for the current pattern
    rasters <- lapply(patternFiles, terra::rast)
    mosaicRaster <- do.call(terra::mosaic, rasters)
    
    #return(terra::wrap(mosaicRaster))
    return(mosaicRaster)
  }
  
  # #mosaic in parallel
  # if(cores == 1) {
  #   future::plan("sequential")
  # } else {
  #   future::plan("multisession", workers = cores)
  # }
  #rasterList <- uniquePatterns |> furrr::future_map(mosaic.all.pattern)
  rasterList <- uniquePatterns |> purrr::map(mosaic.all.pattern)
  
  
  future::plan("sequential")
  
  # Combine all mosaicked rasters into a single SpatRaster
  #rasterList <- rasterList |> purrr::map(terra::unwrap)
  combinedRaster <- terra::rast(rasterList)
  
  # Set band names based on unique patterns
  names(combinedRaster) <- uniquePatterns
  
  return(combinedRaster)
  # # Write the combined raster to a file in the output directory
  # terra::writeRaster(combinedRaster,
  #                    filename = file.path(outDir, outFile),
  #                    overwrite = TRUE,
  #                    datatype = dataType)
}


here::here()

landfirePolygon <- mosaic.gee.outputs(inDir = here::here('data/derived/landfire_polygon_cbi_bc'),
                   outDir = here::here('data/derived/'),
                   outFile = "landfire_fire_events_cbi_bc.tif",
                   dataType = "",
                   cores = 8)

# Write the combined raster to a file in the output directory
terra::writeRaster(landfirePolygon,
                   filename = here::here('data/derived/landfire_fire_events_cbi_bc.tif'),
                   overwrite = TRUE,
                   gdal=c("COMPRESS=DEFLATE"))



######################
#RASTER
landfireRaster <- mosaic.gee.outputs(inDir = here::here('data/derived/landfire_raster_cbi_bc'),
                           outDir = here::here('data/derived/'),
                           outFile = "landfire_fire_raster_cbi_bc.tif",
                           dataType = "",
                           cores = 8)

# Write the combined raster to a file in the output directory
terra::writeRaster(landfireRaster,
                   filename = here::here('data/derived/landfire_fire_raster_cbi_bc.tif'),
                   overwrite = TRUE,
                   gdal=c("COMPRESS=DEFLATE"))






tifFiles <- list.files(here::here('data/derived/landfire_polygon_cbi_bc'), pattern = "\\.tif$", full.names = TRUE)
x <- terra::rast(tifFiles[1])


terra::writeRaster(x,
                   filename = here::here('data/derived/TEST2_landfire_fire_events_cbi_bc.tif'),
                   overwrite = TRUE,
                   gdal=c("COMPRESS=DEFLATE"))