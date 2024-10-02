

#This code section loads a personal utilities package (tlmr), and then uses it for package management
if (!requireNamespace("tlmr", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")  # Install 'devtools' if it's not available
  }
  devtools::install_github('TylerLMcIntosh/tlm-r-utility', force = TRUE)
}
library(tlmr)
tlmr::install_and_load_packages(c(
  "terra",
  "tictoc"))

sr <- tlmr::access_data_epa_l3_ecoregions_vsi() |>
  dplyr::filter(US_L3NAME == "Southern Rockies") |>
  dplyr::group_by(US_L3NAME) |>
  dplyr::summarize(geometry = sf::st_union(geometry)) |>
  sf::st_transform("EPSG:4326")


usa <- tigris::states()
west <- usa[usa$STUSPS %in% c("WA", "OR", "CA", "ID", "MT", "WY", "NV", "AZ", "CO", "NM", "UT"),] 

tic()
lcmap <- tlmr::access_data_lcmap_v13_stac_year_range(earliest_year = 2019, latest_year = 2020, aoi = sr)
toc()

tic()
lcms <- tlmr::access_data_lcms_conus_v20239_vsi_year_range(earliest_year = 2019, latest_year = 2020) %>%
  terra::crop(sr |> sf::st_transform(terra::crs(.)))
toc()



forest_lcmap_mask <- any(lcmap == 4) |> as.numeric() #4 = Tree Cover for primary land cover
# "Tree-covered land where the tree cover density is greater
# than 10%. Cleared or harvested trees (i.e., clearcuts) will
# be mapped according to current cover (e.g., Barren,
#                                       Grass/Shrub).

forest_lcms_mask <- any(lcms %in% 1:5) |> as.numeric()





