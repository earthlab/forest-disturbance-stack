library(dplyr)
library(terra)
library(USAboundaries)
library(here)

dir.create("data/out/landfire-disturbance/western-conus", showWarnings = FALSE, recursive = TRUE)

relevant_files <- read.csv(here::here("data", "out", "landfire-disturbance", "file-directory_landfire-disturbance_conus.csv"))

# # Potential disturbance type groupings:
# unique(rat$dist_type)
new_dist_type <-
  tibble::tribble(
    ~dist_type,          ~new_dist_type,                  ~new_dist,
    "Wildfire",          "fire",                          1,
    "Wildland Fire Use", "fire",                          1,
    "Prescribed Fire",   "fire",                          1,
    "Wildland Fire",     "fire",                          1,
    "Fire",              "fire",                          1,
    "Insects",           "insect_disease",                2,
    "Disease",           "insect_disease",                2,
    "Insects/Disease",   "insect_disease",                2,
    "Biological",        "insect_disease",                2,
    "Clearcut",          "clearcut_harvest_othermech",    3,
    "Harvest",           "clearcut_harvest_othermech",    3,
    "Other Mechanical",  "clearcut_harvest_othermech",    3,
    "Thinning",          "fuel_trt",                      3,
    "Mastication",       "fuel_trt",                      3,
    "Weather",           "weather",                       3,
    "Development",       "development",                   3,
    "Chemical",          "chemical",                      3,
    "Herbicide" ,        "chemical",                      3,
    "Insecticide",       "insecticide",                   3,
    "Unknown",           "unknown",                       3)

# # Potential severity groupings:
# unique(rat$severity)
new_sev_type <-
  tibble::tribble(
    ~severity,         ~new_sev_type,           ~new_sev,
    "Unburned/Low",    "low",                   1,
    "Low",             "low",                   1,
    "No Severity",     "low",                   1,
    "Medium",          "medium",                2,
    "Low-Medium",      "medium",                2,
    "Medium-Low",      "medium",                2,
    "Medium-High",     "medium",                2,
    "High-Medium",     "medium",                2,
    "High",            "high",                  3,
    "Increased Green", "increased_green",       4)

new_dist_sev_table <- 
  tidyr::expand_grid(new_dist_type, new_sev_type) %>% 
  # This makes the 51, 52, and 53 categories lumped versions of "everything else at low severity, medium severity, and high severity"
  dplyr::mutate(new_val = dplyr::case_when((10 * new_dist + new_sev) %in% c(31, 41, 51, 61, 71, 81, 91) ~ 31,
                                           (10 * new_dist + new_sev) %in% c(32, 42, 52, 62, 72, 82, 92) ~ 32,
                                           (10 * new_dist + new_sev) %in% c(33, 43, 53, 63, 73, 83, 93) ~ 33,
                                           (10 * new_dist + new_sev) %in% c(34, 44, 54, 64, 74, 84, 94) ~ 34,
                                           TRUE ~ (10 * new_dist + new_sev))) %>% 
  dplyr::mutate(new_dist_type = dplyr::case_when((new_val %in% 31:34) ~ "other",
                                                 TRUE ~ new_dist_type)) %>% 
  dplyr::mutate(new_cat = paste(new_dist_type, new_sev_type, sep = "_"),
                new_sev_name = paste(new_sev_type, "sev", sep = "_"))

rat <-
  read.csv(here::here("data", "out", "landfire-disturbance", "raster-attribute-table_landfire-disturbance_conus.csv")) %>% 
  dplyr::left_join(new_dist_sev_table, by = c("dist_type", "severity"))

new_dist_sev_table_simple <-
  new_dist_sev_table %>% 
  dplyr::group_by(new_val, new_cat, new_dist_type, new_sev_type, new_sev_name) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::select(new_val, new_cat, new_dist_type, new_sev_type, new_sev_name) %>% 
  data.table::as.data.table()

western_conus <- 
  USAboundaries::us_states(resolution = "high", states = c("California", "Washington", "Oregon", "Nevada", "Idaho", "Montana", "Arizona", "New Mexico", "Colorado", "Utah", "Wyoming")) %>% 
  sf::st_transform(sf::st_crs(terra::rast(relevant_files$out_path[1]))) %>% 
  sf::st_geometry() %>% 
  terra::vect()

relevant_files <-
  relevant_files %>% 
  dplyr::mutate(out_path_western_conus = gsub(x = out_path, pattern = "conus", replacement = "western-conus"))

(start <- Sys.time())
for (i in 1:nrow(relevant_files)) {
  
  reclassification_mat <- 
    rat %>% 
    dplyr::filter(year == relevant_files$year[i]) %>% 
    dplyr::select(value, new_val) %>% 
    as.matrix() %>% 
    rbind(c(-9999, NA))
  
  r <- 
    terra::rast(relevant_files$out_path[i]) %>% 
    terra::crop(y = western_conus) %>% 
    terra::classify(rcl = reclassification_mat, 
                    filename = relevant_files$out_path_western_conus[i], 
                    overwrite = TRUE)
  
}
(end <- Sys.time())
(difftime(end, start, units = "mins"))

