library(dplyr)
library(data.table)
library(arrow)

# Raster Attribute Table that describes the mapping of values in Landfire and Hotter Drought raster
# to a new value
rat <-
  expand.grid(lf = c(NA, 11:14, 21:24, 31:34), hd = c(0, 4:6)) %>% 
  dplyr::mutate(no_lf_bool = as.numeric(is.na(lf)),
                fire_bool = as.numeric(lf %in% 11:14),
                insect_disease_bool = as.numeric(lf %in% 21:24),
                other_lf_bool = as.numeric(lf %in% 31:34),
                no_drought_bool = as.numeric(hd == 0),
                drought4_bool = as.numeric(hd == 4),
                drought5_bool = as.numeric(hd == 5),
                drought6_bool = as.numeric(hd == 6)) %>% 
  dplyr::mutate(new_val = dplyr::case_when(no_drought_bool & no_lf_bool ~ 0, # no drought, no landfire disturbance --> 0
                                           no_drought_bool & fire_bool ~ 1, # no drought, fire --> 1
                                           no_drought_bool & insect_disease_bool ~ 2, # no drought, insects_disease --> 2
                                           no_drought_bool & other_lf_bool ~ 3, # no drought, other Landfire disturbance --> 3
                                           drought4_bool & no_lf_bool ~ 4, # 4 drought thresholds exceeded, no landfire disturbance --> 4
                                           drought4_bool & fire_bool ~ 5, # 4 drought thresholds exceeded, fire --> 5
                                           drought4_bool & insect_disease_bool ~ 6, # 4 drought thresholds exceeded, insects_disease --> 6
                                           drought4_bool & other_lf_bool ~ 7, # 4 drought thresholds exceeded, other Landfire disturbance --> 7
                                           drought5_bool & no_lf_bool ~ 8, # 5 drought thresholds exceeded, no landfire disturbance --> 8
                                           drought5_bool & fire_bool ~ 9, # 5 drought thresholds exceeded, fire --> 9
                                           drought5_bool & insect_disease_bool ~ 10, # 5 drought thresholds exceeded, insects_disease --> 10
                                           drought5_bool & other_lf_bool ~ 11, # 5 drought thresholds exceeded, other Landfire disturbance --> 11
                                           drought6_bool & no_lf_bool ~ 12, # 6 drought thresholds exceeded, no landfire disturbance --> 12
                                           drought6_bool & fire_bool ~ 13, # 6 drought thresholds exceeded, fire --> 13
                                           drought6_bool & insect_disease_bool ~ 14, # 6 drought thresholds exceeded, insects_disease --> 14
                                           drought6_bool & other_lf_bool ~ 15 # 6 drought thresholds exceeded, other Landfire disturbance --> 15
  ))

rat


simul_lf_and_drought_vals <- unique(rat$new_val[(rat$drought6_bool == 1 & rat$fire_bool == 1) | (rat$drought6_bool == 1 & rat$insect_disease_bool == 1)])
drought_vals <- unique(rat$new_val[rat$drought6_bool == 1])
fire_vals <- unique(rat$new_val[rat$fire_bool == 1])
insect_disease_vals <- unique(rat$new_val[rat$insect_disease_bool == 1])
no_lf_or_drought <- unique(rat$new_val[rat$drought6_bool == 0 & rat$fire_bool == 0 & rat$insect_disease_bool == 0])

determine_target_cols <- function(interval = 5, 
                                  year_col_start = 5, 
                                  ncol_DT = 26) {
  year_cols <- year_col_start:ncol_DT
  year_col_ends <- year_cols + interval - 1
  year_col_starts <- year_cols[!year_col_ends > max(year_cols)]
  year_col_ends <- year_col_ends[!year_col_ends > max(year_cols)]
  
  target_cols <- tibble::tibble(year_col_starts, 
                                year_col_ends)
  
  return(target_cols)
}

# Summary across whole dataset
recipe <- 
  tibble::tibble(path = list.files(here::here("data", "out", "masked-forest-disturbance-stack_tabular"), full.names = TRUE),
                 year_start = 1999,
                 year_end = 2020)

min_undisturbed_interval <- function(x) {
  
  if(any(x %in% simul_lf_and_drought_vals)) {
    out <- 0L
  } else {
    no_disturb_vec <- x %in% no_lf_or_drought
    
    if(sum(!no_disturb_vec) <= 1) {
      out <- NA_integer_
    } else {
      start_idx <- min(which(!(no_disturb_vec)))
      end_idx <- max(which(!(no_disturb_vec)))
      
      no_disturb_vec <- no_disturb_vec[start_idx:end_idx]
      
      rle_out <- rle(no_disturb_vec)
      rle_idx_no_disturbance <- which(rle_out$values)
      rle_idx_disturbance <- which(!rle_out$values)
      
      # If there were any back-to-back disturbances, then the interval between is 1 year
      if(any(rle_out$lengths[rle_idx_disturbance] > 1)) {
        out <- 1L
      } else {
        out <- 1L + as.integer(min(rle_out$lengths[rle_idx_no_disturbance]))
      }
    }
  }
  return(out)
}

x <- c(0, 1, 1, 0, 0, 0, 0)
x <- c(0, 1, 0, 1, 0, 0, 0, 0)
x <- c(0, 1, 1, 0, 0, 0, 1)
x <- c(1, 1, 0, 0, 0, 1)
min_undisturbed_interval(x)

summarize_min_undisturbed_interval <- function(path) {
  
  DT_long <- melt_multidisturbance(path, year_start = 1999, 
                                   year_end = 2020)
  
  min_undisturbed_interval_percell <- DT_long[, .(min_undisturbed_interval = min_undisturbed_interval(value)), 
                                              by = .(x_5070, y_5070, row_num, col_num)]
  
  out <- min_undisturbed_interval_percell[!is.na(min_undisturbed_interval)]
  out <- out[, .(cell_count = .N), by = .(min_undisturbed_interval)]
  out[, area_km2 := cell_count * 30 * 30 / 1e6]
  out[, path := path]

    return(out)
}

melt_multidisturbance <- function(path, 
                                  year_start = 1999, 
                                  year_end = 2020) {
  DT <- arrow::read_parquet(path)
  
  DT_long <- data.table::melt(data = DT, id.vars = c("x_5070", "y_5070", "row_num", "col_num"), 
                              variable.name = "year",
                              variable.factor = FALSE)
  DT_long <- DT_long[year >= year_start & year <= year_end, ]
  
  return(DT_long)
}

multidisturbance_per_cell <- function(path,
                                      year_start = 1999,
                                      year_end = 2020) {
  
  DT_long <- melt_multidisturbance(path, year_start, year_end)
  
  out <- 
    DT_long[, .(drought_count = sum(value %in% drought_vals),
                fire_count = sum(value %in% fire_vals),
                insect_disease_count = sum(value %in% insect_disease_vals),
                simul_lf_and_drought_count = sum(value %in% simul_lf_and_drought_vals)), 
            by = .(x_5070, y_5070, row_num, col_num)]
  
  out[, `:=`(simul_lf_and_drought = simul_lf_and_drought_count > 0,
             multi_with_drought = simul_lf_and_drought_count > 0 | 
               ((drought_count + fire_count + insect_disease_count) > 1),
             multi_just_lf = (fire_count + insect_disease_count) > 1)]
  
  return(out)
}

summarize_multidisturbance <- function(path, 
                                       year_start = 1999, 
                                       year_end = 2020) {
  
  multi_per_cell <- multidisturbance_per_cell(path, year_start, year_end)
  
  out <- data.frame(fname = path, 
                    year_start = year_start,
                    year_end = year_end,
                    interval = year_end - year_start + 1,
                    forest_area_km2 = (nrow(multi_per_cell) * 30 * 30 / 1e6),
                    any_multidisturbance_w_drought = sum(multi_per_cell$multi_with_drought) * 30 * 30 / 1e6,
                    any_multidisturbance_wo_drought = sum(multi_per_cell$multi_just_lf) * 30 * 30 / 1e6)
  
  return(out)
}

test <- summarize_min_undisturbed_interval(path = recipe$path[1])

(start_time <- Sys.time())
future::plan(strategy = "multisession", workers = 4)
short_interval_distribution <- 
  furrr::future_map(.x = recipe$path, .f = summarize_min_undisturbed_interval, .progress = TRUE)
(end_time <- Sys.time())
difftime(end_time, start_time, units = "hours")
# Took ~ 3 hours parallelized with 4 cores on a 64GB, 12-core machine

out <-
  short_interval_distribution %>% 
  data.table::rbindlist()

readr::write_csv(x = out, file = "data/analysis/short-interval-distribution.csv")

out_gg <-
  out %>% 
  dplyr::group_by(min_undisturbed_interval) %>% 
  dplyr::summarize(area_km2 = sum(area_km2)) %>% 
  dplyr::arrange(min_undisturbed_interval) %>% 
  dplyr::mutate(cumsum = cumsum(area_km2)) %>% 
  dplyr::mutate(cum_prop = cumsum / sum(out$area_km2))

out_gg %>% print(n = 22)
sum(out_gg$area_km2)

library(ggplot2)
ggplot(out_gg, aes(x = min_undisturbed_interval, y = area_km2)) +
  geom_col() +
  labs(x = "Minimum interval between disturbance\nfor multi-disturbed forest (years)",
       y = "Area (km^2)") +
  theme_bw()


idx <- 1:nrow(recipe)
idx <- idx[1:4]
(start_time <- Sys.time())
future::plan(strategy = "multisession", workers = 10)
multidisturbance_summary <- 
  furrr::future_pmap(.l = list(path = recipe$path[idx],
                               year_start = recipe$year_start[idx],
                               year_end = recipe$year_end[idx]),
                     .f = summarize_multidisturbance, .progress = TRUE)
(end_time <- Sys.time())
difftime(end_time, start_time, units = "mins")

multidisturbance_summary %>% 
  data.table::rbindlist()

# short_interval_multidisturbance_w_drought = sum(short_interval_disturbance_counts$short_interval_multi_with_drought_count > 1) * 30 * 30 / 1e6,
# short_interval_multidisturbance_wo_drought = sum(short_interval_disturbance_counts$short_interval_multi_just_lf_count > 1) * 30 * 30 / 1e6
# 
# multidisturbance_summary_western_conus <-
#   multidisturbance_summary_western_conus_l %>% 
#   data.table::rbindlist()
# 
# dir.create(here::here("data", "analysis"), showWarnings = FALSE, recursive = TRUE)
# 
# data.table::fwrite(x = multidisturbance_summary_western_conus, 
#                    file = here::here("data", "analysis", "multidisturbance-summary_western-conus.csv"))
# 
# multidisturbance_summary_western_conus <- data.table::fread(
#   here::here("data", "analysis", "multidisturbance-summary_western-conus.csv")
# )
# 
# tot_forest_area <- sum(multidisturbance_summary_western_conus$forest_area_km2)
# 
# sum(multidisturbance_summary_western_conus$any_multidisturbance_w_drought)
# sum(multidisturbance_summary_western_conus$any_multidisturbance_w_drought) / tot_forest_area
# 
# sum(multidisturbance_summary_western_conus$any_multidisturbance_wo_drought)
# sum(multidisturbance_summary_western_conus$any_multidisturbance_wo_drought) / tot_forest_area
# 
# sum(multidisturbance_summary_western_conus$short_interval_multidisturbance_w_drought)
# sum(multidisturbance_summary_western_conus$short_interval_multidisturbance_w_drought) / tot_forest_area
# 
# sum(multidisturbance_summary_western_conus$short_interval_multidisturbance_wo_drought)
# sum(multidisturbance_summary_western_conus$short_interval_multidisturbance_wo_drought) / tot_forest_area
