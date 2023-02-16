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

fnames <- list.files(here::here("data", "out", "masked-forest-disturbance-stack_tabular"), full.names = TRUE)
i = 1

# Took just under 3 hours without parallelization
multidisturbance_summary_western_conus_l <- 
  pbapply::pblapply(X = seq_along(fnames), FUN = function(i) {
    DT <- arrow::read_parquet(fnames[i])
    
    # https://stackoverflow.com/questions/39789618/subset-rows-in-data-table-if-all-specified-columns-match-a-criterion
    any_multi_summary <- DT[, .(x_5070, y_5070, row_num, col_num,
                                drought_count = Reduce(`+`, lapply(.SD, FUN = `%in%`, drought_vals)),
                                fire_count = Reduce(`+`, lapply(.SD, FUN = `%in%`, fire_vals)),
                                insect_disease_count = Reduce(`+`, lapply(.SD, FUN = `%in%`, insect_disease_vals)),
                                simul_lf_and_drought_count = Reduce(`+`, lapply(.SD, FUN = `%in%`, simul_lf_and_drought_vals))),
                            .SDcols = 5:ncol(DT)]
    
    any_multi_summary[, `:=`(multi_with_drought = simul_lf_and_drought_count > 0 | (drought_count + fire_count + insect_disease_count > 1),
                             multi_just_lf = fire_count + insect_disease_count > 1)]
    
    # (nrow(any_multi_summary) * 30 * 30 / 1e6) # square kilometers of forest represented
    # sum(any_multi_summary$multi_with_drought) * 30 * 30 / 1e6 # square kilometers of forest impacted by 2+ disturbances including drought
    # sum(any_multi_summary$multi_just_lf) * 30 * 30 / 1e6 # square kilometers of forest impacted by 2+ landfire disturbances
    
    # short interval multidisturbances
    interval <- 5
    year_cols <- 5:ncol(DT)
    year_col_ends <- year_cols + interval - 1
    year_col_starts <- year_cols[!year_col_ends > max(year_cols)]
    year_col_ends <- year_col_ends[!year_col_ends > max(year_cols)]
    
    short_intervals <- dplyr::tibble(interval = interval, year_col_starts, year_col_ends)
    # k = 1
    
    short_interval_summary <- 
      lapply(X = 1:nrow(short_intervals), FUN = function(k) {
        
        short_interval_multi_summary <- DT[, .(x_5070, y_5070, row_num, col_num,
                                               drought_count = Reduce(`+`, lapply(.SD, FUN = `%in%`, drought_vals)),
                                               fire_count = Reduce(`+`, lapply(.SD, FUN = `%in%`, fire_vals)),
                                               insect_disease_count = Reduce(`+`, lapply(.SD, FUN = `%in%`, insect_disease_vals)),
                                               simul_lf_and_drought_count = Reduce(`+`, lapply(.SD, FUN = `%in%`, simul_lf_and_drought_vals))),
                                           .SDcols = short_intervals$year_col_starts[k]:short_intervals$year_col_ends[k]]
        
        short_interval_multi_summary[, `:=`(x_5070 = x_5070, y_5070 = y_5070, row_num = row_num, col_num = col_num,
                                            year = paste(names(DT)[c(short_intervals$year_col_starts[k], short_intervals$year_col_ends[k])], collapse = "-"),
                                            short_interval_multi_with_drought = simul_lf_and_drought_count > 0 | (drought_count + fire_count + insect_disease_count > 1),
                                            short_interval_multi_just_lf = fire_count + insect_disease_count > 1)]
        
        out <- short_interval_multi_summary[, .SD, .SDcols = c("x_5070", "y_5070", "row_num", "col_num", "year",
                                                               "short_interval_multi_with_drought",
                                                               "short_interval_multi_just_lf")]
        
        return(out)
      }) %>% 
      data.table::rbindlist()
    
    short_interval_disturbance_counts <-
      short_interval_summary[, .(short_interval_multi_with_drought_count = sum(short_interval_multi_with_drought),
                                 short_interval_multi_just_lf_count = sum(short_interval_multi_just_lf)),
                             by = .(x_5070, y_5070, row_num, col_num)]
    
    # (nrow(short_interval_disturbance_counts) * 30 * 30 / 1e6) # square kilometers of forest represented
    # sum(short_interval_disturbance_counts$short_interval_multi_with_drought_count > 1) * 30 * 30 / 1e6 # square kilometers of forest impacted by 2+ disturbances including drought
    # sum(short_interval_disturbance_counts$short_interval_multi_just_lf_count > 1) * 30 * 30 / 1e6 # square kilometers of forest impacted by 2+ landfire disturbances
    
    out <- data.frame(fname = fnames[i], 
                      forest_area_km2 = (nrow(any_multi_summary) * 30 * 30 / 1e6),
                      any_multidisturbance_w_drought = sum(any_multi_summary$multi_with_drought) * 30 * 30 / 1e6,
                      any_multidisturbance_wo_drought = sum(any_multi_summary$multi_just_lf) * 30 * 30 / 1e6,
                      short_interval_multidisturbance_w_drought = sum(short_interval_disturbance_counts$short_interval_multi_with_drought_count > 1) * 30 * 30 / 1e6,
                      short_interval_multidisturbance_wo_drought = sum(short_interval_disturbance_counts$short_interval_multi_just_lf_count > 1) * 30 * 30 / 1e6)
    
    return(out)
  })

multidisturbance_summary_western_conus <-
  multidisturbance_summary_western_conus_l %>% 
  data.table::rbindlist()

dir.create(here::here("data", "analysis"), showWarnings = FALSE, recursive = TRUE)

data.table::fwrite(x = multidisturbance_summary_western_conus, file = here::here("data", "analysis", "multidisturbance-summary_western-conus.csv"))

tot_forest_area <- sum(multidisturbance_summary_western_conus$forest_area_km2)

sum(multidisturbance_summary_western_conus$any_multidisturbance_w_drought)
sum(multidisturbance_summary_western_conus$any_multidisturbance_w_drought) / tot_forest_area

sum(multidisturbance_summary_western_conus$any_multidisturbance_wo_drought)
sum(multidisturbance_summary_western_conus$any_multidisturbance_wo_drought) / tot_forest_area

sum(multidisturbance_summary_western_conus$short_interval_multidisturbance_w_drought)
sum(multidisturbance_summary_western_conus$short_interval_multidisturbance_w_drought) / tot_forest_area

sum(multidisturbance_summary_western_conus$short_interval_multidisturbance_wo_drought)
sum(multidisturbance_summary_western_conus$short_interval_multidisturbance_wo_drought) / tot_forest_area
