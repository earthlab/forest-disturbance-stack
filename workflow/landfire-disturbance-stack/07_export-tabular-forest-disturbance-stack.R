library(terra)
library(dplyr)
library(arrow)
library(data.table)
library(pbapply)

dir.create(here::here("data", "out", "forest-disturbance-stack_tabular"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("data", "out", "masked-forest-disturbance-stack_tabular"), showWarnings = FALSE, recursive = TRUE)

fnames <- sort(list.files(here::here("data", "out", "forest-disturbance-stack", "western-conus"), full.names = TRUE))
lyr_names <- stringr::str_sub(fnames, start = -8, end = -5)

forest_disturbance_stack <- terra::rast(fnames) %>% setNames(lyr_names)

if(!file.exists(here::here("data", "ard", "forest-disturbance-stack_western-conus.tif"))) {
  terra::writeRaster(x = forest_disturbance_stack,
                     filename = here::here("data", "ard", "forest-disturbance-stack_western-conus.tif"),
                     datatype = "INT1U",
                     overwrite = TRUE)
}

masked_forest_disturbance_stack <- terra::rast(here::here("data", "ard", "masked-forest-disturbance-stack_western-conus.tif"))

forest_mask <- terra::rast(here::here("data", "ard", "landfire-bps-derived-forest-mask.tif"))

# Full stack written to disk
# https://stackoverflow.com/questions/73865059/geotiff-raster-data-to-delta-lake-parquet-format
fds_schema <- arrow::schema(arrow::field(name = 'x_5070', type = arrow::int32()),
                            arrow::field(name = 'y_5070', type = arrow::int32()),
                            arrow::field(name = 'row_num', type = arrow::int32()),
                            arrow::field(name = 'col_num', type = arrow::int32()),
                            arrow::field(name = '1999', type = arrow::int8()),
                            arrow::field(name = '2000', type = arrow::int8()),
                            arrow::field(name = '2001', type = arrow::int8()),
                            arrow::field(name = '2002', type = arrow::int8()),
                            arrow::field(name = '2003', type = arrow::int8()),
                            arrow::field(name = '2004', type = arrow::int8()),
                            arrow::field(name = '2005', type = arrow::int8()),
                            arrow::field(name = '2006', type = arrow::int8()),
                            arrow::field(name = '2007', type = arrow::int8()),
                            arrow::field(name = '2008', type = arrow::int8()),
                            arrow::field(name = '2009', type = arrow::int8()),
                            arrow::field(name = '2010', type = arrow::int8()),
                            arrow::field(name = '2011', type = arrow::int8()),
                            arrow::field(name = '2012', type = arrow::int8()),
                            arrow::field(name = '2013', type = arrow::int8()),
                            arrow::field(name = '2014', type = arrow::int8()),
                            arrow::field(name = '2015', type = arrow::int8()),
                            arrow::field(name = '2016', type = arrow::int8()),
                            arrow::field(name = '2017', type = arrow::int8()),
                            arrow::field(name = '2018', type = arrow::int8()),
                            arrow::field(name = '2019', type = arrow::int8()),
                            arrow::field(name = '2020', type = arrow::int8()))

# Write sequence as tabular data to file (Arrow Parquet)
# Must do it in chunks because data are so large.
writeParquet <- function(r, rows_per_file = 500, out_dir, na.rm = FALSE) {
  n_files <- nrow(r) %/% rows_per_file
  rows_last_file <- nrow(r) %% rows_per_file
  
  starts <- seq(from = 1, 
                to = nrow(r), 
                by = rows_per_file)
  
  if (rows_last_file == 0) {
    ends <- c(seq(from = rows_per_file, 
                  to = nrow(r), 
                  by = rows_per_file))
  } else if(rows_last_file > 0) {
    ends <- c(seq(from = rows_per_file, 
                  to = nrow(r), 
                  by = rows_per_file), 
              nrow(r))
  }
  
  row_idxs_all <- data.table(starts, ends)
  row_idxs_all[, nrows := (ends - starts) + 1]
  row_idxs_all[, filename := here::here(out_dir,
                                        paste0(sprintf(fmt = paste0("%0", nchar(.N), "d"), .I), 
                                               "_",
                                               sprintf(fmt = paste0("%0", max(nchar(ends)), "d"), starts),
                                               "_",
                                               sprintf(fmt = paste0("%0", max(nchar(ends)), "d"), ends), 
                                               ".parquet"))]
  
  row_idxs <-
    row_idxs_all %>% 
    dplyr::filter(!file.exists(filename))
  
  
  
  pblapply(X = seq_len(nrow(row_idxs)), 
           FUN = function(i) {
             
             tmp <-
               values(r, 
                      row = row_idxs$starts[i], 
                      nrows = row_idxs$nrows[i], 
                      dataframe = FALSE, 
                      mat = TRUE) %>% 
               as.data.table()
             
             for(j in colnames(tmp)) {
               data.table::set(tmp, j = j, value = as.integer(tmp[[j]]))
             }
             
             tmp[, `:=`(row_num = rep(row_idxs$starts[i]:row_idxs$ends[i],
                                      each = ncol(r)),
                        col_num = rep(1:ncol(r), 
                                      times = row_idxs$nrows[i]))]
             
             tmp[, `:=`(x_5070 = as.integer(terra::xFromCol(object = r,
                                                            col = col_num)),
                        y_5070 = as.integer(terra::yFromRow(object = r,
                                                            row = row_num)))]
             
             data.table::setcolorder(tmp, neworder = c("x_5070", "y_5070", "row_num", "col_num"))
             
             if(na.rm) {
               forest_mask_vec <-
                 values(forest_mask, 
                        row = row_idxs$starts[i], 
                        nrows = row_idxs$nrows[i], 
                        dataframe = FALSE, 
                        mat = FALSE)
               
               na_idx <- which(!is.na(forest_mask_vec))
               
               tmp <- tmp[na_idx, ]
             }
             
             tmp <- arrow::as_arrow_table(x = tmp, schema = fds_schema)
             arrow::write_parquet(x = tmp, sink = row_idxs$file[i])
             
             return(invisible())
             
           })
}

writeParquet(r = forest_disturbance_stack, 
             rows_per_file = 500,
             out_dir = here::here("data", "out", "forest-disturbance-stack_tabular"), 
             na.rm = FALSE)

writeParquet(r = masked_forest_disturbance_stack, 
             rows_per_file = 500, 
             out_dir = here::here("data", "out", "masked-forest-disturbance-stack_tabular"), 
             na.rm = TRUE)

out <- arrow::open_dataset(sources = here::here("data", "out", "forest-disturbance-stack_tabular"), schema = fds_schema)

computed_result <- 
  out %>% 
  dplyr::compute() %>% 
  as.data.table()

