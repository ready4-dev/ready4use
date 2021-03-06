get_read_fn.ready4_dv_import_lup <- function(x){
  purrr::map(x$file_type,
             ~ switch(.x,
                      ".csv" = read.csv, ## Need to add fread
                      ".xls" = readxl::read_excel,
                      ".xlsx" = readxl::read_excel,
                      ".rds" = readRDS()),
  )
}
