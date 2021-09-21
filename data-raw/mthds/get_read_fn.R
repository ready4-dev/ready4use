get_read_fn.ready4use_dv_import_lup <- function(x){
  purrr::map(x$file_type_chr,
             ~ switch(.x,
                      ".csv" = read.csv, ## Need to add fread
                      ".xls" = readxl::read_excel,
                      ".xlsx" = readxl::read_excel,
                      ".rds" = readRDS()),
  )
}
