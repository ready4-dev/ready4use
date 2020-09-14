#' Read import from comma separated variables file
#' @description read_import_from_csv() is a Read function that reads an R script into memory. Specifically, this function implements an algorithm to read import from comma separated variables file. Function argument file_ref_chr specifies the path to object. The function returns Import (a readyforwhatsnext S3).
#' @param file_ref_chr File reference (a character vector)
#' @param is_url PARAM_DESCRIPTION, Default: F
#' @return Import (a readyforwhatsnext S3)
#' @rdname read_import_from_csv
#' @export 
#' @importFrom data.table fread
read_import_from_csv <- function (file_ref_chr, is_url = F) 
{
    if (is_url) 
        read_in_file <- data.table::fread(file_ref_chr, stringsAsFactors = F)
    else read_in_file <- read.csv(file = file_ref_chr, stringsAsFactors = F)
    import_r3 <- make_r3_from_csv_tb(read_in_file, ready4_import_lup)
    return(import_r3)
}
