#' Get read function - a method that
#' @description get_read_fn.ready4use_dataverses() is a Get Read Function method that retrieves a read function. This method is implemented for the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @return NULL
#' @rdname get_read_fn-methods
#' @export 
#' @importFrom purrr map
#' @importFrom readxl read_excel
get_read_fn.ready4use_dataverses <- function (x) 
{
    purrr::map(x$file_type_chr, ~switch(.x, .csv = read.csv, 
        .xls = readxl::read_excel, .xlsx = readxl::read_excel, 
        .rds = readRDS()), )
}
#' @rdname get_read_fn-methods
#' @aliases get_read_fn,ready4use_dataverses-method
methods::setMethod("get_read_fn", methods::className("ready4use_dataverses", package = "ready4use"), get_read_fn.ready4use_dataverses)
