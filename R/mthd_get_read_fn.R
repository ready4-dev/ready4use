#' Get read function method applied toeadyforwhatsnext S3 class for tibble object lookup table of files to be imported from a dataverse..
#' @description get_read_fn.ready4_dv_import_lup() is a Get Read Function method that retrieves a read function. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of files to be imported from a dataverse..NA
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @return NULL
#' @rdname get_read_fn.ready4_dv_import_lup
#' @export 
#' @importFrom purrr map
#' @importFrom readxl read_excel
#' @keywords internal
get_read_fn.ready4_dv_import_lup <- function (x) 
{
    purrr::map(x$file_type, ~switch(.x, .csv = read.csv, .xls = readxl::read_excel, 
        .xlsx = readxl::read_excel, .rds = readRDS()), )
}
