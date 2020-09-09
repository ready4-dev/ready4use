#' Get data method applied toeadyforwhatsnext S3 class for tibble object lookup table of files to be imported from a dataverse..
#' @description get_data.ready4_dv_import_lup() is a Get Data method that retrieves data from R objects loaded in memory. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of files to be imported from a dataverse..NA
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param save_dir_path_chr Save directory path (a character vector), Default: ''
#' @param unlink_lgl Unlink (a logical vector), Default: T
#' @return NULL
#' @rdname get_data.ready4_dv_import_lup
#' @export 
#' @importFrom purrr map2
#' @keywords internal
get_data.ready4_dv_import_lup <- function (x, save_dir_path_chr = "", unlink_lgl = T) 
{
    data_ls <- purrr::map2(1:nrow(x), get_read_fn(x), ~get_file_from_dv(database_ui_chr = x$data_repo_db_ui[.x], 
        filename_chr = x$file_name[.x], save_format_chr = x$file_type[.x], 
        repo_file_format = x$data_repo_file_ext[.x], dataverse_chr = ifelse(is.na(x$data_repo_ui[.x]), 
            Sys.getenv("DATAVERSE_SERVER"), x$data_repo_ui[.x]), 
        save_type_chr = ifelse(is.na(x$data_repo_save_type[.x]), 
            "original", x$data_repo_save_type[.x]), save_dir_path_chr = save_dir_path_chr, 
        read_fn = .y, unlink_lgl = unlink_lgl))
    if (length(data_ls) > 1) 
        data_ls
    else data_ls[[1]]
}
