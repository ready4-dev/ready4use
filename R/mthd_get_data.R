#' Get data.ready4 dataverse import lookup table
#' @description get_data.ready4_dv_import_lup() is a Get Data generic that retrieves data from R objects loaded in memory. The function is called for its side effects and does not return a value.
#' @param x An instance of 
#' @param save_dir_path_1L_chr Save directory path (a character vector of length one), Default: ''
#' @param unlink_1L_lgl Unlink (a logical vector of length one), Default: T
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @return NULL
#' @rdname get_data-methods
#' @export 
#' @importFrom purrr map2
get_data.ready4_dv_import_lup <- function (x, save_dir_path_1L_chr = "", unlink_1L_lgl = T, server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), 
    key_1L_chr = Sys.getenv("DATAVERSE_KEY")) 
{
    data_ls <- purrr::map2(1:nrow(x), get_read_fn(x), ~get_file_from_dv(ds_ui_1L_chr = x$data_repo_db_ui_chr[.x], 
        fl_nm_1L_chr = x$file_name_chr[.x], save_fmt_1L_chr = x$file_type_chr[.x], 
        repo_fl_fmt_1L_chr = x$data_repo_file_ext_chr[.x], server_1L_chr = ifelse(is.na(x$data_repo_ui_chr[.x]), 
            server_1L_chr, x$data_repo_ui_chr[.x]), key_1L_chr = key_1L_chr, 
        save_type_1L_chr = ifelse(is.na(x$data_repo_save_type_chr[.x]), 
            "original", x$data_repo_save_type_chr[.x]), save_dir_path_1L_chr = save_dir_path_1L_chr, 
        read_fn = .y, unlink_1L_lgl = unlink_1L_lgl))
    if (length(data_ls) > 1) 
        data_ls
    else data_ls[[1]]
}
#' @rdname get_data-methods
#' @aliases get_data,ready4_dv_import_lup-method
methods::setMethod("get_data", "ready4_dv_import_lup", get_data.ready4_dv_import_lup)
