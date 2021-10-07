#' Procure method applied to ready4 S3 class for tibble object lookup table of files to be imported from a dataverse..
#' @description procure.ready4use_dataverses() is a Procure method that searches and retrieves requested data from a specified source. This method is implemented for the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param save_dir_path_1L_chr Save directory path (a character vector of length one), Default: ''
#' @param unlink_1L_lgl Unlink (a logical vector of length one), Default: T
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @return NULL
#' @rdname procure-methods
#' @export 
#' @importFrom purrr map2
#' @importFrom ready4fun procure
procure.ready4use_dataverses <- function (x, save_dir_path_1L_chr = "", unlink_1L_lgl = T, server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), 
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
#' @rdname procure-methods
#' @aliases procure,ready4use_dataverses-method
#' @importFrom ready4fun procure
methods::setMethod("procure", methods::className("ready4use_dataverses", package = "ready4use"), procure.ready4use_dataverses)
#' Procure method applied to ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import..
#' @description procure.ready4use_imports() is a Procure method that searches and retrieves requested data from a specified source. This method is implemented for the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param inc_script_lgl Include script (a logical vector), Default: T
#' @param forced_choice_chr Forced choice (a character vector), Default: 'NA'
#' @return NULL
#' @rdname procure-methods
#' @export 
#' @importFrom purrr discard
#' @importFrom ready4fun procure
procure.ready4use_imports <- function (x, inc_script_lgl = T, forced_choice_chr = NA_character_) 
{
    assert_single_row_tb(x)
    options_ls <- list(script_chr = x$path_to_make_script_chr, 
        local_chr = x$local_file_src_chr, repo_chr = x$data_repo_db_ui_chr, 
        source_url_chr = x$download_url_chr) %>% purrr::discard(is.na)
    if ("script_chr" %in% names(options_ls) & !inc_script_lgl) 
        options_ls$script_chr <- NULL
    if (!is.na(forced_choice_chr)) {
        if (!forced_choice_chr %in% names(options_ls)) 
            stop("Forced choice option is not available from input lookup table")
        options_ls <- options_ls[names(options_ls) == forced_choice_chr]
    }
    options_ls[1]
}
#' @rdname procure-methods
#' @aliases procure,ready4use_imports-method
#' @importFrom ready4fun procure
methods::setMethod("procure", methods::className("ready4use_imports", package = "ready4use"), procure.ready4use_imports)
