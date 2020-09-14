#' Get import type list method applied toeadyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import..
#' @description get_import_type_ls.ready4_all_import_lup() is a Get Import Type List method that retrieves data about the type of import to be processed. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param inc_script_lgl Include script (a logical vector), Default: T
#' @param forced_choice_chr Forced choice (a character vector), Default: 'NA'
#' @return NULL
#' @rdname get_import_type_ls.ready4_all_import_lup
#' @export 
#' @importFrom purrr discard
get_import_type_ls.ready4_all_import_lup <- function (x, inc_script_lgl = T, forced_choice_chr = NA_character_) 
{
    assert_single_row_tb(x)
    options_ls <- list(script_chr = x$make_script_src, local_chr = x$local_file_src, 
        repo_chr = x$data_repo_db_ui, source_url_chr = x$download_url) %>% 
        purrr::discard(is.na)
    if ("script_chr" %in% names(options_ls) & !inc_script_lgl) 
        options_ls$script_chr <- NULL
    if (!is.na(forced_choice_chr)) {
        if (!forced_choice_chr %in% names(options_ls)) 
            stop("Forced choice option is not available from input lookup table")
        options_ls <- options_ls[names(options_ls) == forced_choice_chr]
    }
    options_ls[1]
}
