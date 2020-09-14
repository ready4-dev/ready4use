#' Make import output object of multiple potential types method applied toeadyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import..
#' @description make_import_xx.ready4_all_import_lup() is a Make Import Output Object of Multiple Potential Types method that makes an output object of multiple potential classes. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param forced_choice_chr Forced choice (a character vector), Default: 'NA'
#' @param script_args_ls Script arguments (a list), Default: NULL
#' @return NULL
#' @rdname make_import_xx.ready4_all_import_lup
#' @export 
#' @importFrom rlang exec
make_import_xx.ready4_all_import_lup <- function (x, forced_choice_chr = NA_character_, script_args_ls = NULL) 
{
    assert_single_row_tb(x)
    import_type_ls <- get_import_type_ls(x, inc_script_lgl = !is.null(script_args_ls), 
        forced_choice_chr = forced_choice_chr)
    switch(names(import_type_ls), script_chr = rlang::exec(ready4_script_data, 
        x, !!!script_args_ls), local_chr = get_valid_path_chr(import_type_ls[[1]]), 
        repo_chr = make_dv_import_lup(x), source_url_chr = url(import_type_ls[[1]]))
}
