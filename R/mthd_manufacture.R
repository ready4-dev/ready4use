#' Manufacture method applied to ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import..
#' @description manufacture.ready4use_imports() is a Manufacture method that creates a novel R object. This method is implemented for the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param forced_choice_chr Forced choice (a character vector), Default: 'NA'
#' @param script_args_ls Script arguments (a list), Default: NULL
#' @return NULL
#' @rdname manufacture-methods
#' @export 
#' @importFrom rlang exec
#' @importFrom ready4fun manufacture
manufacture.ready4use_imports <- function (x, forced_choice_chr = NA_character_, script_args_ls = NULL) 
{
    assert_single_row_tb(x)
    import_type_ls <- procure(x, inc_script_lgl = !is.null(script_args_ls), 
        forced_choice_chr = forced_choice_chr)
    switch(names(import_type_ls), script_chr = rlang::exec(Ready4useArguments, 
        x, !!!script_args_ls), local_chr = get_valid_path_chr(import_type_ls[[1]]), 
        repo_chr = manufacture(x), source_url_chr = url(import_type_ls[[1]]))
}
#' @rdname manufacture-methods
#' @aliases manufacture,ready4use_imports-method
#' @importFrom ready4fun manufacture
methods::setMethod("manufacture", methods::className("ready4use_imports", package = "ready4use"), manufacture.ready4use_imports)
