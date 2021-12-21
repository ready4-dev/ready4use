#' Manufacture a new object
#' @description manufacture.ready4use_dataverses() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse. The function returns Read (a list of functions).
#' @param x An instance of ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param type_1L_chr Type (a character vector of length one), Default: 'read_fn'
#' @return Read (a list of functions)
#' @rdname manufacture-methods
#' @export 
#' @importFrom purrr map
#' @importFrom readxl read_excel
#' @importFrom ready4 manufacture
manufacture.ready4use_dataverses <- function (x, type_1L_chr = "read_fn") 
{
    read_fn_ls <- NULL
    if (type_1L_chr == "read_fn") 
        read_fn_ls <- purrr::map(x$file_type_chr, ~switch(.x, 
            .csv = read.csv, .xls = readxl::read_excel, .xlsx = readxl::read_excel, 
            .RDS = readRDS()), )
    return(read_fn_ls)
}
#' @rdname manufacture-methods
#' @aliases manufacture,ready4use_dataverses-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("ready4use_dataverses", package = "ready4use"), manufacture.ready4use_dataverses)
#' Manufacture a new object
#' @description manufacture.ready4use_imports() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param forced_choice_chr Forced choice (a character vector), Default: 'NA'
#' @param script_args_ls Script arguments (a list), Default: NULL
#' @return NULL
#' @rdname manufacture-methods
#' @export 
#' @importFrom ready4 procure manufacture
#' @importFrom rlang exec
manufacture.ready4use_imports <- function (x, forced_choice_chr = NA_character_, script_args_ls = NULL) 
{
    assert_single_row_tb(x)
    import_type_ls <- ready4::procure(x, inc_script_lgl = !is.null(script_args_ls), 
        forced_choice_chr = forced_choice_chr)
    switch(names(import_type_ls), script_chr = rlang::exec(Ready4useArguments, 
        x, !!!script_args_ls), local_chr = get_valid_path_chr(import_type_ls[[1]]), 
        repo_chr = manufacture(x), source_url_chr = url(import_type_ls[[1]]))
}
#' @rdname manufacture-methods
#' @aliases manufacture,ready4use_imports-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("ready4use_imports", package = "ready4use"), manufacture.ready4use_imports)
