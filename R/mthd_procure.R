#' Procure data from a model module
#' @description procure.ready4use_dataverses() is a procure method that procures data by executing a search and retrieval algorithm using data contained in an instance of a class. This method is implemented for the ready4 submodule class for tibble object lookup table of files to be imported from a dataverse. The function returns Data (an output object of multiple potential types).
#' @param x An instance of `ready4use_dataverses`, a ready4 submodule class for tibble object lookup table of files to be imported from a dataverse.
#' @param save_dir_path_1L_chr Save directory path (a character vector of length one), Default: ''
#' @param unlink_1L_lgl Unlink (a logical vector of length one), Default: T
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @return Data (an output object of multiple potential types)
#' @rdname procure-methods
#' @export 
#' @importFrom purrr map2
#' @importFrom ready4 procure
procure.ready4use_dataverses <- function (x, save_dir_path_1L_chr = "", unlink_1L_lgl = T, server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), 
    key_1L_chr = Sys.getenv("DATAVERSE_KEY")) 
{
    data_ls <- purrr::map2(1:nrow(x), manufacture(x), ~get_file_from_dv(ds_ui_1L_chr = x$data_repo_db_ui_chr[.x], 
        fl_nm_1L_chr = x$file_name_chr[.x], save_fmt_1L_chr = x$file_type_chr[.x], 
        repo_fl_fmt_1L_chr = x$data_repo_file_ext_chr[.x], server_1L_chr = ifelse(is.na(x$data_repo_ui_chr[.x]), 
            server_1L_chr, x$data_repo_ui_chr[.x]), key_1L_chr = key_1L_chr, 
        save_type_1L_chr = ifelse(is.na(x$data_repo_save_type_chr[.x]), 
            "original", x$data_repo_save_type_chr[.x]), save_dir_path_1L_chr = save_dir_path_1L_chr, 
        read_fn = .y, unlink_1L_lgl = unlink_1L_lgl))
    if (length(data_ls) > 1) 
        data_xx <- data_ls
    else data_xx <- data_ls[[1]]
    return(data_xx)
}
#' @rdname procure-methods
#' @aliases procure,ready4use_dataverses-method
#' @importFrom ready4 procure
methods::setMethod("procure", methods::className("ready4use_dataverses", package = "ready4use"), procure.ready4use_dataverses)
#' Procure data from a model module
#' @description procure.ready4use_imports() is a procure method that procures data by executing a search and retrieval algorithm using data contained in an instance of a class. This method is implemented for the ready4 submodule class for tibble object lookup table of sources of raw (un-processed) data to import. The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4use_imports`, a ready4 submodule class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param inc_script_1L_lgl Include script (a logical vector of length one), Default: T
#' @param forced_choice_chr Forced choice (a character vector), Default: 'NA'
#' @return No return value, called for side effects.
#' @rdname procure-methods
#' @export 
#' @importFrom purrr discard
#' @importFrom ready4 procure
procure.ready4use_imports <- function (x, inc_script_1L_lgl = T, forced_choice_chr = NA_character_) 
{
    assert_single_row_tb(x)
    options_ls <- list(script_chr = x$path_to_make_script_chr, 
        local_chr = x$local_file_src_chr, repo_chr = x$data_repo_db_ui_chr, 
        source_url_chr = x$download_url_chr) %>% purrr::discard(is.na)
    if ("script_chr" %in% names(options_ls) & !inc_script_1L_lgl) 
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
#' @importFrom ready4 procure
methods::setMethod("procure", methods::className("ready4use_imports", package = "ready4use"), procure.ready4use_imports)
#' 
#' Procure data from a model module
#' @name procure-Ready4useIngest
#' @description procure method applied to Ready4useIngest
#' @param x An object of class Ready4useIngest
#' @param fl_nm_1L_chr File name (a character vector of length one), Default: 'NA'
#' @return Object (an output object of multiple potential types)
#' @rdname procure-methods
#' @aliases procure,Ready4useIngest-method
#' @export 
#' @importFrom purrr pluck
#' @importFrom ready4 procure
methods::setMethod("procure", "Ready4useIngest", function (x, fl_nm_1L_chr = NA_character_) 
{
    if (!is.na(fl_nm_1L_chr[1])) {
        object_xx <- x@objects_ls %>% purrr::pluck(fl_nm_1L_chr)
    }
    else {
        object_xx <- x@objects_ls
    }
    return(object_xx)
})
