#' Renew.ready4 dictionary
#' @description renew.ready4_dictionary() is a Renew function that updates an instance of a class with new values. Specifically, this function implements an algorithm to renew.ready4 dictionary. The function is called for its side effects and does not return a value.
#' @param x An instance of 
#' @param new_ready4_dict_r3 New ready4 dictionary (a ready4 S3)
#' @return NA ()
#' @rdname renew-methods
#' @export 
#' @importFrom ready4fun add_lups renew
renew.ready4_dictionary <- function (x, new_ready4_dict_r3) 
{
    combined_ready4_dictionaries <- ready4fun::add_lups(x, new_lup = new_ready4_dict_r3, 
        key_var_nm_1L_chr = "var_nm_chr")
    return(combined_ready4_dictionaries)
}
#' @rdname renew-methods
#' @aliases renew,ready4_dictionary-method
#' @importFrom ready4fun renew
methods::setMethod("renew", methods::className("ready4_dictionary", package = "ready4use"), renew.ready4_dictionary)
#' Renew method applied to ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import..
#' @description renew.ready4use_imports() is a Renew method that updates an instance of a class with new values. This method is implemented for the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param local_to_url_vec_chr Local to url vector (a character vector)
#' @param urls_vec_chr Urls vector (a character vector)
#' @return NULL
#' @rdname renew-methods
#' @export 
#' @importFrom purrr reduce
#' @importFrom ready4fun renew
renew.ready4use_imports <- function (x, local_to_url_vec_chr, urls_vec_chr) 
{
    purrr::reduce(1:length(local_to_url_vec_chr), .init = x, 
        ~update_tb_src_loc_to_url_sngl_tb(x = .x, y = .y, local_to_url_vec_chr = local_to_url_vec_chr, 
            urls_vec_chr = urls_vec_chr))
}
#' @rdname renew-methods
#' @aliases renew,ready4use_imports-method
#' @importFrom ready4fun renew
methods::setMethod("renew", methods::className("ready4use_imports", package = "ready4use"), renew.ready4use_imports)