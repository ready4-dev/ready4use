#' renew - a method that renews an instance of a class by updating it with new data
#' @description renew.ready4use_dictionary() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 s3 class defining a data dictionary tibble. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 s3 class defining a data dictionary tibble.
#' @param new_ready4_dict_r3 New ready4 dictionary (a ready4 S3)
#' @return combined_ready4_dictionaries (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom ready4 add_lups renew
renew.ready4use_dictionary <- function (x, new_ready4_dict_r3) 
{
    combined_ready4_dictionaries <- ready4::add_lups(x, new_lup = new_ready4_dict_r3, 
        key_var_nm_1L_chr = "var_nm_chr")
    return(combined_ready4_dictionaries)
}
#' @rdname renew-methods
#' @aliases renew,ready4use_dictionary-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4use_dictionary", package = "ready4use"), renew.ready4use_dictionary)
#' renew - a method that renews an instance of a class by updating it with new data
#' @description renew.ready4use_imports() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param local_to_url_vec_chr Local to url vector (a character vector)
#' @param urls_vec_chr Urls vector (a character vector)
#' @return NULL
#' @rdname renew-methods
#' @export 
#' @importFrom purrr reduce
#' @importFrom ready4 renew
renew.ready4use_imports <- function (x, local_to_url_vec_chr, urls_vec_chr) 
{
    purrr::reduce(1:length(local_to_url_vec_chr), .init = x, 
        ~update_tb_src_loc_to_url_sngl_tb(x = .x, y = .y, local_to_url_vec_chr = local_to_url_vec_chr, 
            urls_vec_chr = urls_vec_chr))
}
#' @rdname renew-methods
#' @aliases renew,ready4use_imports-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4use_imports", package = "ready4use"), renew.ready4use_imports)
