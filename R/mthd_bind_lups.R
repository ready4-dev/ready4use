#' Bind lups method applied to ready4 S3 class defining a data dictionary tibble..
#' @description bind_lups.ready4_dictionary() is a Bind Lups method that rowbinds lookup tables of the same class, removing duplicates based on priority. This method is implemented for the ready4 s3 class defining a data dictionary tibble.. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 s3 class defining a data dictionary tibble.
#' @param new_ready4_dict_r3 New ready4 dictionary (a ready4 S3)
#' @return NA ()
#' @rdname bind_lups-methods
#' @export 
#' @importFrom ready4fun add_lups
bind_lups.ready4_dictionary <- function (x, new_ready4_dict_r3) 
{
    combined_ready4_dictionaries <- ready4fun::add_lups(x, new_lup = new_ready4_dict_r3, 
        key_var_nm_1L_chr = "var_nm_chr")
    return(combined_ready4_dictionaries)
}
#' @rdname bind_lups-methods
#' @aliases bind_lups,ready4_dictionary-method
methods::setMethod("bind_lups", "ready4_dictionary", bind_lups.ready4_dictionary)
