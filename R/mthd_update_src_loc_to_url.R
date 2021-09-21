#' Update source local to url.ready4 all import lookup table
#' @description update_src_loc_to_url.ready4_all_import_lup() is an Update Source Local to Url generic that updates data from a local file reference to a URL The function is called for its side effects and does not return a value.
#' @param x An instance of 
#' @param local_to_url_vec_chr Local to url vector (a character vector)
#' @param urls_vec_chr Urls vector (a character vector)
#' @return NULL
#' @rdname update_src_loc_to_url-methods
#' @export 
#' @importFrom purrr reduce
update_src_loc_to_url.ready4_all_import_lup <- function (x, local_to_url_vec_chr, urls_vec_chr) 
{
    purrr::reduce(1:length(local_to_url_vec_chr), .init = x, 
        ~update_tb_src_loc_to_url_sngl_tb(x = .x, y = .y, local_to_url_vec_chr = local_to_url_vec_chr, 
            urls_vec_chr = urls_vec_chr))
}
#' @rdname update_src_loc_to_url-methods
#' @aliases update_src_loc_to_url,ready4_all_import_lup-method
methods::setMethod("update_src_loc_to_url", "ready4_all_import_lup", update_src_loc_to_url.ready4_all_import_lup)
