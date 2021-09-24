#' Update source local to url method applied to ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import..
#' @description update_src_loc_to_url.ready4use_all_import_lup() is an Update Source Local to Url method that updates data from a local file reference to a URL This method is implemented for the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param local_to_url_vec_chr Local to url vector (a character vector)
#' @param urls_vec_chr Urls vector (a character vector)
#' @return NULL
#' @rdname update_src_loc_to_url-methods
#' @export 
#' @importFrom purrr reduce
update_src_loc_to_url.ready4use_all_import_lup <- function (x, local_to_url_vec_chr, urls_vec_chr) 
{
    purrr::reduce(1:length(local_to_url_vec_chr), .init = x, 
        ~update_tb_src_loc_to_url_sngl_tb(x = .x, y = .y, local_to_url_vec_chr = local_to_url_vec_chr, 
            urls_vec_chr = urls_vec_chr))
}
#' @rdname update_src_loc_to_url-methods
#' @aliases update_src_loc_to_url,ready4use_all_import_lup-method
methods::setMethod("update_src_loc_to_url", "ready4use_all_import_lup", update_src_loc_to_url.ready4use_all_import_lup)
