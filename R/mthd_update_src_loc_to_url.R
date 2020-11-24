#' Update source local to url method applied toeadyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import..
#' @description update_src_loc_to_url.ready4_all_import_lup() is an Update Source Local to Url method that updates data from a local file reference to a URL This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param local_to_url_vec PARAM_DESCRIPTION
#' @param urls_vec PARAM_DESCRIPTION
#' @return NULL
#' @rdname update_src_loc_to_url-methods
#' @export 
#' @importFrom purrr reduce
update_src_loc_to_url.ready4_all_import_lup <- function (x, local_to_url_vec, urls_vec) 
{
    purrr::reduce(1:length(local_to_url_vec), .init = x, ~update_tb_src_loc_to_url_sgl_tb(x = .x, 
        y = .y, local_to_url_vec = local_to_url_vec, urls_vec = urls_vec))
}
#' @rdname update_src_loc_to_url-methods
#' @aliases update_src_loc_to_url,ready4_all_import_lup-method
methods::setMethod("update_src_loc_to_url", "ready4_all_import_lup", update_src_loc_to_url.ready4_all_import_lup)
