#' update tibble source local to url single tibble
#' @description update_tb_src_loc_to_url_sngl_tb() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update tibble source local to url single tibble. Function argument x specifies the object to be updated. Argument y provides the object to be updated. The function returns Updated (a tibble).
#' @param x An object
#' @param y PARAM_DESCRIPTION
#' @param local_to_url_vec_chr Local to url vector (a character vector)
#' @param urls_vec_chr Urls vector (a character vector)
#' @return Updated (a tibble)
#' @rdname update_tb_src_loc_to_url_sngl_tb
#' @export 
#' @importFrom dplyr mutate
#' @importFrom purrr map2_chr map_chr
update_tb_src_loc_to_url_sngl_tb <- function (x, y, local_to_url_vec_chr, urls_vec_chr) 
{
    updated_tb <- x %>% dplyr::mutate(download_url_chr = purrr::map2_chr(local_file_src_chr, 
        download_url_chr, ~ifelse(.x %in% local_to_url_vec_chr, 
            urls_vec_chr[y], .y))) %>% dplyr::mutate(local_file_src_chr = purrr::map_chr(local_file_src_chr, 
        ~ifelse(.x %in% local_to_url_vec_chr, NA_character_, 
            .x)))
    return(updated_tb)
}
