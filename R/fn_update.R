#' Update tibble source local to url sgl
#' @description update_tb_src_loc_to_url_sgl_tb() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update tibble source local to url sgl tibble. Function argument x specifies the object to be updated. Argument y provides the object to be updated. The function returns Updated (a tibble).
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param local_to_url_vec PARAM_DESCRIPTION
#' @param urls_vec PARAM_DESCRIPTION
#' @return Updated (a tibble)
#' @rdname update_tb_src_loc_to_url_sgl_tb
#' @export 
#' @importFrom dplyr mutate
#' @importFrom purrr map2_chr map_chr
#' @keywords internal
update_tb_src_loc_to_url_sgl_tb <- function (x, y, local_to_url_vec, urls_vec) 
{
    updated_tb <- x %>% dplyr::mutate(download_url = purrr::map2_chr(local_file_src, 
        download_url, ~ifelse(.x %in% local_to_url_vec, urls_vec[y], 
            .y))) %>% dplyr::mutate(local_file_src = purrr::map_chr(local_file_src, 
        ~ifelse(.x %in% local_to_url_vec, NA_character_, .x)))
    return(updated_tb)
}
