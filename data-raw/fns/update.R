update_tb_src_loc_to_url_sgl_tb <- function(x,
                                      y,
                                      local_to_url_vec,
                                      urls_vec){
  updated_tb <- x %>% dplyr::mutate(download_url = purrr::map2_chr(local_file_src,
                                                     download_url,
                                                     ~ ifelse(.x %in% local_to_url_vec,
                                                              urls_vec[y],
                                                              .y))) %>%
    dplyr::mutate(local_file_src = purrr::map_chr(local_file_src,
                                                  ~ ifelse(.x %in% local_to_url_vec,
                                                           NA_character_,
                                                           .x)))
  return(updated_tb)
}
