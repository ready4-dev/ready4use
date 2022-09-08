update_tb_src_loc_to_url_sngl_tb <- function(x,
                                      y,
                                      local_to_url_vec_chr,
                                      urls_vec_chr){
  updated_tb <- x %>% dplyr::mutate(download_url_chr = purrr::map2_chr(local_file_src_chr,
                                                     download_url_chr,
                                                     ~ ifelse(.x %in% local_to_url_vec_chr,
                                                              urls_vec_chr[y],
                                                              .y))) %>%
    dplyr::mutate(local_file_src_chr = purrr::map_chr(local_file_src_chr,
                                                  ~ ifelse(.x %in% local_to_url_vec_chr,
                                                           NA_character_,
                                                           .x)))
  return(updated_tb)
}
