renew.ready4use_dictionary <- function(x,
                                       var_nm_chr = NA_character_,
                                       var_ctg_chr = NA_character_,
                                       var_desc_chr = NA_character_,
                                       var_type_chr = NA_character_,
                                       filter_cdn_1L_chr = NA_character_,
                                       new_ready4_dict_r3 = NULL,
                                       slice_idxs_int = NA_integer_){
  x <- ready4::update_tb_r3(x,
                            filter_cdn_1L_chr = filter_cdn_1L_chr,
                            slice_idxs_int = slice_idxs_int)
  x <- dplyr::bind_rows(x,
                        tibble::tibble(var_nm_chr = var_nm_chr,
                                       var_ctg_chr = var_ctg_chr,
                                       var_desc_chr = var_desc_chr,
                                       var_type_chr = var_type_chr))
  if(!is.null(new_ready4_dict_r3)){
    x <- ready4::add_lups(x,
                          new_lup = new_ready4_dict_r3,
                          key_var_nm_1L_chr = "var_nm_chr")
  }

  return(x)
}
renew.ready4use_imports <- function(x,
                                    local_file_src_chr = NA_character_,
                                    path_to_make_script_chr = NA_character_,
                                    download_url_chr = NA_character_,
                                    inc_file_main_chr = NA_character_,
                                    inc_fls_to_rename_ls =list(),
                                    new_nms_for_inc_fls_ls = list(),
                                    filter_cdn_1L_chr = NA_character_,
                                    local_to_url_vec_chr = NA_character_,
                                    slice_idxs_int = NA_integer_,
                                    urls_vec_chr = NA_character_){
  x <- ready4::update_tb_r3(x,
                            filter_cdn_1L_chr = filter_cdn_1L_chr,
                            slice_idxs_int = slice_idxs_int)
  x <- dplyr::bind_rows(x,
                        tibble::tibble(local_file_src_chr = local_file_src_chr,
                                       path_to_make_script_chr = path_to_make_script_chr,
                                       download_url_chr = download_url_chr,
                                       inc_file_main_chr = inc_file_main_chr,
                                       inc_fls_to_rename_ls =list(),
                                       new_nms_for_inc_fls_ls = list()))
  if(!is.na(local_to_url_vec_chr) & !is.na(urls_vec_chr))
    x <- purrr::reduce(1:length(local_to_url_vec_chr),
                .init = x,
                ~ update_tb_src_loc_to_url_sngl_tb(x = .x,
                                                   y = .y,
                                                   local_to_url_vec_chr = local_to_url_vec_chr,
                                                   urls_vec_chr = urls_vec_chr))
  return(x)
}
