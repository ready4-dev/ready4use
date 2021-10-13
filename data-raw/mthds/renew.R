renew.ready4use_dictionary <- function(x,
                                    new_ready4_dict_r3){
  combined_ready4_dictionaries <- ready4fun::add_lups(x,
                                                      new_lup = new_ready4_dict_r3,
                                                      key_var_nm_1L_chr = "var_nm_chr")
  return(combined_ready4_dictionaries)
}
renew.ready4use_imports <- function(x,
                                    local_to_url_vec_chr,
                                    urls_vec_chr){
  purrr::reduce(1:length(local_to_url_vec_chr),
                .init = x,
                ~ update_tb_src_loc_to_url_sngl_tb(x = .x,
                                                   y = .y,
                                                   local_to_url_vec_chr = local_to_url_vec_chr,
                                                   urls_vec_chr = urls_vec_chr))
}
