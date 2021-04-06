update_src_loc_to_url.ready4_all_import_lup <- function(x,
                                                        local_to_url_vec_chr,
                                                        urls_vec_chr){
  purrr::reduce(1:length(local_to_url_vec_chr),
                .init = x,
                ~ update_tb_src_loc_to_url_sgl_tb(x = .x,
                                            y = .y,
                                            local_to_url_vec_chr = local_to_url_vec_chr,
                                            urls_vec_chr = urls_vec_chr))
}
