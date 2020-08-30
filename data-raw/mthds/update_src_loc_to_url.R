update_src_loc_to_url.ready4_all_import_lup <- function(x,
                                                        local_to_url_vec,
                                                        urls_vec){
  purrr::reduce(1:length(local_to_url_vec),
                .init = x,
                ~ update_tb_src_loc_to_url_sgl_tb(x = .x,
                                            y = .y,
                                            local_to_url_vec = local_to_url_vec,
                                            urls_vec = urls_vec))
}
