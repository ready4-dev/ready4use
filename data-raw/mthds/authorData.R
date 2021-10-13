authorData.ready4use_manifest <- function(x){
  if(!is.null(x$clss_to_apply_ls)){
    ds_nms_chr <- x$pkg_ds_ls_ls %>% purrr::map_chr(~ .x$db_1L_chr)
    x$clss_to_apply_ls %>% purrr::walk2(names(x$clss_to_apply_ls),
                                        ~ {
                                          fun_fn <- eval(parse(text=.y))
                                          .x %>%
                                            purrr::walk(~{
                                              idx_1L_int <- which(ds_nms_chr==.x)
                                              x$pkg_ds_ls_ls[[idx_1L_int]]$db_df %>%
                                                fun_fn %>%
                                                ready4fun::write_and_doc_ds(db_1L_chr = x$pkg_ds_ls_ls[[idx_1L_int]]$db_1L_chr,
                                                                            title_1L_chr = x$pkg_ds_ls_ls[[idx_1L_int]]$title_1L_chr,
                                                                            desc_1L_chr = x$pkg_ds_ls_ls[[idx_1L_int]]$desc_1L_chr,
                                                                            url_1L_chr = x$pkg_ds_ls_ls[[idx_1L_int]]$url_1L_chr,
                                                                            abbreviations_lup = x$x_ready4fun_manifest$subsequent_ls$abbreviations_lup,
                                                                            object_type_lup = x$x_ready4fun_manifest$subsequent_ls$object_type_lup)
                                            })
                                        })
  }

}
