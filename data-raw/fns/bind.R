bind_dyads <- function(dyad_ls,
                       drop_chr = character(0),
                       factors_chr = character(0),
                       tfmn_fn = identity,
                       uid_var_nm_1L_chr = character(0)){
  if(!identical(drop_chr, character(0))){
    dyad_ls <- dyad_ls %>% purrr::discard_at(drop_chr)
  }
  ds_tb <- dyad_ls %>% purrr::map_dfr(~.x@ds_tb)
  if(!identical(uid_var_nm_1L_chr, character(0))){
    ds_tb <- ds_tb %>% dplyr::arrange(tfmn_fn(!!rlang::sym(uid_var_nm_1L_chr)))
  }
  dictionary_r3 <- dyad_ls %>% purrr::map_dfr(~.x@dictionary_r3) %>% dplyr::filter(!duplicated(var_nm_chr)) %>%
    dplyr::filter(var_nm_chr %in% names(ds_tb))
  X_Ready4useDyad <- Ready4useDyad(ds_tb = ds_tb,
                                   dictionary_r3 = dictionary_r3)
  if(!identical(factors_chr, character(0))){
    X_Ready4useDyad <- purrr::reduce(factors_chr, .init = X_Ready4useDyad,
                                     ~{
                                       Z <- .x
                                       factor_var_1L_chr <- .y
                                       if(!is.factor(Z@ds_tb %>% dplyr::pull(!!rlang::sym(factor_var_1L_chr)))){
                                         Z@ds_tb <- Z@ds_tb %>% dplyr::mutate(!!rlang::sym(factor_var_1L_chr) := as.factor(!!rlang::sym(factor_var_1L_chr)))
                                         Z@dictionary_r3 <- Z@dictionary_r3 %>% dplyr::mutate(var_type_chr = dplyr::case_when(var_nm_chr == factor_var_1L_chr ~ "factor",
                                                                                                                              T ~ var_type_chr))
                                       }
                                       Z
                                     })
  }
  return(X_Ready4useDyad)
}
