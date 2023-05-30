renew_Ready4useDyad <- function(x,
                                new_val_xx = NULL,
                                remove_old_lbls_1L_lgl = T,
                                tfmn_1L_chr = "capitalise",
                                type_1L_chr = "label",
                                ...){
  if(type_1L_chr %in% c("label","case")){
    dictionary_tb <- x@dictionary_r3
    if(tfmn_1L_chr == "capitalise")
      dictionary_tb$var_desc_chr <- dictionary_tb$var_desc_chr %>%
        Hmisc::capitalize()
    if(tfmn_1L_chr == "title")
      dictionary_tb$var_desc_chr <- dictionary_tb$var_desc_chr %>%
        stringr::str_to_title()
  }
  if(type_1L_chr == "case"){
    x@dictionary_r3 <- dictionary_tb
  }
  if(type_1L_chr == "label"){
    tfd_ds_tb <- add_labels_from_dictionary(x@ds_tb,
                                            dictionary_tb = dictionary_tb %>% ready4::remove_lbls_from_df(),
                                            remove_old_lbls_1L_lgl = remove_old_lbls_1L_lgl)
    x@ds_tb <- tfd_ds_tb
  }
  if(type_1L_chr == "unlabel"){
    x@ds_tb <- remove_labels_from_ds(x@ds_tb)
  }
  if(type_1L_chr %in% c("base","dummys","levels")){
    dummys_dict_r3 <- manufacture(x, dummys_ls = new_val_xx, flatten_1L_lgl = F, type_1L_chr = ifelse(type_1L_chr=="levels","all",type_1L_chr), what_1L_chr = "factors-d")
    x@dictionary_r3 <- renew.ready4use_dictionary(x@dictionary_r3, new_cases_r3 = dummys_dict_r3)
    x@ds_tb <- purrr::reduce(dummys_dict_r3$var_ctg_chr %>% unique(),
                             .init = x@ds_tb,
                             ~{
                               var_nm_1L_chr <- .y
                               val_1_1L_chr <- if("base" %in% ready4::get_from_lup_obj(dummys_dict_r3, match_value_xx = .y, match_var_nm_1L_chr = "var_ctg_chr", target_var_nm_1L_chr = "var_type_chr")){character(0)}else{levels(.x %>% dplyr::pull(!!rlang::sym(.y)))[1]}
                               .x %>% dplyr::mutate(!!rlang::sym(.y) := factor(!!rlang::sym(.y), labels = c(val_1_1L_chr,
                                                                                                            ready4::get_from_lup_obj(dummys_dict_r3, match_value_xx = .y, match_var_nm_1L_chr = "var_ctg_chr", target_var_nm_1L_chr = "var_nm_chr") %>%
                                                                                                              purrr::map_chr(~stringi::stri_replace_first_fixed(.x,var_nm_1L_chr,""))
                               )))
                             } )

  }
  return(x)
}
