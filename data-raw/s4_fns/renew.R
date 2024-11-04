renew_Ready4useDyad <- function(x,
                                arrange_by_1L_chr = c("category", "name", "both"),
                                categories_chr = character(0),
                                drop_chr = character(0),
                                dictionary_lups_ls = list(),
                                dictionary_r3 = ready4use_dictionary(), # new_cases_r3 = ready4use_dictionary(),
                                dummys_ls = NULL,
                                exclude_chr = character(0),
                                factors_chr = character(0),
                                dyad_ls = NULL,
                                fn = NULL,
                                fn_args_ls = NULL,
                                lup_tb = NULL,
                                match_var_nm_1L_chr = character(0),
                                method_1L_chr = c("first", "sample"),
                                names_chr = character(0),
                                new_val_xx = NULL,
                                remove_old_lbls_1L_lgl = T,
                                tfmn_1L_chr = "capitalise",
                                type_1L_chr = c("label", "base", "case", "drop", "dummys", "join", "keep", "levels", "mutate", "new", "rbind", "unlabel", "update",
                                                "sequential", "batch", "self"),
                                uid_var_nm_1L_chr = character(0),
                                var_ctg_chr = "Uncategorised",
                                vars_chr = character(0),
                                what_1L_chr = c("all", "dataset", "dictionary"),
                                ...){
  type_1L_chr <- match.arg(type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  assertthat::assert_that((is.list(dictionary_lups_ls) & (dictionary_lups_ls %>% purrr::map_lgl(~ready4show::is_ready4show_correspondences(.x)) %>% all())),
                          msg = "dictionary_lups_ls must be comprised solely of elements that are ready4show_correspondences.")
  if(what_1L_chr %in% c("all", "dataset") & type_1L_chr %in% c("label", "base", "case",  "dummys",  "levels",  "unlabel")){
    if(type_1L_chr %in% c("label","case")){
      dictionary_tb <- x@dictionary_r3
      if(tfmn_1L_chr == "capitalise"){
        dictionary_tb$var_desc_chr <- dictionary_tb$var_desc_chr %>%
          Hmisc::capitalize()
      }
      if(tfmn_1L_chr == "title"){
        dictionary_tb$var_desc_chr <- dictionary_tb$var_desc_chr %>%
          stringr::str_to_title()
      }
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
      if(is.null(dummys_ls)){
        dummys_ls <- new_val_xx
      }
      dummys_dict_r3 <- manufacture(x, dummys_ls = dummys_ls, flatten_1L_lgl = F, type_1L_chr = ifelse(type_1L_chr=="levels", "all", type_1L_chr), what_1L_chr = "factors-d")
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
  }else{
    if(!is.null(dyad_ls)){
      dyad_ls <- list(x) %>% append(dyad_ls)
    }
  }
  # add_dictionary, add_with_join,
  if(what_1L_chr == "dictionary" & type_1L_chr == "new"){
    x <- add_dictionary(x,
                        new_cases_r3 = dictionary_r3,
                        var_ctg_chr = var_ctg_chr,
                        arrange_by_1L_chr = ifelse(arrange_by_1L_chr=="both", "category", arrange_by_1L_chr))
  }
  if(type_1L_chr %in% c("drop", "keep", "mutate", "sequential", "batch", "self") | (what_1L_chr == "dictionary" & type_1L_chr == "update")){ # "rbind"
      x <- update_dyad(x,
                       arrange_1L_chr = arrange_by_1L_chr,
                       categories_chr = categories_chr,
                       dictionary_lups_ls = dictionary_lups_ls,
                       dictionary_r3 = dictionary_r3,
                       fn = fn,
                       fn_args_ls = fn_args_ls,
                       exclude_chr = exclude_chr,
                       lup_prototype_tb = lup_tb,
                       match_var_nm_1L_chr = match_var_nm_1L_chr,
                       method_1L_chr = method_1L_chr,
                       names_chr =  names_chr,
                       type_1L_chr = type_1L_chr,#c("keep","drop", "mutate")
                       vars_chr = vars_chr,
                       what_1L_chr = what_1L_chr)
    }
  if(type_1L_chr == "join"){
    x <- purrr::reduce(dyad_ls,
                       ~ add_with_join(.x,.y))
  }
  if(type_1L_chr == "rbind"){
    if(is.null(fn)){
      tfmn_fn <- identity
    }else{
      tfmn_fn <- fn
    }
    x <- bind_dyads(dyad_ls,
                    drop_chr = drop_chr,
                    factors_chr = factors_chr,
                    tfmn_fn = tfmn_fn,
                    uid_var_nm_1L_chr = uid_var_nm_1L_chr)
  }
  return(x)
}
