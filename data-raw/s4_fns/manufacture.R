manufacture_Ready4useDyad <- function(x,
                                      correspondences_r3 = ready4show::ready4show_correspondences(),
                                      dummys_ls = list(),
                                      indcs_int = 1L,
                                      flatten_1L_lgl = F,
                                      match_1L_chr = character(0),
                                      restrict_to_chr = character(0),
                                      remove_ctg_1L_lgl = F,
                                      target_1L_chr = "var_ctg_chr",
                                      type_1L_chr = "all",
                                      what_1L_chr = "factors",
                                      ...){
  if(what_1L_chr %in% c("factors","factors-c","factors-d")){
    factor_vars_chr <- x@dictionary_r3 %>%
      renew.ready4use_dictionary(filter_cdn_1L_chr = "var_type_chr == 'factor'") %>%
      dplyr::pull(var_nm_chr) %>% as.character()
    if(!identical(restrict_to_chr, character(0))){
      factor_vars_chr <- intersect(factor_vars_chr, restrict_to_chr)
    }
    if(!identical(dummys_ls, list())){
      old_ls <- manufacture(x, flatten_1L_lgl = F, type_1L_chr = type_1L_chr, what_1L_chr = "factors", restrict_to_chr = names(dummys_ls))
      dummys_ls <- dummys_ls[names(old_ls)]
      dummys_ls <- purrr::map2(dummys_ls, names(dummys_ls), ~ paste0(.y,.x))
      object_xx <- manufacture(x, flatten_1L_lgl = F, type_1L_chr = type_1L_chr, what_1L_chr = what_1L_chr, restrict_to_chr = names(dummys_ls), new_nms_chr = dummys_ls %>% purrr::flatten_chr())
    }else{
      if(type_1L_chr == "vars" && what_1L_chr == "factors"){
        object_xx <- factor_vars_chr
      }else{
        object_xx <- purrr::map(factor_vars_chr,
                                ~ paste0(.x,eval(parse(text=paste0("levels(x@ds_tb$",.x,")"))))) %>%
          stats::setNames(factor_vars_chr)
        if(what_1L_chr == "factors-d"){
          lookup_ls <- object_xx
        }
        if(type_1L_chr == "base"){
          object_xx <- object_xx %>% purrr::map2(indcs_int, ~ .x[.y])
        }
        if(type_1L_chr == "dummys"){
          object_xx <- object_xx %>% purrr::map2(indcs_int, ~ .x[-.y])
        }
        if(remove_ctg_1L_lgl){
          object_xx <- object_xx %>% purrr::map2(names(object_xx), ~ stringr::str_remove(.x,.y))
        }
        if(!identical(correspondences_r3, ready4show::ready4show_correspondences())){
          object_xx <- ready4show::manufacture.ready4show_correspondences(correspondences_r3,
                                                                          data_ls = object_xx,
                                                                          flatten_1L_chr = F, # Correct
                                                                          type_1L_chr = "new",
                                                                          what_1L_chr = "names",
                                                                          ...)
        }
        if(flatten_1L_lgl | what_1L_chr %in% c("factors-c","factors-d")){
          object_xx <- object_xx %>% purrr::flatten_chr()
        }
        if(what_1L_chr %in% c("factors-c", "factors-d")){
          object_xx <- ready4show::ready4show_correspondences(ready4show::make_pt_ready4show_correspondences(old_nms_chr = object_xx, ...))
          if(what_1L_chr == "factors-d"){
            object_xx <- ready4use_dictionary(make_pt_ready4use_dictionary(var_nm_chr = object_xx %>% purrr::pmap_chr(~ifelse(is.na(as.character(..2)),
                                                                                                                              as.character(..1),as.character(..2))),
                                                                           var_ctg_chr = object_xx$old_nms_chr %>%  purrr::map_chr(~{
                                                                             level_1L_chr <- .x
                                                                             names(lookup_ls)[purrr::map_lgl(lookup_ls,~level_1L_chr %in% .x)]
                                                                           }),
                                                                           var_desc_chr = lookup_ls %>%
                                                                             purrr::map2(names(lookup_ls),
                                                                                         ~ stringr::str_remove(.x,.y)) %>%
                                                                             purrr::map2(indcs_int,
                                                                                         ~ {
                                                                                           if(!type_1L_chr %in% c("base","dummys")){
                                                                                             description_chr <- .x
                                                                                           }
                                                                                           if(type_1L_chr == "base"){
                                                                                             description_chr <- .x[.y]
                                                                                           }
                                                                                           if(type_1L_chr == "dummys"){
                                                                                             description_chr <- .x[-.y]
                                                                                           }
                                                                                           description_chr
                                                                                         }) %>% purrr::flatten_chr(),
                                                                           var_type_chr = object_xx$old_nms_chr %>%
                                                                             purrr::map(~{
                                                                               level_1L_chr <- .x
                                                                               purrr::map2(lookup_ls, indcs_int,
                                                                                           ~if(level_1L_chr %in% .x[.y]){
                                                                                             "base"
                                                                                           }else{
                                                                                             if(level_1L_chr %in% .x[-.y]){
                                                                                               "dummy"
                                                                                             }else{
                                                                                               NULL
                                                                                             }
                                                                                           }) %>% purrr::discard(is.null) %>%
                                                                                 purrr::flatten_chr()
                                                                             }) %>% purrr::flatten_chr()))
          }
          if(!identical(match_1L_chr, character(0))){
            object_xx <- ready4::get_from_lup_obj(object_xx,
                                                  match_value_xx = match_1L_chr,
                                                  match_var_nm_1L_chr = ifelse(what_1L_chr == "factors-d","var_nm_chr","old_nms_chr"),
                                                  target_var_nm_1L_chr = ifelse(what_1L_chr == "factors-d",target_1L_chr,"new_nms_chr"))
          }

        }
      }
    }
  }
  return(object_xx)
}
