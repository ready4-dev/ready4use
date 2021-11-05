args_ls<-purrr::pmap(x %>% dplyr::filter(make_s3_lgl != T),
             ~ is.list(..12[[1]])
               # list(name_stub_1L_chr = ..2,
               #                            name_pfx_1L_chr = stringr::str_sub(name_pfx_1L_chr,
               #                                                               end = -2) %>%
               #                              Hmisc::capitalize(),
               #                            output_dir_1L_chr = output_dir_1L_chr,
               #                            class_desc_1L_chr = ..10,
               #                            parent_cls_nm_1L_chr = if(is.na(..11)){
               #                              NULL}else{
               #                                ..11},
               #                            slots_chr = if(is.list(..12[[1]])){
               #                              ..12[[1]] %>% purrr::flatten() %>% purrr::flatten_chr()}else{
               #                                ..12[[1]]
               #                              },
               #                            type_chr = if(is.list(..3[[1]])){
               #                              ..3[[1]] %>% purrr::flatten() %>% purrr::flatten_chr()}else{
               #                                ..3[[1]]
               #                              },
               #                            meaningful_nms_ls = ..13,
               #                            vals_ls = ..6[[1]],
               #                            allowed_vals_ls = ..7[[1]],
               #                            clss_to_inc_chr = ..14[[1]],
               #                            prototype_lup = prototype_lup,
               #                            nss_to_ignore_chr = nss_to_ignore_chr,
               #                            req_pkgs_chr = req_pkgs_chr,
               #                            class_in_cache_cdn_1L_chr = class_in_cache_cdn_1L_chr,
               #                            asserts_ls = ..15[[1]],
               #                            fn_types_lup = fn_types_lup,
               #                            object_type_lup = object_type_lup,
               #                            consent_1L_chr = consent_1L_chr)
             )
