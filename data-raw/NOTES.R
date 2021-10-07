# manifest_r3 <-  ready4fun::write_package(pkg_setup_ls,
#                                          self_serve_1L_lgl = F,
#                                          self_serve_fn_ls = NULL)
# pkg_setup_ls$subsequent_ls$abbreviations_lup <- pkg_setup_ls$subsequent_ls$abbreviations_lup %>%
#   dplyr::filter(!short_name_chr %in% c("ready4classs"))
# pkg_setup_ls <- ready4fun::update_msng_abbrs(pkg_setup_ls,
#                                   are_words_chr = c("download", "recode"))
# pkg_setup_ls <- ready4fun::write_new_abbrs(pkg_setup_ls,
#                                            long_name_chr = c("ready4use R package"),
#                                            no_plural_chr = c("ready4use R package"))
# pkg_setup_ls$subsequent_ls$fn_types_lup <- pkg_setup_ls$subsequent_ls$fn_types_lup %>%
#   dplyr::filter(fn_type_nm_chr != "Bind Lups")
# ready4fun::write_new_fn_types(pkg_setup_ls, fn_type_desc_chr = "Rowbinds lookup tables of the same class, removing duplicates based on priority.")
