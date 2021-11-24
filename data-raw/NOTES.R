# pkg_setup_ls <- x_ready4fun_manifest
# library(ready4fun)
prototype_lup <- ready4fun::get_rds_from_pkg_dmt(fl_nm_1L_chr = "prototype_lup",
                                                piggyback_to_1L_chr = "ready4-dev/ready4")
make_addl_cls_pts_tb <- function(prototype_lup,
                                 # pkg_setup_ls,
                                 cls_nms_chr,
                                 clss_ns_1L_chr){
#   if(is.null(pkg_setup_ls$subsequent_ls$prototype_lup)){
# prototype_lup <- ready4fun::get_rds_from_pkg_dmt(pkg_setup_ls,
#                                       fl_nm_1L_chr = "prototype_lup")
#   }else{
#     prototype_lup <- pkg_setup_ls$subsequent_ls$prototype_lup
#   }
  addl_cls_pts_tb <- setdiff(cls_nms_chr,
                             prototype_lup$type_chr) %>%
    purrr::reduce(.init = prototype_lup[0,],
                  ~ .x %>% #
                    tibble::add_case(type_chr = .y,
                                     fn_to_call_chr = .y,
                                     val_chr = paste0(clss_ns_1L_chr,
                                                      "::",
                                                      .y,
                                                      "()"),
                                     pt_ns_chr = clss_ns_1L_chr,
                                     old_class_lgl = startsWith(.y,paste0(clss_ns_1L_chr,"_")),
                                     default_val_chr = "")) %>%
    dplyr::arrange(pt_ns_chr, type_chr)
  return(addl_cls_pts_tb)
}

# prototype_lup <- get_rds_from_pkg_dmt(pkg_setup_ls,
#                                       fl_nm_1L_chr = "prototype_lup")
prototype_lup<- prototype_lup %>%
  dplyr::filter(pt_ns_chr != "ready4" )
prototype_lup <- ready4::add_lups(prototype_lup,
                                  new_lup = make_addl_cls_pts_tb(prototype_lup,
                                                                 cls_nms_chr = c("Ready4Module",
                                                                                 "Ready4Private",
                                                                                 "Ready4Public"),
                                                                 clss_ns_1L_chr = "ready4"),
                                  key_var_nm_1L_chr = "type_chr") %>%
  dplyr::arrange(pt_ns_chr, type_chr)
# prototype_lup <- prototype_lup %>% dplyr::mutate(default_val_chr = dplyr::case_when(pt_ns_chr == "ready4use" ~ "",
#                                                                    T ~ default_val_chr),
#                                                  val_chr = dplyr::case_when(pt_ns_chr == "ready4use" ~ val_chr %>%
#                                                                               stringr::str_replace_all("ready4use::",""),
#                                                                             T ~ val_chr))
ready4::write_env_objs_to_dv(list(prototype_lup = prototype_lup),
                     descriptions_chr = "Class prototype lookup table",
                     ds_url_1L_chr = character(0),
                     piggyback_to_1L_chr = "ready4-dev/ready4",
                     publish_dv_1L_lgl = F)
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
