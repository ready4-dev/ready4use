# pkg_setup_ls <- x_ready4fun_manifest
library(ready4)
#library(ready4use)
library(ready4fun)
devtools::load_all()

X <- Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                    gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(
                 #classes_bup_lup =  Y@b_Ready4useIngest@objects_ls$bup_classes_lup
                 #classes_lup = Z@b_Ready4useIngest@objects_ls$framework_metadata_ls$classes_lup
                 #abbreviations_lup = abbreviations_lup,
                 #libraries_tb = libraries_tb
                 #exclude_chr = c("aus_09_synth","dce_sa_cards","fakefolk","rebuild")
                 #libraries_ls = libraries_ls
                 #methods_tb = methods_tb
                 #modules_tb = modules_tb
                 treat_as_words_chr = c(Y@b_Ready4useIngest@objects_ls$treat_as_words_chr, "ready4show", "datestamp") %>% sort()
               )),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y,
           type_1L_chr = "prefer_gh")


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
# y <- ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
#                                gh_tag_1L_chr = "Documentation_0.0")
# x <- y %>%
#   ingest(metadata_1L_lgl = F)
# prototype_lup <- x$prototype_lup
# prototype_lup <- prototype_lup %>% dplyr::mutate(default_val_chr = dplyr::case_when(pt_ns_chr == "ready4" ~ "",
#                                                                    T ~ default_val_chr)) %>%
#   dplyr::arrange(pt_ns_chr)
# y <- share(y,
#            obj_to_share_xx = prototype_lup,
#            fl_nm_1L_chr = "prototype_lup")
# prototype_lup <- get_rds_from_pkg_dmt(pkg_setup_ls,
#                                       fl_nm_1L_chr = "prototype_lup")
# prototype_lup<- prototype_lup %>%
#   dplyr::filter(pt_ns_chr != "ready4" )
# prototype_lup <- ready4::add_lups(prototype_lup,
#                                   new_lup = make_addl_cls_pts_tb(prototype_lup,
#                                                                  cls_nms_chr = c("Ready4Module",
#                                                                                  "Ready4Private",
#                                                                                  "Ready4Public"),
#                                                                  clss_ns_1L_chr = "ready4"),
#                                   key_var_nm_1L_chr = "type_chr") %>%
#   dplyr::arrange(pt_ns_chr, type_chr)
# prototype_lup <- prototype_lup %>% dplyr::mutate(default_val_chr = dplyr::case_when(pt_ns_chr == "ready4use" ~ "",
#                                                                    T ~ default_val_chr),
#                                                  val_chr = dplyr::case_when(pt_ns_chr == "ready4use" ~ val_chr %>%
#                                                                               stringr::str_replace_all("ready4use::",""),
#                                                                             T ~ val_chr))
# write_env_objs_to_dv(list(prototype_lup = prototype_lup),
#                      descriptions_chr = "Class prototype lookup table",
#                      ds_url_1L_chr = "",
#                      piggyback_to_1L_chr = "ready4-dev/ready4",
#                      publish_dv_1L_lgl = T)
# See NOTES in youthvars

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
