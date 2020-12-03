# ## Script to create the object from which S4 classes will be made.
# ##
# ## 1. Prerequisites
# ##    None
# ##
# ## 2. Make the Make Class Table summarising the metadata about the S3 classes that we wish to create and export with this package.
# s4_classes_to_make_tb <- ready4class::ready4_constructor_tbl() %>%
#   tibble::add_case(name_stub_chr = "local",
#                    slots_ls = c("merge_with_chr_vec","raw_data_dir_chr","pckg_chr","overwrite_lgl", "save_lgl") %>% list(), # Cut: "lup_tbs_r4",
#                    pt_ls = c("character","character","character","logical", "logical") %>% list(), # Cut: "ready4_lookup",
#                    class_desc_chr= "Readyforwhatsnext S4 class defining data to be saved in local directory.",
#                    parent_class_chr = NA_character_) %>% # Cut: ,include_classes = list("ready4_lookup")
#   tibble::add_case(name_stub_chr = "local_raw",
#                    slots_ls = c("save_type") %>% list(),
#                    pt_ls = c("character") %>% list(),
#                    vals_ls = list(save_type ="raw"),
#                    allowed_vals_ls = list(save_type = "raw"),
#                    class_desc_chr= "Readyforwhatsnext S4 class defining data to be saved in local directory in a raw (unprocessed) format.",
#                    parent_class_chr = "ready4_local",
#                    inc_clss_ls = list("ready4_local")) %>%
#   tibble::add_case(name_stub_chr = "local_proc",
#                    slots_ls = c("save_type","proc_data_dir_chr","import_chr_vec","path_to_starter_sf_chr","import_this_ls") %>% list(),
#                    pt_ls = c("character","character","character","character","list") %>% list(),
#                    vals_ls = list(save_type = "proc"),
#                    allowed_vals_ls = list(save_type = "proc"),
#                    class_desc_chr= "Readyforwhatsnext S4 class defining data to be saved in local directory in a processed (R) format.",
#                    parent_class_chr = "ready4_local",
#                    inc_clss_ls = list("ready4_local")) %>%
#   tibble::add_case(name_stub_chr = "script_data",
#                    slots_ls = c(#"proc_data_dir_chr",
#                     # "raw_data_dir_chr",
#                      "crs_nbr_vec") %>% list(),
#                    pt_ls = c(#"character",
#                                  "numeric") %>% list(),
#                    class_desc_chr= "Readyforwhatsnext S4 class containint data to be passed to a function that constructs a spatial object from a lookup table.",
#                    parent_class_chr = "ready4_local_proc",
#                    inc_clss_ls = list("ready4_local_proc")) %>%
#   dplyr::mutate(make_s3_lgl = FALSE) #%>%  ready4class::remake_ls_cols()
