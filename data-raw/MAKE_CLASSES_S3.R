# ## Script to create the object from which S3 classes will be made.
# ##
# ## 1. Prerequisites
# ##    None
# ##
# ## 2. Make the ready4_constructor_tbl object summarising the metadata about the S3 classes that we wish to create and export with this package.
# s3_classes_to_make_tb <- ready4class::ready4_constructor_tbl() %>%
#   dplyr::bind_rows(tibble::tribble(
#     ~ make_s3_lgl, ~ name_stub_chr, ~ pt_ls, ~ pt_chkr_pfx_ls, ~ pt_ns_ls, ~ vals_ls, ~ allowed_vals_ls, ~ min_max_vals_ls, ~ start_end_vals_ls, ~ class_desc_chr, ~ parent_class_chr, ~ slots_ls, ~ meaningful_nms_ls, ~inc_clss_ls,
#     TRUE, "dist", list("list"), list("is."),list("base"),list(distribution = "character(0)",
#                                                               dist_param_1 = "numeric(0)",
#                                                               dist_param_2 = "numeric(0)",
#                                                               dist_param_3 = "numeric(0)",
#                                                               dist_param_4 = "numeric(0)",
#                                                               transformation = "character(0)"), NULL, NULL, NULL, "ready4 S3 class for list object that summarises the parameters of each distribution", NA_character_, NULL, NULL, NULL,
#     TRUE, "dv_import_lup", list("tibble"), list("is_"), list("tibble"),list(file_type = "character(0)",
#                                                                             file_name = "character(0)",
#                                                                             data_repo = "character(0)",
#                                                                             data_repo_ui = "character(0)",
#                                                                             data_repo_db_ui = "character(0)",
#                                                                             data_repo_file_ext = "character(0)",
#                                                                             data_repo_save_type = "character(0)"), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.", NA_character_, NULL, NULL, NULL,
#     TRUE, "all_import_lup", list("tibble"), list("is_"), list("tibble"),list(local_file_src = "character(0)",
#                                                                          make_script_src = "character(0)",
#                                                                          download_url = "character(0)",
#                                                                          inc_file_main = "character(0)",
#                                                                          inc_files_to_rename = "list()",
#                                                                          new_names_for_inc_files = "list()"), NULL, NULL, NULL, "ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.", "ready4_dv_import_lup", NULL, NULL, NULL,
#     TRUE, "par_struc_mape", list("tibble"), list("is_"), list("tibble"),list(param_name = "character(0)",
#                                                                              sex_age_band = "character(0)",
#                                                                              mape_05_yr_mde = "numeric(0)",
#                                                                              mape_10_yr_mde = "numeric(0)",
#                                                                              mape_15_yr_mde = "numeric(0)",
#                                                                              mape_05_yr_min = "numeric(0)",
#                                                                              mape_10_yr_min = "numeric(0)",
#                                                                              mape_15_yr_min = "numeric(0)",
#                                                                              mape_05_yr_max = "numeric(0)",
#                                                                              mape_10_yr_max = "numeric(0)",
#                                                                              mape_15_yr_max = "numeric(0)",
#                                                                              mape_05_yr_shp = "numeric(0)",
#                                                                              mape_10_yr_shp = "numeric(0)",
#                                                                              mape_15_yr_shp = "numeric(0)"), NULL, NULL, NULL, "ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.", NA_character_, NULL, NULL, NULL))
#
#
#
