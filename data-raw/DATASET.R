## Script to make classes and save updated prototype table.
## This script creates the data files embedded with this package.
# 1. Load magrittr package to that the pipe operator ("%>%") can be used in this script.
library(magrittr)
#
# if(!dir.exists("man/figures"))
#   dir.create("man/figures")
# 2. Create "fns", "gnrcs" and "mthds" sub-directories.
ready4fun::write_fn_type_dirs()
#
# 3. MANUAL STEP. Write all your functions to R files in the new "fns" directory.
#
# 4. Set-up package structure
ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Standardised Developer Tools For Retrieving and Managing Data In Projects Developed With The Ready4 Suite" %>% tools::toTitleCase(),
                            pkg_desc_1L_chr = "ready4use provides a set of classes and methods for general data management tasks throughout the ready4 suite of tools for mental health data synthesis and modelling projects.
  This development version of the ready4use package has been made available as part of the process of testing and documenting the package. The tools contained in this development release automate a number of tasks which MODIFY THE DIRECTORY STRUCTURE OF YOUR LOCAL MACHINE.
  Therefore you should only trial this software if you feel confident that you understand what it does and have created a sandpit area in which you can safely undertake testing. If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                            authors_prsns = c(utils::person(
                              given = "Matthew",family = "Hamilton", email =
                                "matthew.hamilton@orygen.org.au",role = c("aut",
                                                                          "cre"),comment = c(ORCID = "0000-0001-7407-9194")
                            ),
                            utils::person("Glen", "Wiesner", email = "Glen.Wiesner@vu.edu.au",
                                          role = c("aut"), comment = c(ORCID = "0000-0002-0071-130X")),
                            #person("Alexandra", "Parker", email =  "Alex.Parker@vu.edu.au", role = c("rev"), comment = c(ORCID ="0000-0002-2398-6306")),
                            #person("Cathrine", "Mihalopoulos",email = "cathy.mihalopoulos@deakin.edu.au", role = c("rev"), comment = c(ORCID = "0000-0002-7127-9462")),
                            #person("Jonathan", "Karnon", email ="Jonathan.Karnon@flinders.edu.au", role = c("rev"), comment =c(ORCID = "0000-0003-3220-2099")),
                            #person("Petra","Plencnerova", email = "Petra.Plencnerova@vu.edu.au", role =c("rev"), comment = c(ORCID = "0000-0001-9698-9084")),
                            utils::person("Orygen", role = c("cph", "fnd")),
                            utils::person("VicHealth",role = c("fnd")),
                            utils::person("Victoria University", role =c("fnd"))
                            ),
                            urls_chr = c("https://ready4-dev.github.io/ready4use/",
                                         "https://github.com/ready4-dev/ready4use",
                                         "https://ready4-dev.github.io/ready4/")) %>%
  ready4fun::write_pkg_setup_fls(incr_ver_1L_lgl = F,
                                 delete_contents_of_R_dir = T,
                                 copyright_holders_chr = "Orygen",
                                 check_type_1L_chr = "gh",
                                 path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4use-logo/default.png",
                                 github_repo = "ready4-dev/ready4use",
                                 lifecycle_stage_1L_chr = "experimental",
                                 badges_lup = ready4fun::badges_lup,
                                 addl_badges_ls = list(ready4 = "development"))
# PAUSE FOR INTERACTIVE
#
# 5. MANUAL STEP - ADD Function scripts to "fns","gnrcs" and" mthds" directories.
#
# 6. MAKE CONSTRUCTOR TABLE objects with the metadata about the classes we will be creating.
name_pfx_1L_chr <- "ready4_"
classes_to_make_tb <- dplyr::bind_rows(
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = TRUE,
                                              name_stub_chr = "dist",
                                              pt_ls = list(list("list")),
                                              pt_chkr_pfx_ls = list(list("is.")),
                                              pt_ns_ls = list(list("base")),
                                              vals_ls = list(list(distribution = "character(0)",
                                                                  dist_param_1 = "numeric(0)",
                                                                  dist_param_2 = "numeric(0)",
                                                                  dist_param_3 = "numeric(0)",
                                                                  dist_param_4 = "numeric(0)",
                                                                  transformation = "character(0)")),
                                              class_desc_chr = "ready4 S3 class for list object that summarises the parameters of each distribution"),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = TRUE,
                                              name_stub_chr = "dv_import_lup",
                                              pt_ls = list(list("tibble")),
                                              pt_chkr_pfx_ls = list(list("is_")),
                                              pt_ns_ls = list(list("tibble")),
                                              vals_ls = list(list(file_type = "character(0)",
                                                                  file_name = "character(0)",
                                                                  data_repo = "character(0)",
                                                                  data_repo_ui = "character(0)",
                                                                  data_repo_db_ui = "character(0)",
                                                                  data_repo_file_ext = "character(0)",
                                                                  data_repo_save_type = "character(0)")),
                                              class_desc_chr = "ready4 S3 class for tibble object lookup table of files to be imported from a dataverse."),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = TRUE,
                                              name_stub_chr = "all_import_lup",
                                              pt_ls = list(list("tibble")),
                                              pt_chkr_pfx_ls = list(list("is_")),
                                              pt_ns_ls = list(list("tibble")),
                                              vals_ls = list(list(local_file_src = "character(0)",
                                                                  make_script_src = "character(0)",
                                                                  download_url = "character(0)",
                                                                  inc_file_main = "character(0)",
                                                                  inc_files_to_rename = "list()",
                                                                  new_names_for_inc_files = "list()")),
                                              class_desc_chr = "ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.",
                                              parent_class_chr = "ready4_dv_import_lup"),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = TRUE,
                                              name_stub_chr = "par_struc_mape",
                                              pt_ls = list(list("tibble")),
                                              pt_chkr_pfx_ls = list(list("is_")),
                                              pt_ns_ls = list(list("tibble")),
                                              vals_ls = list(list(param_name = "character(0)",
                                                                  sex_age_band = "character(0)",
                                                                  mape_05_yr_mde = "numeric(0)",
                                                                  mape_10_yr_mde = "numeric(0)",
                                                                  mape_15_yr_mde = "numeric(0)",
                                                                  mape_05_yr_min = "numeric(0)",
                                                                  mape_10_yr_min = "numeric(0)",
                                                                  mape_15_yr_min = "numeric(0)",
                                                                  mape_05_yr_max = "numeric(0)",
                                                                  mape_10_yr_max = "numeric(0)",
                                                                  mape_15_yr_max = "numeric(0)",
                                                                  mape_05_yr_shp = "numeric(0)",
                                                                  mape_10_yr_shp = "numeric(0)",
                                                                  mape_15_yr_shp = "numeric(0)")),
                                              class_desc_chr = "ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors."),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = T,
                                              name_stub_chr = "dictionary",
                                              pt_ls = list(list("tibble")),
                                              pt_chkr_pfx_ls = list(list("is_")),
                                              pt_ns_ls = list(list("tibble")),
                                              vals_ls = list(list(var_nm_chr = "character(0)",
                                                                  var_cat_chr = "character(0)",
                                                                  var_desc_chr = "character(0)",
                                                                  var_type_chr = "character(0)")),
                                              class_desc_chr= "ready4 s3 class defining a data dictionary tibble."),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "local",
                                              slots_ls = c("merge_with_chr_vec","raw_data_dir_chr","pckg_chr","overwrite_lgl", "save_lgl") %>% list(), # Cut: "lup_tbs_r4",
                                              pt_ls = c("character","character","character","logical", "logical") %>% list(), # Cut: "ready4_lookup",
                                              class_desc_chr= "ready4 S4 class defining data to be saved in local directory.",
                                              parent_class_chr = NA_character_), # Cut: ,include_classes = list("ready4_lookup")
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "local_raw",
                                              slots_ls = c("save_type") %>% list(),
                                              pt_ls = c("character") %>% list(),
                                              vals_ls = list(save_type ="raw"),
                                              allowed_vals_ls = list(save_type = "raw"),
                                              class_desc_chr= "ready4 S4 class defining data to be saved in local directory in a raw (unprocessed) format.",
                                              parent_class_chr = "ready4_local",
                                              inc_clss_ls = list("ready4_local")),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "local_proc",
                                              slots_ls = c("save_type","proc_data_dir_chr","import_chr_vec","path_to_starter_sf_chr","import_this_ls") %>% list(),
                                              pt_ls = c("character","character","character","character","list") %>% list(),
                                              vals_ls = list(save_type = "proc"),
                                              allowed_vals_ls = list(save_type = "proc"),
                                              class_desc_chr= "ready4 S4 class defining data to be saved in local directory in a processed (R) format.",
                                              parent_class_chr = "ready4_local",
                                              inc_clss_ls = list("ready4_local")),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "script_data",
                                              slots_ls = c("crs_nbr_vec") %>% list(),
                                              pt_ls = c("numeric") %>% list(),
                                              class_desc_chr= "ready4 S4 class containing data to be passed to a function that constructs a spatial object from a lookup table.",
                                              parent_class_chr = "ready4_local_proc",
                                              inc_clss_ls = list("ready4_local_proc"))
) %>%
  ready4class::ready4_constructor_tbl()
#
# 7. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
utils::data("abbreviations_lup",package = "ready4class")
utils::data("fn_type_lup_tb",package = "ready4class")
utils::data("prototype_lup",package = "ready4class")
pkg_dss_tb <- ready4fun::write_abbr_lup(short_name_chr = c("dv","loc","proc","src",
                                             paste0(name_pfx_1L_chr,classes_to_make_tb$name_stub_chr)),
                          long_name_chr = c("dataverse","local","process","source",
                                            classes_to_make_tb$class_desc_chr),
                          custom_plural_ls = list(proc = "es"),
                          no_plural_chr = classes_to_make_tb$class_desc_chr,
                          url_1L_chr = "https://ready4-dev.github.io/ready4/",
                          seed_lup = abbreviations_lup)
utils::data("abbreviations_lup")
#
# 8. Create function types look-up table and save it as a package dataset
pkg_dss_tb <- fn_type_lup_tb %>%
  ready4fun::add_rows_to_fn_type_lup(fn_type_nm_chr = ready4fun::get_new_fn_types(abbreviations_lup = abbreviations_lup,
                                                                                  fn_type_lup_tb = fn_type_lup_tb),
                                     fn_type_desc_chr = c("Rowbinds lookup tables of the same class, removing duplicates based on priority.",
                                                          "Downloads data files.",
                                                          "Retrieves data from R objects loaded in memory.",
                                                          "Retrieves data about the type of import to be processed.",
                                                          "Retrieves a read function.",
                                                          "Imports data from saved files and loads them into memory as R objects.",
                                                          "Makes a Dataverse import lookup table",
                                                          "Makes an output object of multiple potential classes.",
                                                          #"Makes an instance of the Make Local Process Readyforwhatsnext S4 Class",
                                                          "Saves the native version of a file format.",
                                                          "Updates data from a local file reference to a URL",
                                                          "Updates and object"),
                                     is_generic_lgl = T,
                                     is_method_lgl = T) %>%
  ready4fun::write_dmtd_fn_type_lup(url_1L_chr = "https://ready4-dev.github.io/ready4/",
                                    abbreviations_lup = abbreviations_lup,
                                    pkg_dss_tb = pkg_dss_tb)
utils::data("fn_type_lup_tb")
##
## 9. WRITE and document new classes with the metadata contained in the merged object.
pkg_dss_tb <- classes_to_make_tb %>%
  ready4class::write_classes_and_make_lup(dev_pkg_ns_1L_chr = ready4fun::get_dev_pkg_nm(),
                                          name_pfx_1L_chr = "ready4_",
                                          output_dir_1L_chr = "R",
                                          file_exists_cdn_1L_chr = "overwrite",
                                          abbreviations_lup = abbreviations_lup,
                                          init_class_pt_lup = prototype_lup) %>%
ready4fun::write_and_doc_ds(db_1L_chr = "prototype_lup",
                            title_1L_chr = "Class prototype lookup table",
                            desc_1L_chr = "Metadata on classes used in ready4 suite",
                            pkg_dss_tb = pkg_dss_tb)
#
# 10. Create a table of all functions to document
fns_dmt_tb <- ready4fun::make_dmt_for_all_fns(custom_dmt_ls = list(details_ls = NULL,
                                                                       inc_for_main_user_lgl_ls = list(force_true_chr = c("assert_matches_chr",
                                                                                                                          "assert_single_row_tb",
                                                                                                                          "get_local_path_to_dv_data",
                                                                                                                          "get_r3_from_dv_csv"),
                                                                                       force_false_chr = NA_character_),
                                                                      args_ls_ls = NULL),
                                                 fn_type_lup_tb = fn_type_lup_tb,
                                                 abbreviations_lup = abbreviations_lup)

pkg_dss_tb <- fns_dmt_tb %>%
  ready4fun::write_and_doc_ds(overwrite_1L_lgl = T,
                   db_1L_chr = "fns_dmt_tb",
                   title_1L_chr = "ready4use function documentation table",
                   desc_1L_chr = "A table with the summary information on functions included in the ready4use package.",
                   format_1L_chr = "A tibble",
                   url_1L_chr = "https://ready4-dev.github.io/ready4/",
                   abbreviations_lup = abbreviations_lup,
                   #object_type_lup = object_type_lup,
                   pkg_dss_tb = pkg_dss_tb)
## 11. Write and document.
# NOTE: There seems to be a choice: either create the dataverse dataset from script and then manage updates from
# the web or vice versa. No satisfactory resolution that I can identify.
#
# ds_ls <- dataverse::get_dataset("https://doi.org/10.7910/DVN/OZLSLR")
# dataverse::update_dataset_file("data-raw/abbreviations_lup.csv",
#                                 dataset = ds_ls,
#                                id = ds_ls[["files"]][,c("id", "originalFileName")] %>%
#                                  tibble::as_tibble() %>%
#                                  dplyr::filter(originalFileName=="abbreviations_lup.csv") %>%
#                                  dplyr::pull(id))
# ds_url <- "https://doi.org/10.7910/DVN/OZLSLR"
# write_to_add_urls_to_dss("https://doi.org/10.7910/DVN/OZLSLR",
#                          pkg_dss_tb = pkg_dss_tb)
## BELOW LINE MAY NOT WORK AS DATASET IS SWORD NOT NATIVE
# ds_ls <- write_pkg_dss_to_dv_ds_csvs(pkg_dss_tb,
#                                      dv_nm_1L_chr = "ready4work",
#                                      ds_url_1L_chr = "https://doi.org/10.7910/DVN/OZLSLR",
#                                      parent_dv_dir_1L_chr = "../../../../Data/Dataverse",
#                                      wait_time_in_secs_int = 5L)
## Note files to be rewritten cannot be open in RStudio.
usethis::use_build_ignore("initial_setup.R")
readLines(".github/workflows/R-CMD-check.yaml")[-28] %>%
  writeLines(".github/workflows/R-CMD-check.yaml")
ready4fun::write_and_doc_fn_fls(fns_dmt_tb,
                                r_dir_1L_chr = "R",
                                dev_pkgs_chr = c("ready4fun","ready4class"),
                                update_pkgdown_1L_lgl = T)
#ready4fun::write_links_for_website(user_manual_url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/pdfs/ready4class_0.0.0.9138.pdf",
#developer_manual_url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/pdfs/ready4class_0.0.0.9138_dev.pdf")
#pkgdown::build_site()
#
# 12. Add vignette

