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
                            authors_prsn = c(utils::person(
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
                                 delete_r_dir_cnts_1L_lgl = T,
                                 copyright_holders_chr = "Orygen",
                                 check_type_1L_chr = "gh",
                                 path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4use-logo/default.png",
                                 github_repo = "ready4-dev/ready4use",
                                 lifecycle_stage_1L_chr = "experimental",
                                 badges_lup = ready4fun::badges_lup,
                                 addl_badges_ls = list(ready4 = "authoring"))
# PAUSE FOR INTERACTIVE
#
# 5. MANUAL STEP - ADD Function scripts to "fns","gnrcs" and" mthds" directories.
#
# 6. MAKE CONSTRUCTOR TABLE objects with the metadata about the classes we will be creating.
source("data-raw/MAKE_CLASSES.R")
#
# 7. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
# Temporarily relies on MAKE_HOUSESTYLE_DSS.R
#fn_type_lup_tb <- ready4fun::get_rds_from_dv("abbreviations_lup") # Uncomment once dataverse DRAFT published
pkg_dss_tb <- abbreviations_lup %>%
  ready4fun::write_abbr_lup()
utils::data("abbreviations_lup")
#
# 8. Create function types look-up table and save it as a package dataset
# Temporarily relies on MAKE_HOUSESTYLE_DSS.R
#fn_type_lup_tb <- ready4fun::get_rds_from_dv("fn_type_lup_tb") # Uncomment once dataverse DRAFT published
pkg_dss_tb <- fn_type_lup_tb %>%
  ready4fun::write_dmtd_fn_type_lup(abbreviations_lup = abbreviations_lup,
                                    pkg_dss_tb = pkg_dss_tb)
utils::data("fn_type_lup_tb")
##
## 9. WRITE and document new classes with the metadata contained in the merged object.
prototype_lup <- ready4fun::get_rds_from_dv("prototype_lup")
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
fns_dmt_tb <- ready4fun::make_dmt_for_all_fns(paths_ls = make_fn_nms(),
                                              undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(),
                                              custom_dmt_ls = list(details_ls = NULL,
                                                                   inc_for_main_user_lgl_ls = list(force_true_chr = c("add_labels_from_dictionary",
                                                                                                                      "assert_matches_chr",
                                                                                                                      "assert_single_row_tb",
                                                                                                                      "get_fl_id_from_dv_ls",
                                                                                                                      #"get_import_type_ls",
                                                                                                                      "get_local_path_to_dv_data",
                                                                                                                      "get_r3_from_dv_csv",
                                                                                                                      "write_fls_to_dv_ds",
                                                                                                                      "write_paired_ds_fls_to_dv",
                                                                                                                      "write_pkg_dss_to_dv_ds_csvs"),
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
## Note files to be rewritten cannot be open in RStudio.
usethis::use_build_ignore("initial_setup.R")
readLines(".github/workflows/R-CMD-check.yaml")[-28] %>%
  writeLines(".github/workflows/R-CMD-check.yaml")
ready4fun::write_and_doc_fn_fls(fns_dmt_tb,
                                r_dir_1L_chr = "R",
                                dev_pkgs_chr = c("ready4fun","ready4class","dataverse"),
                                update_pkgdown_1L_lgl = T)
##
data("prototype_lup")
prototype_lup %>%
  write_paired_ds_fls_to_dv(fl_nm_1L_chr = "abbreviations_lup",
                            desc_1L_chr = "Abbreviations lookup table")
##
#ready4fun::write_links_for_website(user_manual_url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/pdfs/ready4class_0.0.0.9138.pdf",
#developer_manual_url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/pdfs/ready4class_0.0.0.9138_dev.pdf")
#pkgdown::build_site()
#
# 12. Add vignette

