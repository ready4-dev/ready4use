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
options(usethis.description = list(
  Package = ready4fun::get_dev_pkg_nm(),
  Title =  "Readyforwhatsnext Methods for retrieving and managing data.",
  Description = "ready4use provides a set of classes and methods for general data management tasks throughout the readyforwhatsnext suite.",
  `Authors@R` = c(utils::person(
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
  License = usethis::use_gpl3_license("Orygen"),
  URL = c(#"https://readyforwhatsnext.github.io/ready4class/,
          #https://github.com/readyforwhatsnext/ready4class,
          "https://readyforwhatsnext.github.io/readyforwhatsnext/") # Updated from first run
))
# Deletes contents of R directory and resets DESCRIPTION and NAMESPACE files.
ready4fun::write_pkg_setup_fls(incr_ver_1L_lgl = F,
  delete_contents_of_R_dir = T,
  copyright_holders_chr = "Orygen",
  use_travis_1L_lgl = F,
  path_to_pkg_logo_1L_chr = "../../../../Documentation/Images/ready4use-logo/default.png",
  github_repo_1L_chr = "readyforwhatsnext/ready4use",
  lifecycle_stage_1L_chr = "experimental")
# PAUSE FOR INTERACTIVE
#
# 5. MANUAL STEP - ADD Function scripts to "fns","gnrcs" and" mthds" directories.
#
# 6. MAKE CONSTRUCTOR TABLE objects with the metadata about the classes we will be creating.
source("data-raw/MAKE_CLASSES_S3.R")
source("data-raw/MAKE_CLASSES_S4.R")
name_pfx_1L_chr <- "ready4_"
classes_to_make_tb <- dplyr::bind_rows(s3_classes_to_make_tb, # Bug: Need to delete all named C3_ and C4_ starting files as initial step (ie don't wait for relevant place in sequence)
                                       s4_classes_to_make_tb)
#
# 7. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
data("abbreviations_lup",package = "ready4class")
data("fn_type_lup_tb",package = "ready4class")
data("prototype_lup",package = "ready4class")

pkg_dss_tb <- ready4fun::write_abbr_lup(short_name_chr = c("dv","loc","proc","src",
                                             paste0(name_pfx_1L_chr,classes_to_make_tb$name_stub_chr)),
                          long_name_chr = c("dataverse","local","process","source",
                                            classes_to_make_tb$class_desc_chr),
                          custom_plural_ls = list(proc = "es"),
                          no_plural_chr = classes_to_make_tb$class_desc_chr,
                          url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                          seed_lup = abbreviations_lup)
data("abbreviations_lup")
#
# 8. Create function types look-up table and save it as a package dataset


pkg_dss_tb <- fn_type_lup_tb %>%
  ready4fun::add_rows_to_fn_type_lup(fn_type_nm_chr = ready4fun::get_new_fn_types(abbreviations_lup = abbreviations_lup,
                                                                                  fn_type_lup_tb = fn_type_lup_tb),
                                     fn_type_desc_chr = c("Downloads data files.",
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
  ready4fun::write_dmtd_fn_type_lup(url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                                    abbreviations_lup = abbreviations_lup,
                                    pkg_dss_tb = pkg_dss_tb)
data("fn_type_lup_tb")
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
                            desc_1L_chr = "Metadata on classes used in readyforwhatsnext suite",
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
## 11. Write and document.
# dataverse::update_dataset_file("data-raw/abbreviations_lup.csv",
#                                 dataset = dataverse::get_dataset("https://doi.org/10.7910/DVN/OZLSLR"),
#                                id = ds_fls_ls[[1]])
#ds_url <- "https://doi.org/10.7910/DVN/OZLSLR"
# write_to_add_urls_to_dss("https://doi.org/10.7910/DVN/OZLSLR",
#                          pkg_dss_tb = pkg_dss_tb)
## Note files to be rewritten cannot be open in RStudio.
ready4fun::write_and_doc_fn_fls(fns_dmt_tb,
                                r_dir_1L_chr = "R",
                                dev_pkgs_chr = c("ready4fun","ready4class"),
                                update_pkgdown_1L_lgl = T)
#ready4fun::write_links_for_website(user_manual_url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/pdfs/ready4class_0.0.0.9138.pdf",
#developer_manual_url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/pdfs/ready4class_0.0.0.9138_dev.pdf")
pkgdown::build_site()
#
# 12. Add vignette

