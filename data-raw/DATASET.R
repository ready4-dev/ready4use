## Script to make classes and save updated prototype table.
## This script creates the data files embedded with this package.
# 1. Load magrittr package to that the pipe operator ("%>%") can be used in this script.
library(magrittr)
#
# 2. Set up standardised core package elements.
# For Workflow:
# https://docs.github.com/en/github/using-git/setting-your-username-in-git
# plus user.email
#
ready4fun::write_pkg_setup_fls_R(#make_tmpl_vignette_lgl = T,
                                 incr_ver_lgl = F)
#
# 3. MANUAL STEP - ADD Function scripts to "fns","gnrcs" and" mthds" directories.
#
# 4. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
data("abbreviations_lup",package = "ready4class")
ready4fun::make_abbr_lup_tb(short_name_chr_vec = c("dv","loc","proc","src"#,
                                                   # "ready4_class_make_tb","ready4_class_pt_lup"
                                                   ),
                            long_name_chr_vec = c("dataverse","local","process","source"#,
                                                  # "Class Make Table readyforwhatsnext S3 class",
                                                  # "Class Prototype Lookup Table readyforwhatsnext S3 class"
                                                  ),
                            custom_plural_ls = list(proc = "es"),
                            no_plural_chr_vec = NA_character_,# c("ready4_class_make_tb","ready4_class_pt_lup"),
                            url_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                            seed_lup = abbreviations_lup)
data("abbreviations_lup")
#
# 5. Create function types and generics look-up tables
data("fn_type_lup_tb",package = "ready4class")
fn_type_lup_tb %>%
  ready4fun::add_rows_to_fn_type_lup_tb(fn_type_nm_chr = c("Download Data","Get Data","Import Data","Get Import Type List",
                                                           "Get Read Function", "Make Dataverse Import Lookup Table","Make Import Output Object of Multiple Potential Types","Make Local Process Readyforwhatsnext S4",
                                                           "Save Raw","Update Source Local to Url","Update this"),
                                        fn_type_desc_chr = c("Downloads data files.",
                                                            "Retrieves data from R objects loaded in memory.",
                                                            "Imports data from saved files and loads them into memory as R objects.",
                                                            "Retrieves data about the type of import to be processed.",
                                                            "Retrieves a read function.",
                                                            "Makes a Dataverse import lookup table",
                                                            "Makes an output object of multiple potential classes.",
                                                            "Makes an instance of the Make Local Process Readyforwhatsnext S4 Class",
                                                            "Saves the native version of a file format.",
                                                            "Updates data from a local file reference to a URL",
                                                            "Updates and object"),
                                        is_generic_lgl = T,
                                        is_method_lgl = T) %>%
  ready4fun::make_and_doc_fn_type_R(url_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                                    abbreviations_lup = abbreviations_lup)
data("fn_type_lup_tb")
#
# 6. Create a table of all functions to document
all_fns_dmt_tb <- ready4fun::make_all_fns_dmt_tb(custom_dmt_ls = list(details_ls = NULL,
                                                                      export_ls = list(force_true_chr_vec = NA_character_,
                                                                                       force_false_chr_vec = NA_character_),
                                                                      args_ls_ls = NULL),
                                                 fn_type_lup_tb = fn_type_lup_tb,
                                                 abbreviations_lup = abbreviations_lup)

## 7. Write and document.
## Note files to be rewritten cannot be open in RStudio.
ready4fun::write_and_doc_fn_fls_R(all_fns_dmt_tb,
                                  r_dir_chr = "R")
ready4fun::write_ns_imps_to_desc(incr_ver_lgl = F)
## 8. Run script to create MAKE CLASS TABLE objects with the metadata about the classes we will be creating.
source("data-raw/MAKE_CLASSES_S3.R")
source("data-raw/MAKE_CLASSES_S4.R")
##
## 9. Merge the MAKE CLASS TABLE objects and apply the method to make new classes with the metadata contained in the merged object.
prototype_lup <- dplyr::bind_rows(s3_classes_to_make_tb, # Bug: Need to delete all named C3_ and C4_ starting files as initial step (ie don't wait for relevant place in sequence)
                                  s4_classes_to_make_tb
                                  ) %>%
  ready4class::make_and_update(dev_pckg_namespace = ready4fun::get_dev_pkg_nm_1L_chr(),
                               name_prefix = "ready4_",
                               output_dir = "R",
                               file_exists_logic = "overwrite")
##
## 10. Save a copy of a PROTOTYPE LOOKUP TABLE object with details about the newly created classes.
usethis::use_data(prototype_lup,overwrite = T)
##
## 11. Document.
## Note: You will first need to consult the NAMESPACE file to see which packages you need to add to the DESCRIPTION file with the following function calls:
ready4fun::write_ns_imps_to_desc(incr_ver_lgl = T)

# 12. Add vignette

