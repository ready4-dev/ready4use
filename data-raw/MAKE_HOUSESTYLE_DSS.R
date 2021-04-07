library(magrittr)
ready4fun::read_fns("data-raw/fns")
source("data-raw/MAKE_CLASSES.R")
abbreviations_lup <- ready4fun::get_rds_from_dv("abbreviations_lup")
# Edits go here
abbreviations_lup <- abbreviations_lup %>%
  ready4fun::update_abbr_lup(short_name_chr = c(#"dv",
                                                "loc",#"lup",
                                                "proc","src",
                                                paste0(name_pfx_1L_chr,classes_to_make_tb$name_stub_chr)),
                             long_name_chr = c(#"dataverse",
                                               "local",#"lookup table",
                                               "process","source",
                                               classes_to_make_tb$class_desc_chr),
                             custom_plural_ls = list(proc = "es"),
                             no_plural_chr = classes_to_make_tb$class_desc_chr)
fn_type_lup_tb <- ready4fun::get_rds_from_dv("fn_type_lup_tb")
# Edits go here
fn_type_lup_tb <- fn_type_lup_tb %>%
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
                                                          #"Makes an instance of the Make Local Process ready4 S4 Class",
                                                          "Saves the native version of a file format.",
                                                          "Updates data from a local file reference to a URL",
                                                          "Updates and object"),
                                     is_generic_lgl = T,
                                     is_method_lgl = T)
# Push updates to dataverse
abbreviations_lup %>%
  write_paired_ds_fls_to_dv(fl_nm_1L_chr = "abbreviations_lup",
                            desc_1L_chr = "Abbreviations lookup table")
fn_type_lup_tb %>%
  write_paired_ds_fls_to_dv(fl_nm_1L_chr = "fn_type_lup_tb",
                            desc_1L_chr = "Function type lookup table")
# Previous edits

