## Script to create the object from which S4 classes will be made.
##
## 1. Prerequisites
##    None
##
## 2. Make the Make Class Table summarising the metadata about the S3 classes that we wish to create and export with this package.
s4_classes_to_make_tb <- ready4class::ready4_class_make_tb() %>%
  tibble::add_case(name_stub = "local",
                   class_slots = c("merge_with_chr_vec","raw_data_dir_chr","pckg_chr","overwrite_lgl", "save_lgl") %>% list(), # Cut: "lup_tbs_r4",
                   prototype = c("character","character","character","logical", "logical") %>% list(), # Cut: "ready4_lookup",
                   class_desc = "Object defining data to be saved in local directory.",
                   parent_class = NA_character_) %>% # Cut: ,include_classes = list("ready4_lookup")
  tibble::add_case(name_stub = "local_raw",
                   class_slots = c("save_type") %>% list(),
                   prototype = c("character") %>% list(),
                   values = list(save_type ="raw"),
                   allowed_values = list(save_type = "raw"),
                   class_desc = "Object defining data to be saved in local directory in a raw (unprocessed) format.",
                   parent_class = "ready4_local",
                   include_classes = list("ready4_local")) %>%
  tibble::add_case(name_stub = "local_proc",
                   class_slots = c("save_type","proc_data_dir_chr","import_chr_vec","path_to_starter_sf_chr","import_this_ls") %>% list(),
                   prototype = c("character","character","character","character","list") %>% list(),
                   values = list(save_type = "proc"),
                   allowed_values = list(save_type = "proc"),
                   class_desc = "Object defining data to be saved in local directory in a processed (R) format.",
                   parent_class = "ready4_local",
                   include_classes = list("ready4_local")) %>%
  tibble::add_case(name_stub = "script_data",
                   class_slots = c(#"proc_data_dir_chr",
                    # "raw_data_dir_chr",
                     "crs_nbr_vec") %>% list(),
                   prototype = c(#"character",
                                 "numeric") %>% list(),
                   class_desc = "Data to be passed to a function that constructs a spatial object from a lookup table.",
                   parent_class = "ready4_local_proc",
                   include_classes = list("ready4_local_proc")) %>%
  dplyr::mutate(make_s3 = FALSE) %>%
  ready4class::remake_ls_cols()
