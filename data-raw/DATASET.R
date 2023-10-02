library(ready4fun)
library(ready4show)
pkg_desc_ls <- ready4fun::make_pkg_desc_ls(pkg_title_1L_chr = "Author, Ingest, Label and Share Health Economic Model Datasets" %>% tools::toTitleCase(),
                                           pkg_desc_1L_chr = "ready4use provides a set of tools for managing data for models developed with the ready4 framework (https://www.ready4-dev.com/).
  This development version of the ready4use package has been made available as part of the process of testing and documenting the package.
  You should only trial this software if you feel confident that you understand what it does and have created a sandpit area in which you can safely undertake testing. If you have any questions, please contact the authors (matthew.hamilton1@monash.edu).",
                                           authors_prsn = c(utils::person(
                                             given = "Matthew",family = "Hamilton", email =
                                               "matthew.hamilton1@monash.edu",role = c("aut",
                                                                                         "cre"),comment = c(ORCID = "0000-0001-7407-9194")
                                           ),
                                           utils::person("Glen", "Wiesner", #email = "Glen.Wiesner@vu.edu.au",
                                                         role = c("aut"), comment = c(ORCID = "0000-0002-0071-130X")),
                                           utils::person("Orygen", role = c("cph", "fnd")),
                                           utils::person("Australian Government Research Training Program", role =c("fnd")),
                                           utils::person("VicHealth",role = c("fnd")),
                                           utils::person("Victoria University", role =c("fnd"))
                                           ),
                                           urls_chr = c("https://ready4-dev.github.io/ready4use/",
                                                        "https://github.com/ready4-dev/ready4use",
                                                        "https://www.ready4-dev.com/"))
x <- pkg_desc_ls %>%
  ready4fun::make_manifest(addl_pkgs_ls = ready4fun::make_addl_pkgs_ls(#depends_chr = "ready4",
                                                                       suggests_chr = c("rmarkdown")),
                           build_ignore_ls = ready4fun::make_build_ignore_ls(file_nms_chr = c("initial_setup.R")),
                           check_type_1L_chr = "ready4",
                           custom_dmt_ls = ready4fun::make_custom_dmt_ls(user_manual_fns_chr = c("add_labels_from_dictionary",
                                                                                                 "assert_matches_chr",
                                                                                                 "assert_single_row_tb",
                                                                                                 "get_r3_from_dv_csv"#, "write_fls_to_dv_ds"
                           )),
                           copyright_holders_chr = "Orygen",
                           dev_pkgs_chr = c("ready4","ready4show"),
                           lifecycle_stage_1L_chr = "experimental",
                           path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4use-logo/default.png",
                           piggyback_to_1L_chr = "ready4-dev/ready4",
                           ready4_type_1L_chr = "authoring",
                           zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5644336.svg)](https://doi.org/10.5281/zenodo.5644336)")
y <- dplyr::bind_rows(
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = TRUE,
                                               name_stub_chr = "distributions",
                                               pt_ls = list(list("list")),
                                               pt_chkr_pfx_ls = list(list("is.")),
                                               pt_ns_ls = list(list("base")),
                                               vals_ls = list(list(distribution_chr = "character(0)",
                                                                   dstr_param_1_dbl = "numeric(0)",
                                                                   dstr_param_2_dbl = "numeric(0)",
                                                                   dstr_param_3_dbl = "numeric(0)",
                                                                   dstr_param_4_dbl = "numeric(0)",
                                                                   transformation_chr = "character(0)")),
                                               class_desc_chr = "List object that summarises the parameters of each distribution"),
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = TRUE,
                                               name_stub_chr = "dataverses",
                                               pt_ls = list(list("tibble")),
                                               pt_chkr_pfx_ls = list(list("is_")),
                                               pt_ns_ls = list(list("tibble")),
                                               vals_ls = list(list(file_type_chr = "character(0)",
                                                                   file_name_chr = "character(0)",
                                                                   data_repo_chr = "character(0)",
                                                                   data_repo_ui_chr = "character(0)",
                                                                   data_repo_db_ui_chr = "character(0)",
                                                                   data_repo_file_ext_chr = "character(0)",
                                                                   data_repo_save_type_chr = "character(0)")),
                                               class_desc_chr = "Tibble object lookup table of files to be imported from a dataverse."),
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = TRUE,
                                               name_stub_chr = "imports",
                                               pt_ls = list(list("tibble")),
                                               pt_chkr_pfx_ls = list(list("is_")),
                                               pt_ns_ls = list(list("tibble")),
                                               vals_ls = list(list(local_file_src_chr = "character(0)",
                                                                   path_to_make_script_chr = "character(0)",
                                                                   download_url_chr = "character(0)",
                                                                   inc_file_main_chr = "character(0)",
                                                                   inc_fls_to_rename_ls = "list()",
                                                                   new_nms_for_inc_fls_ls = "list()")),
                                               class_desc_chr = "Tibble object lookup table of sources of raw (un-processed) data to import.",
                                               parent_class_chr = "ready4use_dataverses"),
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = TRUE,
                                               name_stub_chr = "mapes",
                                               pt_ls = list(list("tibble")),
                                               pt_chkr_pfx_ls = list(list("is_")),
                                               pt_ns_ls = list(list("tibble")),
                                               vals_ls = list(list(param_name_chr = "character(0)",
                                                                   sex_age_band_chr = "character(0)",
                                                                   mape_05_yr_mde_dbl = "numeric(0)",
                                                                   mape_10_yr_mde_dbl = "numeric(0)",
                                                                   mape_15_yr_mde_dbl = "numeric(0)",
                                                                   mape_05_yr_min_dbl = "numeric(0)",
                                                                   mape_10_yr_min_dbl = "numeric(0)",
                                                                   mape_15_yr_min_dbl = "numeric(0)",
                                                                   mape_05_yr_max_dbl = "numeric(0)",
                                                                   mape_10_yr_max_dbl = "numeric(0)",
                                                                   mape_15_yr_max_dbl = "numeric(0)",
                                                                   mape_05_yr_shp_dbl = "numeric(0)",
                                                                   mape_10_yr_shp_dbl = "numeric(0)",
                                                                   mape_15_yr_shp_dbl = "numeric(0)")),
                                               class_desc_chr = "Tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors."),
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = T,
                                               name_stub_chr = "dictionary",
                                               pt_ls = list(list("tibble")),
                                               pt_chkr_pfx_ls = list(list("is_")),
                                               pt_ns_ls = list(list("tibble")),
                                               vals_ls = list(list(var_nm_chr = "character(0)",
                                                                   var_ctg_chr = "character(0)",
                                                                   var_desc_chr = "character(0)",
                                                                   var_type_chr = "character(0)")),
                                               class_desc_chr= "A data dictionary tibble."),
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                               name_stub_chr = "Files",
                                               slots_ls = list("merge_itms_chr","raw_fls_dir_1L_chr","pkg_1L_chr","overwrite_1L_lgl", "write_1L_lgl") %>% list(), # Cut: "lup_tbs_r4",
                                               pt_ls = list("character","character","character","logical", "logical") %>% list(), # Cut: "ready4class_lookup",
                                               class_desc_chr= "Metadata for dataset(s) to be saved in local directory.",
                                               parent_class_chr = "Ready4Module"), # NA_character_ # Cut: ,include_classes = list("ready4class_lookup")
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                               name_stub_chr = "Raw",
                                               slots_ls = list("write_type_1L_chr") %>% list(),
                                               pt_ls = list("character") %>% list(),
                                               vals_ls = list(write_type_1L_chr ="raw"),
                                               allowed_vals_ls = list(write_type_1L_chr = "raw"),
                                               class_desc_chr= "Metadata for dataset(s) to be saved in local directory in a raw (unprocessed) format.",
                                               parent_class_chr = "Ready4useFiles"),
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                               name_stub_chr = "Processed",
                                               slots_ls = list("write_type_1L_chr","processed_fls_dir_1L_chr","imports_chr","path_to_seed_sf_1L_chr","imports_ls") %>% list(),
                                               pt_ls = list("character","character","character","character","list") %>% list(),
                                               vals_ls = list(write_type_1L_chr = "proc"),
                                               allowed_vals_ls = list(write_type_1L_chr = "proc"),
                                               class_desc_chr= "Metadata for dataset(s) to be saved in local directory in a processed (R) format.",
                                               parent_class_chr = "Ready4useFiles"),
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                               name_stub_chr = "Arguments",
                                               slots_ls = list("crs_nbr_dbl") %>% list(), # Change
                                               pt_ls = list("numeric") %>% list(),
                                               class_desc_chr= "Arguments for a function that constructs a spatial object.",
                                               parent_class_chr = "Ready4useProcessed"),
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                               name_stub_chr = "Dyad",
                                               slots_ls = list("ds_tb","dictionary_r3") %>% list(), # Change
                                               pt_ls = list("tbl_df","ready4use_dictionary") %>% list(),
                                               class_desc_chr= "A dataset and data dictionary pair.",
                                               parent_class_chr = "Ready4Module"),
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                               name_stub_chr = "Ingest",
                                               slots_ls = list("objects_ls",#"names_chr",
                                                               "descriptions_chr") %>% list(), # Change
                                               pt_ls = list("list",#"character",
                                                            "character") %>% list(),
                                               class_desc_chr = "Ingested data and descriptive metadata.",
                                               parent_class_chr = "Ready4Module"),
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                               name_stub_chr = "Repos",
                                               slots_ls = list("dv_nm_1L_chr","dv_ds_metadata_ls",
                                                               "dv_ds_nm_1L_chr","dv_server_1L_chr",
                                                               "dv_url_pfx_1L_chr","fl_nms_chr",
                                                               "gh_repo_1L_chr","gh_tag_1L_chr"
                                               ) %>% list(), # Change
                                               pt_ls = list("character","list",
                                                            "character","character","character","character","character","character") %>% list(),
                                               class_desc_chr= "Metadata about online data repositories.",
                                               parent_class_chr = "Ready4Module"),
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                               name_stub_chr = "Pointer",
                                               slots_ls = list("a_Ready4usePaths","b_Ready4useRepos") %>% list(), # Change
                                               pt_ls = list("Ready4Module","Ready4useRepos") %>% list(),
                                               class_desc_chr = "Metadata on local and remote data storage locations.",
                                               parent_class_chr = "Ready4Module",
                                               inc_clss_ls = list("Ready4useRepos") %>% list()),
  ready4class::make_pt_ready4class_constructor(make_s3_lgl = FALSE,
                                               name_stub_chr = "Record",
                                               slots_ls = list("a_Ready4usePointer","b_Ready4useIngest") %>% list(), # Change
                                               pt_ls = list("Ready4usePointer","Ready4useIngest") %>% list(),
                                               class_desc_chr= "Ingested data, descriptive metadata and provenance details.",
                                               parent_class_chr = "Ready4Module",
                                               inc_clss_ls = list("Ready4usePointer","Ready4useIngest") %>% list())
) %>%
  ready4class::ready4class_constructor()
z <- ready4pack::make_pt_ready4pack_manifest(x,
                                             constructor_r3 = y) %>%
  ready4pack::ready4pack_manifest()
z <- author(z)
ready4::write_extra_pkgs_to_actions()
readLines(".github/workflows/R-CMD-check.yaml") %>%
  #stringr::str_replace_all("r-lib/actions/setup-r@master", "r-lib/actions/setup-r@v2") %>%
  #stringr::str_replace_all("r-lib/actions/setup-pandoc@master", "r-lib/actions/setup-pandoc@v2") %>%
  stringr::str_replace_all("- \\{os: windows-latest, r: '3.6'\\}", "#- \\{os: windows-latest, r: '3.6'\\}") %>%
  stringr::str_replace_all("- \\{os: ubuntu-20.04,   r: 'oldrel', ", "#- \\{os: ubuntu-20.04,   r: 'oldrel', ") %>%
  purrr::discard_at(2:4) %>%
  writeLines(con = ".github/workflows/R-CMD-check.yaml")
write_to_edit_workflow("pkgdown.yaml") # In other packages, run for "test-coverage.yaml" as well.
readLines("_pkgdown.yml") %>%
  stringr::str_replace_all("  - text: Model", "  - text: Framework & Model") %>%
  writeLines(con = "_pkgdown.yml")
devtools::build_vignettes()
