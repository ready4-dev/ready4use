procure.ready4use_imports <- function(x,
                                      inc_script_lgl = T,
                                      forced_choice_chr = NA_character_){
  assert_single_row_tb(x)
  options_ls <- list(script_chr = x$path_to_make_script_chr,
                     local_chr = x$local_file_src_chr,
                     repo_chr = x$data_repo_db_ui_chr,
                     source_url_chr = x$download_url_chr) %>%
    purrr::discard(is.na)
  if("script_chr" %in% names(options_ls) & !inc_script_lgl)
    options_ls$script_chr <- NULL
  if(!is.na(forced_choice_chr)){
    if(!forced_choice_chr %in% names(options_ls))
      stop("Forced choice option is not available from input lookup table")
    options_ls <- options_ls[names(options_ls)==forced_choice_chr]
  }
  options_ls[1]
}
procure.ready4use_dataverses <- function(x,
                                         save_dir_path_1L_chr = "",
                                         unlink_1L_lgl = T,
                                         server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                                         key_1L_chr = Sys.getenv("DATAVERSE_KEY")){
  data_ls <- purrr::map2(1:nrow(x),
                         manufacture(x),
                         ~ get_file_from_dv(ds_ui_1L_chr = x$data_repo_db_ui_chr[.x],
                                            fl_nm_1L_chr = x$file_name_chr[.x],
                                            save_fmt_1L_chr = x$file_type_chr[.x],
                                            repo_fl_fmt_1L_chr = x$data_repo_file_ext_chr[.x],
                                            server_1L_chr = ifelse(is.na(x$data_repo_ui_chr[.x]),
                                                                   server_1L_chr,
                                                                   x$data_repo_ui_chr[.x]),
                                            key_1L_chr = key_1L_chr,
                                            save_type_1L_chr = ifelse(is.na(x$data_repo_save_type_chr[.x]),
                                                                      "original",
                                                                      x$data_repo_save_type_chr[.x]),
                                            save_dir_path_1L_chr = save_dir_path_1L_chr,
                                            read_fn = .y,
                                            unlink_1L_lgl = unlink_1L_lgl))
  if(length(data_ls)>1)
   data_xx <- data_ls
  else
    data_xx <- data_ls[[1]]
  return(data_xx)
}
