
get_import_type_ls.ready4_all_import_lup <- function(x,
                                                    inc_script_lgl = T,
                                                    forced_choice_chr = NA_character_){
  assert_single_row_tb(x)
  options_ls <- list(script_chr = x$make_script_src,
                     local_chr = x$local_file_src,
                     repo_chr = x$data_repo_db_ui,
                     source_url_chr = x$download_url) %>%
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
