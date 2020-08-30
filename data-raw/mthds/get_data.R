get_data.ready4_dv_import_lup <- function(x,
                                          save_dir_path_chr = "",
                                          unlink_lgl = T){
  data_ls <- purrr::map2(1:nrow(x),
                         get_read_fn(x),
                         ~ get_file_from_dv(database_ui_chr = x$data_repo_db_ui[.x],
                                            filename_chr = x$file_name[.x],
                                            save_format_chr = x$file_type[.x],
                                            repo_file_format = x$data_repo_file_ext[.x],
                                            dataverse_chr = ifelse(is.na(x$data_repo_ui[.x]),
                                                                   Sys.getenv("DATAVERSE_SERVER")
                                                                   ,x$data_repo_ui[.x]),
                                            save_type_chr = ifelse(is.na(x$data_repo_save_type[.x]),
                                                                   "original",
                                                                   x$data_repo_save_type[.x]),
                                            save_dir_path_chr = save_dir_path_chr,
                                            read_fn = .y,
                                            unlink_lgl = unlink_lgl))
  if(length(data_ls)>1)
    data_ls
  else
    data_ls[[1]]
}
