get_data.ready4_dv_import_lup <- function(x,
                                          save_dir_path_1L_chr = "",
                                          unlink_1L_lgl = T,
                                          server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                                          key_1L_chr = Sys.getenv("DATAVERSE_KEY")){
  data_ls <- purrr::map2(1:nrow(x),
                         get_read_fn(x),
                         ~ get_file_from_dv(ds_ui_1L_chr = x$data_repo_db_ui[.x],
                                            fl_nm_1L_chr = x$file_name[.x],
                                            save_fmt_1L_chr = x$file_type[.x],
                                            repo_fl_fmt_1L_chr = x$data_repo_file_ext[.x],
                                            server_1L_chr = ifelse(is.na(x$data_repo_ui[.x]),
                                                                   server_1L_chr,
                                                                   x$data_repo_ui[.x]),
                                            key_1L_chr = key_1L_chr,
                                            save_type_1L_chr = ifelse(is.na(x$data_repo_save_type[.x]),
                                                                      "original",
                                                                      x$data_repo_save_type[.x]),
                                            save_dir_path_1L_chr = save_dir_path_1L_chr,
                                            read_fn = .y,
                                            unlink_1L_lgl = unlink_1L_lgl))
  if(length(data_ls)>1)
    data_ls
  else
    data_ls[[1]]
}
