get_file_from_dv <- function(database_ui_chr,
                             filename_chr,
                             save_format_chr,
                             repo_file_format,
                             dataverse_chr = Sys.getenv("DATAVERSE_SERVER"),
                             save_type_chr = "original",
                             save_dir_path_chr = "",
                             read_fn,
                             unlink_lgl = T){
  destination_path_chr <- ifelse(unlink_lgl,
                                 tempfile(),
                                 get_local_path_to_dv_data(save_dir_path_chr = save_dir_path_chr,
                                                       filename_chr = filename_chr,
                                                       save_format_chr = save_format_chr))
  write_dv_file_fl(database_ui_chr = database_ui_chr,
               filename_chr = filename_chr,
               repo_file_format = repo_file_format,
               dataverse_chr = dataverse_chr,
               save_type_chr = save_type_chr,
               destination_path_chr = destination_path_chr)
  file_xxx <- rlang::exec(read_fn,destination_path_chr,stringsAsFactors = F)
  if(unlink_lgl)
    unlink(destination_path_chr)
  file_xxx
  return(file_xxx)
}
get_local_path_to_dv_data <- function(save_dir_path_chr,
                                      filename_chr,
                                      save_format_chr){
  path_chr <- paste0(ifelse(save_dir_path_chr!="",paste0(save_dir_path_chr,"/"),""),
         filename_chr,
         save_format_chr)
  return(path_chr)
}
get_r3_from_dv_csv <- function(file_name_chr,
                               data_repo_db_ui_chr,
                               data_repo_ui_chr = NA_character_,
                               r3_fn = ready4_all_import_lup){
  tb_r3 <- tibble::tibble(file_type = ".csv",
                 file_name = file_name_chr,
                 data_repo = NA_character_,
                 data_repo_ui = data_repo_ui_chr,
                 data_repo_db_ui =  data_repo_db_ui_chr,
                 data_repo_file_ext = ".tab",
                 data_repo_save_type = "original") %>%
    ready4_dv_import_lup() %>%
    get_data() %>%
    make_r3_from_csv_tb(r3_fn)
  return(tb_r3)
}
get_valid_path_chr <- function(x){
  assert_file_exists(x)
  valid_path_chr <- x
  return(valid_path_chr)
}
