get_file_from_dv <- function(ds_ui_1L_chr,
                             fl_nm_1L_chr,
                             save_fmt_1L_chr,
                             repo_fl_fmt_1L_chr,
                             key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                             server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), # was dataverse_chr
                             save_type_1L_chr = "original",
                             save_dir_path_1L_chr = "",
                             read_fn,
                             unlink_1L_lgl = T){
  destination_path_chr <- ifelse(unlink_1L_lgl,
                                 tempfile(),
                                 get_local_path_to_dv_data(save_dir_path_1L_chr = save_dir_path_1L_chr,
                                                       fl_nm_1L_chr = fl_nm_1L_chr,
                                                       save_fmt_1L_chr = save_fmt_1L_chr))
  write_dv_fl_to_loc(ds_ui_1L_chr = ds_ui_1L_chr,
                     fl_nm_1L_chr = fl_nm_1L_chr,
                     repo_fl_fmt_1L_chr = repo_fl_fmt_1L_chr,
                     key_1L_chr = key_1L_chr,
                     server_1L_chr = server_1L_chr,
                     save_type_1L_chr = save_type_1L_chr,
                     dest_path_1L_chr = destination_path_chr)
  file_xxx <- rlang::exec(read_fn,destination_path_chr,stringsAsFactors = F)
  if(unlink_1L_lgl)
    unlink(destination_path_chr)
  file_xxx
  return(file_xxx)
}
get_fl_id_from_dv_ls <-  function (ds_ls, fl_nm_1L_chr, nms_chr = NA_character_)
{
  if (is.na(nms_chr[1])) {
    nms_chr <- purrr::map2_chr(ds_ls$files$originalFileName,
                               ds_ls$files$filename, ~ifelse(is.na(.x), .y, .x))
  }
  if (fl_nm_1L_chr %in% nms_chr) {
    id_1L_chr <- ready4fun::get_from_lup_obj(ds_ls$files[,
                                                         names(ds_ls$files) %>% unique()] %>% tibble::as_tibble(),
                                             match_var_nm_1L_chr = ifelse(fl_nm_1L_chr %in%
                                                                            ds_ls$files$originalFileName, "originalFileName",
                                                                          "filename"), match_value_xx = fl_nm_1L_chr,
                                             target_var_nm_1L_chr = "id", evaluate_lgl = F)
  }
  else {
    id_1L_chr <- NA_character_
  }
  return(id_1L_chr)
}
get_local_path_to_dv_data <- function(save_dir_path_1L_chr,
                                      fl_nm_1L_chr,
                                      save_fmt_1L_chr){
  path_chr <- paste0(ifelse(save_dir_path_1L_chr!="",paste0(save_dir_path_1L_chr,"/"),""),
         fl_nm_1L_chr,
         save_fmt_1L_chr)
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
