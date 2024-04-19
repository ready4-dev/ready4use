get_drop_offs <- function(X_Ready4useDyad = Ready4useDyad(),
                          condition_1L_chr = ">1",
                          uid_var_nm_1L_chr = "uid_chr"){
  all_ids_chr <- X_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(uid_var_nm_1L_chr))
  unique_ids_chr <- unique(all_ids_chr)
  counts_int <- unique_ids_chr %>% purrr::map_int(~sum(all_ids_chr == .x))
  pass_test_chr <- unique_ids_chr[purrr::map_lgl(counts_int, ~ eval(parse(text=paste0(.x, condition_1L_chr))))]
  drop_offs_chr <- setdiff(all_ids_chr %>% unique(), pass_test_chr)
  return(drop_offs_chr)
}
get_file_from_dv <- function(ds_ui_1L_chr,
                             fl_nm_1L_chr,
                             save_fmt_1L_chr,
                             repo_fl_fmt_1L_chr,
                             consent_1L_chr = "",
                             consent_indcs_int = 1L,
                             key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                             options_chr = c("Y", "N"),
                             server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), # was dataverse_chr
                             save_type_1L_chr = "original",
                             save_dir_path_1L_chr = "",
                             read_fn,
                             unlink_1L_lgl = T){
  destination_path_chr <- ifelse(unlink_1L_lgl,
                                 tempfile(),
                                 ready4::make_local_path_to_dv_data(save_dir_path_1L_chr = save_dir_path_1L_chr,
                                                                    fl_nm_1L_chr = fl_nm_1L_chr,
                                                                    save_fmt_1L_chr = save_fmt_1L_chr))
  ready4::write_dv_fl_to_loc(consent_1L_chr = ifelse(unlink_1L_lgl,options_chr[consent_indcs_int][1],consent_1L_chr),
                             consent_indcs_int = consent_indcs_int,
                             ds_ui_1L_chr = ds_ui_1L_chr,
                             fl_nm_1L_chr = fl_nm_1L_chr,
                             repo_fl_fmt_1L_chr = repo_fl_fmt_1L_chr,
                             key_1L_chr = key_1L_chr,
                             options_chr = options_chr,
                             server_1L_chr = server_1L_chr,
                             save_type_1L_chr = save_type_1L_chr,
                             dest_path_1L_chr = destination_path_chr)
  file_xxx <- rlang::exec(read_fn,destination_path_chr,stringsAsFactors = F)
  if(unlink_1L_lgl)
    unlink(destination_path_chr)
  file_xxx
  return(file_xxx)
}
get_fl_nms_of_types <- function(fl_nms_chr,
                                types_chr){
  subset_of_fl_nms_chr <- purrr::keep(fl_nms_chr,
                                      ~{
                                        fl_nm_1L_chr <- .x
                                        types_chr %>%
                                          purrr::map_lgl(~endsWith(fl_nm_1L_chr,
                                                                   .x)) %>%
                                          any()
                                      })
  return(subset_of_fl_nms_chr)
}
get_fl_meta_from_dv_ls <- function (ds_ls,
                                    fl_nm_1L_chr,
                                    nms_chr = NA_character_,
                                    type_1L_chr = "description")
{
  if (is.na(nms_chr[1])) {
    nms_chr <- purrr::map_chr(ds_ls$files$filename, ~ifelse(is.na(.x), .y, .x))
  }
  if (fl_nm_1L_chr %in% nms_chr) {
    metadata_xx <- get_from_lup_obj(ds_ls$files[, names(ds_ls$files) %>%
                                                  unique()] %>% tibble::as_tibble(),
                                    match_var_nm_1L_chr = "filename",
                                    match_value_xx = fl_nm_1L_chr,
                                    target_var_nm_1L_chr = type_1L_chr, evaluate_1L_lgl = F)
  }
  else {
    metadata_xx <- NA_character_
  }
  return(metadata_xx)
}
get_local_path_to_dv_data <- function(save_dir_path_1L_chr,
                                      fl_nm_1L_chr,
                                      save_fmt_1L_chr){
  lifecycle::deprecate_soft("0.0.0.9149", "get_local_path_to_dv_data()", "ready4::make_local_path_to_dv_data()")
  path_chr <- paste0(ifelse(save_dir_path_1L_chr!="",paste0(save_dir_path_1L_chr,"/"),""),
         fl_nm_1L_chr,
         save_fmt_1L_chr)
  return(path_chr)
}
get_r3_from_dv_csv <- function(file_name_chr,
                               data_repo_db_ui_chr,
                               data_repo_ui_chr = NA_character_,
                               r3_fn = ready4use_imports){
  tb_r3 <- tibble::tibble(file_type_chr = ".csv",
                 file_name_chr = file_name_chr,
                 data_repo_chr = NA_character_,
                 data_repo_ui_chr = data_repo_ui_chr,
                 data_repo_db_ui_chr =  data_repo_db_ui_chr,
                 data_repo_file_ext_chr = ".tab",
                 data_repo_save_type_chr = "original") %>%
    ready4use_dataverses() %>%
    procure() %>%
    make_r3_from_csv_tb(r3_fn)
  return(tb_r3)
}
get_raw_dss <- function(paths_chr,
                        names_chr,
                        read_fn = read.csv){
  assertthat::assert_that(assertthat::are_equal(length(paths_chr), length(names_chr)), msg = "names_chr must be same length as paths_chr")
  ds_ls <- purrr::map(paths_chr,
                      ~ read_fn(.x)#read.csv(eval(parse(text = .x)))
  ) %>% stats::setNames(names_chr)
  return(ds_ls)
}
get_reference_descs <- function(correspondences_ls,
                                correspondences_r3 = ready4show::ready4show_correspondences()){
  reference_descs_chr <- purrr::reduce(correspondences_ls,
                                       .init = correspondences_r3,
                                       ~ rbind(.x,.y)) %>% dplyr::pull(new_nms_chr) %>% unique()
  return(reference_descs_chr)
}
get_valid_path_chr <- function(x){
  assert_file_exists(x)
  valid_path_chr <- x
  return(valid_path_chr)
}
