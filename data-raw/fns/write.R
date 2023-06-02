# write_dv_ds_fls <- function(files_tb,
#                             fl_ids_int,
#                             ds_url_1L_chr,
#                             local_dv_dir_1L_chr,
#                             key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
#                             server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
#   lifecycle::deprecate_soft("0.0.0.9149", "write_dv_ds_fls()", "ready4::write_fls_from_dv()")
#   purrr::walk(1:length(fl_ids_int),
#               ~{
#                 if(!(ds_ls$versionState=="DRAFT" | files_tb$file_type_chr[.x]==".zip")){
#                   ready4::write_dv_fl_to_loc(ds_ui_1L_chr = ds_url_1L_chr,
#                                      fl_nm_1L_chr = files_tb$file_chr[.x],
#                                      fl_id_1L_int = fl_ids_int[.x],
#                                      repo_fl_fmt_1L_chr = files_tb$ds_file_ext_chr[.x],
#                                      key_1L_chr = key_1L_chr,
#                                      server_1L_chr = server_1L_chr,
#                                      save_type_1L_chr = "original",
#                                      dest_path_1L_chr = ready4::make_local_path_to_dv_data(save_dir_path_chr = local_dv_dir_1L_chr,
#                                                                                   filename_chr = files_tb$file_chr[.x],
#                                                                                   save_format_chr = files_tb$file_type_chr[.x]))
#                 }
#               })
#
# }
# write_dv_ds <- function(ds_meta_ls,
#                         dev_pkg_nm_1L_chr = deprecated(),
#                         dss_tb,
#                         dv_nm_1L_chr,
#                         parent_dv_dir_1L_chr,
#                         paths_to_dirs_chr,
#                         inc_fl_types_chr = NA_character_,
#                         key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
#                         server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
#   if(lifecycle::is_present(dev_pkg_nm_1L_chr)) {
#     lifecycle::deprecate_warn("0.0.0.9153",
#                               "ready4use::write_dv_ds(dev_pkg_nm_1L_chr)")
#   }
#   ds_url_1L_chr <- add_ds_to_dv_repo(dv_1L_chr = dv_nm_1L_chr,
#                                      ds_meta_ls = ds_meta_ls)
#   ds_ls <- ready4::write_to_dv_with_wait(dss_tb = dss_tb,
#                               dv_nm_1L_chr = dv_nm_1L_chr,
#                               ds_url_1L_chr = ds_url_1L_chr, #
#                               parent_dv_dir_1L_chr = parent_dv_dir_1L_chr,
#                               paths_to_dirs_chr = paths_to_dirs_chr,
#                               inc_fl_types_chr = inc_fl_types_chr,
#                               key_1L_chr = key_1L_chr,
#                               server_1L_chr = server_1L_chr)
#   return(ds_ls)
# }
# write_dv_fl_to_loc <- function(ds_ui_1L_chr,
#                                fl_nm_1L_chr = NA_character_,
#                                fl_id_1L_int = NA_integer_,
#                                repo_fl_fmt_1L_chr,
#                                key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
#                                server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
#                                save_type_1L_chr = "original",
#                                dest_path_1L_chr){
#   lifecycle::deprecate_soft("0.0.0.9149", "write_dv_fl_to_loc()", "ready4::write_dv_fl_to_loc()")
#   ds_ls <- dataverse::get_dataset(ds_ui_1L_chr)
#   if(ds_ls$versionState != "DRAFT"){
#     if(!is.na(fl_id_1L_int)){
#       ds_ui_1L_chr <- NULL
#     }
#     writeBin(dataverse::get_file(ifelse(is.na(fl_id_1L_int),
#                                         paste0(fl_nm_1L_chr,repo_fl_fmt_1L_chr),
#                                         fl_id_1L_int),
#                                  dataset = ds_ui_1L_chr,
#                                  format = save_type_1L_chr,
#                                  key = key_1L_chr,
#                                  server = server_1L_chr),
#              dest_path_1L_chr)
#   }else{
#     warning("Cannot write local copy of files from private Dataverse repo")
#   }
# }
write_fls_to_dv_ds <- function(dss_tb, # Used in prior versions of AQoL longitudinal study program
                               dv_nm_1L_chr,
                               ds_url_1L_chr,
                               wait_time_in_secs_int = 5L,
                               make_local_copy_1L_lgl = F,
                               parent_dv_dir_1L_chr,
                               paths_to_dirs_chr,
                               paths_are_rltv_1L_lgl = T,
                               inc_fl_types_chr = NA_character_,
                               key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                               server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  lifecycle::deprecate_soft("0.0.0.9149", "write_fls_to_dv_ds()", "ready4::write_to_dv_with_wait()")
  ds_chr <- dss_tb$ds_obj_nm_chr
  files_tb <- make_files_tb(paths_to_dirs_chr = paths_to_dirs_chr,
                            recode_ls = dss_tb$title_chr %>% as.list() %>% stats::setNames(ds_chr),
                            inc_fl_types_chr = inc_fl_types_chr)
  if(paths_are_rltv_1L_lgl){
    data_dir_rt_1L_chr <- "."
  }else{
    data_dir_rt_1L_chr <- character(0)
  }
  fl_ids_int <- 1:nrow(files_tb) %>%
    purrr::map_int(~{
      Sys.sleep(wait_time_in_secs_int)
      ready4::write_to_dv_from_tbl(files_tb[.x,],
                      data_dir_rt_1L_chr = data_dir_rt_1L_chr,
                      ds_url_1L_chr = ds_url_1L_chr,
                      key_1L_chr = key_1L_chr,
                      server_1L_chr = server_1L_chr)
    }
    )
  ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
  if(make_local_copy_1L_lgl | ds_ls$versionState != "DRAFT"){
    ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
    dv_dir_1L_chr <- paste0(parent_dv_dir_1L_chr,"/",dv_nm_1L_chr)
    # if(!dir.exists(dv_dir_1L_chr)){
    #   dir.create(dv_dir_1L_chr)
    # }
    ready4::write_new_dirs(dv_dir_1L_chr)
    local_dv_dir_1L_chr <- paste0(dv_dir_1L_chr,"/",ds_ls$metadataBlocks$citation$fields$value[[3]])
    # if(!dir.exists(local_dv_dir_1L_chr)){
    #   dir.create(local_dv_dir_1L_chr)
    # }
    ready4::write_new_dirs(local_dv_dir_1L_chr)
    ready4::write_fls_from_dv(files_tb,
                    fl_ids_int = fl_ids_int,
                    ds_url_1L_chr = ds_url_1L_chr,
                    local_dv_dir_1L_chr = local_dv_dir_1L_chr)
  }
  return(ds_ls)
}
# write_paired_ds_fls_to_dv <- function(ds_tb,
#                                       fl_nm_1L_chr,
#                                       desc_1L_chr,
#                                       consent_1L_chr = "",
#                                       consent_indcs_int = 1L,
#                                       data_dir_rt_1L_chr = ".",
#                                       ds_url_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
#                                       options_chr = c("Y", "N"),
#                                       pkg_dv_dir_1L_chr  = "data-raw/dataverse",
#                                       key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
#                                       server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
#
#   if(!dir.exists(pkg_dv_dir_1L_chr))
#     dir.create(pkg_dv_dir_1L_chr)
#   pkg_dv_dir_1L_chr <- paste0(pkg_dv_dir_1L_chr,"/",fl_nm_1L_chr)
#   if(!dir.exists(pkg_dv_dir_1L_chr))
#     dir.create(pkg_dv_dir_1L_chr)
#   ds_tb %>%
#     saveRDS(paste0(pkg_dv_dir_1L_chr,"/",fl_nm_1L_chr,".RDS"))
#   readRDS(paste0(pkg_dv_dir_1L_chr,"/",fl_nm_1L_chr,".RDS")) %>%
#     utils::write.csv(file = paste0(pkg_dv_dir_1L_chr,"/",fl_nm_1L_chr,".csv"),
#                      row.names = F)
#   if(identical(readRDS(paste0(pkg_dv_dir_1L_chr,"/",fl_nm_1L_chr,".RDS")),
#                ready4::get_rds_from_dv(fl_nm_1L_chr))){
#     unlink(paste0(pkg_dv_dir_1L_chr,"/",fl_nm_1L_chr,".RDS"))
#   }
#   ready4::make_files_tb(paths_to_dirs_chr = pkg_dv_dir_1L_chr,
#                 recode_ls = c(rep(desc_1L_chr,2)) %>% as.list() %>% stats::setNames(c(rep(fl_nm_1L_chr,2)))) %>%
#     ready4::write_to_dv_from_tbl(data_dir_rt_1L_chr = data_dir_rt_1L_chr,
#                     ds_url_1L_chr = ds_url_1L_chr,
#                     key_1L_chr = key_1L_chr,
#                     server_1L_chr = server_1L_chr)
# }
write_to_copy_fls_to_dv_dir <- function(files_tb,
                                        local_dv_dir_1L_chr,
                                        consent_1L_chr = "",
                                        consent_indcs_int = 1L,
                                        options_chr = c("Y", "N")){
  consented_fn <- function(files_tb,
                           local_dv_dir_1L_chr){
    purrr::pwalk(files_tb,
                 ~ file.copy(paste0(..1,"/",..2,..3),
                             local_dv_dir_1L_chr))
  }
  files_chr <- purrr::map_chr(files_tb,
                              ~ paste0(..1,"/",..2,..3))
  ready4::write_with_consent(consented_fn = consented_fn,
                             prompt_1L_chr = paste0("Do you confirm that you want to copy the files ",
                                                    ready4::make_list_phrase(files_chr),
                                                    " to ",
                                                    local_dv_dir_1L_chr,
                                                    "?"),
                             consent_1L_chr = consent_1L_chr,
                             consent_indcs_int = consent_indcs_int,
                             consented_args_ls = list(files_tb = files_tb,
                                                      local_dv_dir_1L_chr = local_dv_dir_1L_chr),
                             consented_msg_1L_chr = paste0("The files ",
                                                           ready4::make_list_phrase(files_chr),
                                                           " have been copied to ",
                                                           local_dv_dir_1L_chr,
                                                           "?"),
                             declined_msg_1L_chr = "Copy request cancelled - no files or directories have been written.",
                             options_chr = options_chr)

}

