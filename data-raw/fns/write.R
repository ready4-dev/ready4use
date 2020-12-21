write_dv_ds_fls <- function(files_tb,
                            fl_ids_int,
                            ds_url_1L_chr,
                            local_dv_dir_1L_chr,
                            key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                            server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  purrr::walk(1:length(fl_ids_int),
              ~{
                if(!(ds_ls$versionState=="DRAFT" | files_tb$file_type_chr[.x]==".zip")){
                  write_dv_fl_to_loc(ds_ui_1L_chr = ds_url_1L_chr,
                                     fl_nm_1L_chr = files_tb$file_chr[.x],
                                     fl_id_1L_int = fl_ids_int[.x],
                                     repo_fl_fmt_1L_chr = files_tb$ds_file_ext_chr[.x],
                                     key_1L_chr = key_1L_chr,
                                     server_1L_chr = server_1L_chr,
                                     save_type_1L_chr = "original",
                                     dest_path_1L_chr = get_local_path_to_dv_data(save_dir_path_chr = local_dv_dir_1L_chr,
                                                                                  filename_chr = files_tb$file_chr[.x],
                                                                                  save_format_chr = files_tb$file_type_chr[.x]))
                }
              })

}
write_dv_ds <- function(ds_meta_ls,
                        dev_pkg_nm_1L_chr = ready4fun::get_dev_pkg_nm(),
                        dss_tb,
                        dv_nm_1L_chr,
                        parent_dv_dir_1L_chr,
                        paths_to_dirs_chr,
                        inc_fl_types_chr = NA_character_,
                        key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                        server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  ds_url_1L_chr <- add_ds_to_dv_repo(dv_1L_chr = dv_nm_1L_chr,
                                     ds_meta_ls = ds_meta_ls)
  ds_ls <- write_fls_to_dv_ds(dss_tb = dss_tb,
                              dv_nm_1L_chr = dv_nm_1L_chr,
                              ds_url_1L_chr = ds_url_1L_chr, #
                              parent_dv_dir_1L_chr = parent_dv_dir_1L_chr,
                              paths_to_dirs_chr = paths_to_dirs_chr,
                              inc_fl_types_chr = inc_fl_types_chr,
                              key_1L_chr = key_1L_chr,
                              server_1L_chr = server_1L_chr)
  return(ds_ls)
}
write_dv_fl_to_loc <- function(ds_ui_1L_chr,
                               fl_nm_1L_chr = NA_character_,
                               fl_id_1L_int = NA_integer_,
                               repo_fl_fmt_1L_chr,
                               key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                               server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                               save_type_1L_chr = "original",
                               dest_path_1L_chr){
  ds_ls <- dataverse::get_dataset(ds_ui_1L_chr)
  if(ds_ls$versionState != "DRAFT"){
    if(!is.na(fl_id_1L_int)){
      ds_ui_1L_chr <- NULL
    }
    writeBin(dataverse::get_file(ifelse(is.na(fl_id_1L_int),
                                        paste0(fl_nm_1L_chr,repo_fl_fmt_1L_chr),
                                        fl_id_1L_int),
                                 dataset = ds_ui_1L_chr,
                                 format = save_type_1L_chr,
                                 key = key_1L_chr,
                                 server = server_1L_chr),
             dest_path_1L_chr)
  }else{
    warning("Cannot write local copy of files from private Dataverse repo")
  }
}
write_fls_to_dv_ds <- function(dss_tb,
                               dv_nm_1L_chr,
                               ds_url_1L_chr,
                               wait_time_in_secs_int = 5L,
                               make_local_copy_1L_lgl = F,
                               #dev_pkg_nm_1L_chr = ready4fun::get_dev_pkg_nm(),
                               parent_dv_dir_1L_chr,
                               paths_to_dirs_chr,
                               inc_fl_types_chr = NA_character_,
                               key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                               server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
  dv_dir_1L_chr <- paste0(parent_dv_dir_1L_chr,"/",dv_nm_1L_chr)
  if(!dir.exists(dv_dir_1L_chr)){
    dir.create(dv_dir_1L_chr)
  }
  local_dv_dir_1L_chr <- paste0(dv_dir_1L_chr,"/",ds_ls$metadataBlocks$citation$fields$value[[1]])
  if(!dir.exists(local_dv_dir_1L_chr)){
    dir.create(local_dv_dir_1L_chr)
  }
  ds_chr <- dss_tb$ds_obj_nm_chr
  files_tb <- make_files_tb(paths_to_dirs_chr = paths_to_dirs_chr,
                            recode_ls = dss_tb$title_chr %>% as.list() %>% stats::setNames(ds_chr),
                            inc_fl_types_chr = inc_fl_types_chr)
  fl_ids_int <- 1:nrow(files_tb) %>%
    purrr::map_int(~{
      Sys.sleep(wait_time_in_secs_int)
      add_files_to_dv(files_tb[.x,],
                      ds_url_1L_chr = ds_url_1L_chr,
                      key_1L_chr = key_1L_chr,
                      server_1L_chr = server_1L_chr)
    }
    )
  ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
  if(make_local_copy_1L_lgl | ds_ls$versionState != "DRAFT"){
    write_dv_ds_fls(files_tb,
                    fl_ids_int = fl_ids_int,
                    ds_url_1L_chr = ds_url_1L_chr,
                    local_dv_dir_1L_chr = local_dv_dir_1L_chr)
  }
  return(ds_ls)
}
write_pkg_dss_to_dv_ds_csvs <- function(pkg_dss_tb,
                                        dv_nm_1L_chr,
                                        ds_url_1L_chr,
                                        wait_time_in_secs_int = 5L,
                                        dev_pkg_nm_1L_chr = ready4fun::get_dev_pkg_nm(),
                                        parent_dv_dir_1L_chr = "../../../../Data/Dataverse",
                                        key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                                        server_1L_chr = Sys.getenv("DATAVERSE_SERVER")#,
                                        #subject_1L_chr = "Mental health, health economics, data synthesis, simulation."
                                        ){
  # ds_meta_ls <- list(title = paste0(dev_pkg_nm_1L_chr," Code Library Metadata"),
  #                    creator = suppressWarnings(parse(text=(packageDescription(dev_pkg_nm_1L_chr,
  #                                                                              fields = "Authors@R"))) %>%
  #                                                 eval() %>%
  #                                                 purrr::keep(~stringr::str_detect(.x,"aut, cre")) %>%
  #                                                 stringr::str_sub(end = stringi::stri_locate_first_fixed(.,"<")[1]-2)),
  #                    description = paste0("Metadata relating to the abbreviations, classes, datasets, functions, generics and methods used in the ",dev_pkg_nm_1L_chr," code library."),
  #                    subject = subject_1L_chr)
  ds_chr <- pkg_dss_tb$ds_obj_nm_chr
  purrr::walk(ds_chr,~ {
    utils::data(list=.x, package = dev_pkg_nm_1L_chr, envir = environment())
    eval(parse(text = .x)) %>%
      dplyr::mutate_if(is.list,
                       list(~ifelse(stringr::str_c(.)=="NULL",NA_character_ , stringr::str_c (.)))) %>%
      write.csv(file = paste0("data-raw/",.x,".csv"),
                row.names = F)
  })
  ds_ls <- write_fls_to_dv_ds(dss_tb = pkg_dss_tb,
                              dv_nm_1L_chr  = dv_nm_1L_chr,
                              ds_url_1L_chr = ds_url_1L_chr,
                              wait_time_in_secs_int = wait_time_in_secs_int,
                              #dev_pkg_nm_1L_chr = dev_pkg_nm_1L_chr,
                              parent_dv_dir_1L_chr  = parent_dv_dir_1L_chr,
                              paths_to_dirs_chr  = c("data-raw"),
                              inc_fl_types_chr = ".csv",
                              key_1L_chr = key_1L_chr,
                              server_1L_chr = server_1L_chr)
  return(ds_ls)
}
write_to_copy_fls_to_dv_dir <- function(files_tb,
                                        local_dv_dir_1L_chr){
  purrr::pwalk(files_tb,
               ~ file.copy(paste0(..1,"/",..2,..3),
                           local_dv_dir_1L_chr))
}
write_to_add_urls_to_dss <- function(ds_url, # NOTE WORKING - NEEEDS WORK
                                     pkg_dss_tb,
                                     pkg_nm_1L_chr = ready4fun::get_dev_pkg_nm()){
  ds_fls_ls <- dataverse::dataset_files(ds_url)
  fl_ids_chr <- purrr::map_chr(1:length(ds_fls_ls), ~ ds_fls_ls[[.x]][["dataFile"]][["pidURL"]])
  fl_nms_chr <- purrr::map_chr(1:length(ds_fls_ls), ~ ds_fls_ls[[.x]][["dataFile"]][["originalFileName"]] %>% stringr::str_remove(".csv") )
  url_lup <- purrr::map_dfr(1:length(ds_fls_ls), ~ tibble::tibble(ds_obj_nm_chr = ds_fls_ls[[.x]][["dataFile"]][["originalFileName"]] %>% stringr::str_remove(".csv"),
                                                                  url_chr = ds_fls_ls[[.x]][["dataFile"]][["pidURL"]]))
  pkg_dss_tb <- dplyr::inner_join(pkg_dss_tb %>% dplyr::select(-url_chr),url_lup)
  purrr::walk(pkg_dss_tb,
              ~{
                utils::data(list=..1,
                     package = pkg_nm_1L_chr,
                     envir = environment())
                eval(parse(text = paste0("ds<-", ..1)))
                ds %>%
                  ready4fun::write_and_doc_ds(db_1L_chr = ..1,
                                              title_1L_chr = ..2,
                                              desc_1L_chr = ..3,
                                              url_1L_chr = ..4,
                                              pkg_dss_tb = pkg_dss_tb)
              })
  return(pkg_dss_tb)
}
