add_ds_to_dv_repo <- function(dv_1L_chr,
                              ds_meta_ls,
                              key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                              server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  dv <- dataverse::get_dataverse(dv_1L_chr)
  dv_ls <- dataverse::dataverse_contents(dv)
  per_chr_vec <- purrr::map_chr(dv_ls,
                                ~{
                                  per_chr <-.x %>%
                                    purrr::pluck("persistentUrl")
                                  ifelse(is.null(per_chr),
                                         NA_character_,
                                         per_chr)
                                } ) %>%
    purrr::discard(is.na) %>%
    unname()
  add_ds_lgl <- T
  update_ds_lgl <- F
  if(!identical(per_chr_vec,character(0))){
    db_nm_chr_vec <- purrr::map_chr(per_chr_vec,
                                    ~{
                                      ds_ls <- dataverse::get_dataset(.x)
                                      ds_ls$metadataBlocks$citation$fields$value[[1]]
                                    })
    add_ds_lgl <- !(ds_meta_ls$title %in% db_nm_chr_vec)
  }
  if(add_ds_lgl){
    add_sword_ds <- utils::getFromNamespace("initiate_sword_dataset", "dataverse")
    add_sword_ds(dv_1L_chr,
                 body = ds_meta_ls,
                 key = key_1L_chr,
                 server = server_1L_chr)
    dv_ls <- dataverse::dataverse_contents(dv)
  }else{
    ds_ls <- dataverse::get_dataset(per_chr_vec[ds_meta_ls$title == db_nm_chr_vec])
    update_ds_lgl <- purrr::map_lgl(names(ds_meta_ls),
                                    ~{
                                      type_name_chr <-  {
                                        tmp_chr <- switch(.x,
                                                          creator = "author",
                                                          description = "dsDescription",
                                                          subject = "keyword")
                                        ifelse(is.null(tmp_chr),
                                               ifelse(.x %in% ds_ls$metadataBlocks$citation$fields$typeName,
                                                      .x,
                                                      NA_character_),
                                               tmp_chr)
                                      }
                                      new_val_chr <- ds_meta_ls %>%
                                        purrr::pluck(.x)
                                      idx_dbl <- which(type_name_chr==ds_ls$metadataBlocks$citation$fields$typeName)
                                      purrr::map_lgl(1:length(ds_ls$metadataBlocks$citation$fields$value[idx_dbl]),
                                                     ~{
                                                       if(class(ds_ls$metadataBlocks$citation$fields$value[idx_dbl][[.x]]) == "character"){
                                                         (new_val_chr != ds_ls$metadataBlocks$citation$fields$value[idx_dbl])
                                                       }else{
                                                         if(class(ds_ls$metadataBlocks$citation$fields$value[idx_dbl][[.x]]) == "data.frame")
                                                           (new_val_chr !=  ds_ls$metadataBlocks$citation$fields$value[idx_dbl][[.x]][[1]]$value)
                                                       }
                                                     }) %>% any()

                                    }) %>% any()
    if(update_ds_lgl & F) # TEMPORARILY TURNED OFF AS UPDATE NOT WORKING
      dataverse::update_dataset(dataset = ds_ls,
                                body = ds_meta_ls,
                                key = key_1L_chr,
                                server = server_1L_chr
      )
    dv_ls <- dataverse::dataverse_contents(dv)
  }
  ds_url_1L_chr <- dv_ls[[1]]$persistentUrl
  return(ds_url_1L_chr)
}
add_dv_meta_to_imp_lup <- function(imp_lup,
                                   ds_ui_1L_chr,
                                   file_type_1L_chr,
                                   save_type_1L_chr){
  assert_single_row_tb(imp_lup)
  imp_lup <- imp_lup %>%
    dplyr::mutate(data_repo_db_ui_chr = ds_ui_1L_chr,
                  data_repo_file_ext_chr = file_type_1L_chr,
                  data_repo_save_type_chr = save_type_1L_chr)
  return(imp_lup)
}
add_files_to_dv <- function (files_tb, data_dir_rt_1L_chr = ".", ds_url_1L_chr,
                             key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                             server_1L_chr = Sys.getenv("DATAVERSE_SERVER"))
{
  lifecycle::deprecate_soft("0.0.0.9149", "add_files_to_dv()", "ready4::write_to_dv_from_tbl()")
  ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
  is_draft_1L_lgl <- ds_ls$versionState == "DRAFT"
  nms_chr <- ds_ls$files$filename
  fl_ids_int <- purrr::pmap_int(files_tb, ~{
    path_1L_chr <- paste0(ifelse(identical(character(0),data_dir_rt_1L_chr),
                                 "",
                                 paste0(data_dir_rt_1L_chr, "/")),
                          ..1, "/", ..2, ..3)
    fl_nm_1L_chr <- paste0(..2, ..3)
    ready4::write_fls_to_dv(path_1L_chr,
                               descriptions_chr = ..4,
                               ds_url_1L_chr = ds_url_1L_chr,
                               ds_ls = ds_ls,
                               key_1L_chr = key_1L_chr,
                               server_1L_chr = server_1L_chr)
  })
  return(fl_ids_int)
}
add_labels_from_dictionary <- function(ds_tb,
                                       dictionary_tb,
                                       remove_old_lbls_1L_lgl = F){
  if(remove_old_lbls_1L_lgl)
    ds_tb <- ds_tb %>% ready4::remove_lbls_from_df()
  data_dictionary_tb <- dictionary_tb %>%
    dplyr::filter(var_nm_chr %in% names(ds_tb)) %>%
    dplyr::mutate(var_desc_chr = dplyr::case_when(is.na(var_desc_chr) ~ var_nm_chr,
                                                  TRUE ~ var_desc_chr)) %>%
    ready4::remove_lbls_from_df()

  if(nrow(data_dictionary_tb) > 0){
    labelled_ds_tb <- seq_len(nrow(data_dictionary_tb)) %>%
      purrr::reduce(.init = ds_tb,
                    ~ {
                      Hmisc::label(.x[[data_dictionary_tb$var_nm_chr[.y]]]) <- data_dictionary_tb$var_desc_chr[.y]
                      .x
                    }
      )
  }else{
    labelled_ds_tb <- ds_tb
  }
  return(labelled_ds_tb)
}
