ingest_Ready4useRepos <- function(x,
                                  #idcs_int = NA_integer_,
                                  fls_to_ingest_chr = NA_character_,
                                  key_1L_chr = NULL,
                                  type_1L_chr = "R"){
  ingest_ls <- NULL
  descriptions_chr <- character(0)
  if(!is.na(x@dv_ds_nm_1L_chr)){
    if(identical(x@dv_ds_metadata_ls[[1]],list())){
      ds_ls <- dataverse::get_dataset(x@dv_ds_nm_1L_chr,
                                      key = key_1L_chr,
                                      server = x@dv_server_1L_chr)
      x@dv_ds_metadata_ls <- list(ds_ls = ds_ls)
    }else{
      ds_ls <- x@dv_ds_metadata_ls$ds_ls
    }
    if(type_1L_chr == "R"){
      if(is.na(x@fl_nms_chr)){
        fl_nms_chr <- ds_ls$files$filename %>%
          get_fl_nms_of_types(types_chr = c(".RDS",".Rds",".rds"))
      }else{
        fl_nms_chr <- x@fl_nms_chr
      }
      fl_nms_chr <- fl_nms_chr %>%
        stringi::stri_replace_all_regex("\\.RDS","") %>%
        stringi::stri_replace_all_regex("\\.Rds","") %>%
        stringi::stri_replace_all_regex("\\.rds","")
      if(!is.na(fls_to_ingest_chr))
        fl_nms_chr <- intersect(fl_nms_chr,fls_to_ingest_chr)
      # if(!is.na(idcs_int)){
      #   fl_nms_chr <- fl_nms_chr[idcs_int]
      # }
      if(is.na(x@dv_url_pfx_1L_chr)){
        dv_url_pfx_1L_chr <- character(0)
      }else{
        dv_url_pfx_1L_chr <- x@dv_url_pfx_1L_chr
      }
      if(!identical(fl_nms_chr,character(0)))
        ingest_ls <- purrr::map(fl_nms_chr,
                                ~ready4::get_rds_from_dv(file_nm_1L_chr = .x,
                                                         dv_ds_nm_1L_chr = x@dv_ds_nm_1L_chr,
                                                         dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                                         key_1L_chr = key_1L_chr,
                                                         server_1L_chr = x@dv_server_1L_chr)) %>%
        stats::setNames(fl_nms_chr) %>%
        append(ingest_ls)
    }
    descriptions_chr <- fl_nms_chr %>%
      purrr::map_chr(~ get_fl_meta_from_dv_ls(ds_ls,
                                              fl_nm_1L_chr = .x))
  }
  if(!is.na(x@gh_repo_1L_chr)){ # ASSUMES NO GH RELEASE FL NMS ARE SHARED WITH DATAVERSE DS FILES
    dmt_urls_chr <- piggyback::pb_download_url(repo = x@gh_repo_1L_chr,
                                               tag = x@gh_tag_1L_chr)
    if(type_1L_chr == "R"){
    dmt_urls_chr <- dmt_urls_chr %>%
      get_fl_nms_of_types(types_chr = c(".RDS",".Rds",".rds"))
    fl_nms_chr <- dmt_urls_chr %>%
      fs::path_file()
    fl_nms_chr <- fl_nms_chr %>%
      stringi::stri_replace_all_regex("\\.RDS","") %>%
      stringi::stri_replace_all_regex("\\.Rds","") %>%
      stringi::stri_replace_all_regex("\\.rds","")
    if(!is.na(fls_to_ingest_chr)){
      selected_chr <- intersect(fl_nms_chr,fls_to_ingest_chr)
      idcs_int <- which(fl_nms_chr %in% selected_chr)
    }else{
      idcs_int <- 1:length(fl_nms_chr)
    }
    fl_nms_chr <- fl_nms_chr[idcs_int]
    if(!identical(fl_nms_chr,character(0)))
      ingest_ls <- purrr::map(dmt_urls_chr[idcs_int],
                              ~ readRDS(url(.x))) %>%
      stats::setNames(fl_nms_chr) %>%
      append(ingest_ls)
    descriptions_chr <- c(descriptions_chr,
                          rep(NA_character_, length(fl_nms_chr)))
    }
    }
    x_Ready4useIngest <- Ready4useIngest(objects_ls = ingest_ls,
                                         descriptions_chr = descriptions_chr)
    x_Ready4useRecord <- Ready4useRecord(a_Ready4usePointer = Ready4usePointer(b_Ready4useRepos = x),
                                         b_Ready4useIngest = x_Ready4useIngest)
  return(x_Ready4useRecord)
}
