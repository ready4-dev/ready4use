#' 
#' ingest
#' @name ingest-Ready4useRepos
#' @description ingest method applied to Ready4useRepos
#' @param x An object of class Ready4useRepos
#' @param idcs_int Idcs (an integer vector), Default: NA
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @return X (Ingested data, descriptive metadata and provenance details.)
#' @rdname ingest-methods
#' @aliases ingest,Ready4useRepos-method
#' @export 
#' @importFrom dataverse get_dataset
#' @importFrom purrr map_chr keep map_lgl map
#' @importFrom stringi stri_replace_all_regex
#' @importFrom ready4 get_rds_from_dv ingest
#' @importFrom stats setNames
methods::setMethod("ingest", "Ready4useRepos", function (x, idcs_int = NA_integer_, key_1L_chr = NULL) 
{
    if (identical(x@dv_ds_metadata_ls[[1]], list())) {
        ds_ls <- dataverse::get_dataset(x@dv_ds_nm_1L_chr, key = key_1L_chr, 
            server = x@server_1L_chr)
        x@dv_ds_metadata_ls <- list(ds_ls = ds_ls)
    }
    else {
        ds_ls <- x@dv_ds_metadata_ls$ds_ls
    }
    if (is.na(x@rds_objs_nms_chr)) {
        fl_nms_chr <- purrr::map_chr(ds_ls$files$filename, ~ifelse(is.na(.x), 
            .y, .x)) %>% purrr::keep(~{
            fl_nm_1L_chr <- .x
            c(".RDS", ".Rds", ".rds") %>% purrr::map_lgl(~endsWith(fl_nm_1L_chr, 
                .x)) %>% any()
        })
    }
    else {
        fl_nms_chr <- x@rds_objs_nms_chr
    }
    if (!is.na(idcs_int)) {
        fl_nms_chr <- fl_nms_chr[idcs_int]
    }
    fl_nms_chr <- fl_nms_chr %>% stringi::stri_replace_all_regex("\\.RDS", 
        "") %>% stringi::stri_replace_all_regex("\\.Rds", "") %>% 
        stringi::stri_replace_all_regex("\\.rds", "")
    if (is.na(x@dv_url_pfx_1L_chr)) {
        dv_url_pfx_1L_chr <- character(0)
    }
    else {
        dv_url_pfx_1L_chr <- x@dv_url_pfx_1L_chr
    }
    ingest_ls <- purrr::map(fl_nms_chr, ~ready4::get_rds_from_dv(file_nm_1L_chr = .x, 
        dv_ds_nm_1L_chr = x@dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
        key_1L_chr = key_1L_chr, server_1L_chr = x@server_1L_chr)) %>% 
        stats::setNames(fl_nms_chr)
    x_Ready4useIngest <- Ready4useIngest(objects_ls = ingest_ls, 
        names_chr = names(ingest_ls), descriptions_chr = fl_nms_chr %>% 
            purrr::map_chr(~get_fl_meta_from_dv_ls(ds_ls, fl_nm_1L_chr = .x)))
    x_Ready4useRecord <- Ready4useRecord(Ready4usePointer(b_Ready4useRepos = x), 
        b_Ready4useIngest = x_Ready4useIngest)
    return(x_Ready4useRecord)
})
