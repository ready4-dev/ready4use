#' 
#' share
#' @name share-Ready4useRecord
#' @description share method applied to Ready4useRecord
#' @param x An object of class Ready4useRecord
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param type_1L_chr Type (a character vector of length one), Default: 'dataverse'
#' @param publish_dv_1L_lgl Publish dataverse (a logical vector of length one), Default: F
#' @return x (An object of class Ready4useRecord)
#' @rdname share-methods
#' @aliases share,Ready4useRecord-method
#' @export 
#' @importFrom ready4 write_env_objs_to_dv share
#' @importFrom stats setNames
methods::setMethod("share", "Ready4useRecord", function (x, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), type_1L_chr = "dataverse", 
    publish_dv_1L_lgl = F) 
{
    if (type_1L_chr == "dataverse") {
        server_1L_chr <- ifelse("server_1L_chr" %in% slotNames(x@a_Ready4usePointer@b_Ready4useRepos), 
            x@a_Ready4usePointer@b_Ready4useRepos@server_1L_chr, 
            Sys.getenv("DATAVERSE_SERVER"))
        ds_ls <- ready4::write_env_objs_to_dv(env_objects_ls = x@b_Ready4useIngest@objects_ls %>% 
            stats::setNames(x@b_Ready4useIngest@names_chr), descriptions_chr = x@b_Ready4useIngest@descriptions_chr, 
            ds_url_1L_chr = x@a_Ready4usePointer@b_Ready4useRepos@dv_ds_nm_1L_chr, 
            key_1L_chr = key_1L_chr, publish_dv_1L_lgl = publish_dv_1L_lgl, 
            server_1L_chr = server_1L_chr)
        x@dv_ds_metadata_ls <- list(ds_ls = ds_ls)
    }
    return(x)
})
#' 
#' share
#' @name share-Ready4useRecord
#' @description share method applied to Ready4useRecord
#' @param x An object of class Ready4useRecord
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param type_1L_chr Type (a character vector of length one), Default: 'dataverse'
#' @param publish_dv_1L_lgl Publish dataverse (a logical vector of length one), Default: F
#' @return x (An object of class Ready4useRecord)
#' @rdname share-methods
#' @aliases share,Ready4useRecord-method
#' @export 
#' @importFrom ready4 write_env_objs_to_dv share
#' @importFrom stats setNames
methods::setMethod("share", "Ready4useRecord", function (x, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), type_1L_chr = "dataverse", 
    publish_dv_1L_lgl = F) 
{
    if (type_1L_chr == "dataverse") {
        server_1L_chr <- ifelse("server_1L_chr" %in% slotNames(x@a_Ready4usePointer@b_Ready4useRepos), 
            x@a_Ready4usePointer@b_Ready4useRepos@server_1L_chr, 
            Sys.getenv("DATAVERSE_SERVER"))
        ds_ls <- ready4::write_env_objs_to_dv(env_objects_ls = x@b_Ready4useIngest@objects_ls %>% 
            stats::setNames(x@b_Ready4useIngest@names_chr), descriptions_chr = x@b_Ready4useIngest@descriptions_chr, 
            ds_url_1L_chr = x@a_Ready4usePointer@b_Ready4useRepos@dv_ds_nm_1L_chr, 
            key_1L_chr = key_1L_chr, publish_dv_1L_lgl = publish_dv_1L_lgl, 
            server_1L_chr = server_1L_chr)
        x@dv_ds_metadata_ls <- list(ds_ls = ds_ls)
    }
    return(x)
})
