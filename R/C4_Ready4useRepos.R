#' Ready4useRepos
#' 
#' Metadata about online data repositories.
#' 
#' @slot dataverse_nm_1L_chr Dataverse name (a character vector of length one)
#' @slot dv_ds_metadata_ls Dataverse dataset metadata (a list)
#' @slot dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one)
#' @slot dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one)
#' @slot rds_objs_nms_chr Rds objects names (a character vector)
#' @slot server_1L_chr Server (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name Ready4useRepos-class
#' @rdname Ready4useRepos-class
#' @export Ready4useRepos
#' @exportClass Ready4useRepos
Ready4useRepos <- methods::setClass("Ready4useRepos",
contains = "Ready4Module",
slots = c(dataverse_nm_1L_chr = "character",dv_ds_metadata_ls = "list",dv_ds_nm_1L_chr = "character",dv_url_pfx_1L_chr = "character",rds_objs_nms_chr = "character",server_1L_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(dataverse_nm_1L_chr = NA_character_,dv_ds_metadata_ls = list(list()),dv_ds_nm_1L_chr = NA_character_,dv_url_pfx_1L_chr = NA_character_,rds_objs_nms_chr = NA_character_,server_1L_chr = NA_character_))


methods::setValidity(methods::className("Ready4useRepos"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
