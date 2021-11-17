#' Ready4useFiles
#' 
#' Metadata for dataset(s) to be saved in local directory.
#' 
#' @slot merge_itms_chr Merge items (a character vector)
#' @slot raw_fls_dir_1L_chr Raw files directory (a character vector of length one)
#' @slot pkg_1L_chr Package (a character vector of length one)
#' @slot overwrite_1L_lgl Overwrite (a logical vector of length one)
#' @slot write_1L_lgl Write (a logical vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name Ready4useFiles-class
#' @rdname Ready4useFiles-class
#' @export Ready4useFiles
#' @exportClass Ready4useFiles
Ready4useFiles <- methods::setClass("Ready4useFiles",
contains = "Ready4Module",
slots = c(merge_itms_chr = "character",raw_fls_dir_1L_chr = "character",pkg_1L_chr = "character",overwrite_1L_lgl = "logical",write_1L_lgl = "logical",dissemination_1L_chr = "character"),
prototype =  list(merge_itms_chr = NA_character_,raw_fls_dir_1L_chr = NA_character_,pkg_1L_chr = NA_character_,overwrite_1L_lgl = NA,write_1L_lgl = NA))


methods::setValidity(methods::className("Ready4useFiles"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
