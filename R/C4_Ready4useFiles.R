#' Ready4useFiles
#' @name Ready4useFiles
#' @description An S4 class to represent ready4 S4 class defining data to be saved in local directory.
#' @slot merge_itms_chr character
#' @slot raw_fls_dir_1L_chr character
#' @slot pkg_1L_chr character
#' @slot overwrite_1L_lgl logical
#' @slot write_1L_lgl logical
#' @export Ready4useFiles
#' @exportClass Ready4useFiles
Ready4useFiles <- methods::setClass("Ready4useFiles",
slots = c(merge_itms_chr = "character",raw_fls_dir_1L_chr = "character",pkg_1L_chr = "character",overwrite_1L_lgl = "logical",write_1L_lgl = "logical"),
prototype =  list(merge_itms_chr = NA_character_,raw_fls_dir_1L_chr = NA_character_,pkg_1L_chr = NA_character_,overwrite_1L_lgl = NA,write_1L_lgl = NA))


methods::setValidity(methods::className("Ready4useFiles"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
