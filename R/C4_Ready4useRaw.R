#' Ready4useRaw
#' @name Ready4useRaw
#' @description An S4 class to represent ready4 S4 class defining data to be saved in local directory in a raw (unprocessed) format.
#' @include C4_Ready4useFiles.R
#' @slot write_type_1L_chr character
#' @slot merge_itms_chr character
#' @slot raw_fls_dir_1L_chr character
#' @slot pkg_1L_chr character
#' @slot overwrite_1L_lgl logical
#' @slot write_1L_lgl logical
#' @exportClass Ready4useRaw
#' @export
Ready4useRaw <- methods::setClass("Ready4useRaw",
contains = "Ready4useFiles",
slots = c(write_type_1L_chr = "character",merge_itms_chr = "character",raw_fls_dir_1L_chr = "character",pkg_1L_chr = "character",overwrite_1L_lgl = "logical",write_1L_lgl = "logical"),
prototype =  list(write_type_1L_chr = "raw"))


methods::setValidity(methods::className("Ready4useRaw"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
