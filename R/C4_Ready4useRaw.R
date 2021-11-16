#' Ready4useRaw
#' 
#' Metadata for dataset(s) to be saved in local directory in a raw (unprocessed) format.
#' 
#' @include C4_Ready4useFiles.R
#' @slot write_type_1L_chr Write type (a character vector of length one)
#' @slot merge_itms_chr Merge items (a character vector)
#' @slot raw_fls_dir_1L_chr Raw files directory (a character vector of length one)
#' @slot pkg_1L_chr Package (a character vector of length one)
#' @slot overwrite_1L_lgl Overwrite (a logical vector of length one)
#' @slot write_1L_lgl Write (a logical vector of length one)
#' @name Ready4useRaw-class
#' @rdname Ready4useRaw-class
#' @export Ready4useRaw
#' @exportClass Ready4useRaw
Ready4useRaw <- methods::setClass("Ready4useRaw",
contains = "Ready4useFiles",
slots = c(write_type_1L_chr = "character",merge_itms_chr = "character",raw_fls_dir_1L_chr = "character",pkg_1L_chr = "character",overwrite_1L_lgl = "logical",write_1L_lgl = "logical"),
prototype =  list(write_type_1L_chr = "raw"))


methods::setValidity(methods::className("Ready4useRaw"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
