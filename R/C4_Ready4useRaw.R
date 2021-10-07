#' Ready4useRaw
#' @name Ready4useRaw
#' @description An S4 class to represent ready4 S4 class defining data to be saved in local directory in a raw (unprocessed) format.
#' @include C4_Ready4useFiles.R
#' @slot write_type_1L_chr character
#' @slot merge_itms_chr character
Ready4useRaw <- methods::setClass("Ready4useRaw",
contains = "Ready4useFiles",
slots = c(write_type_1L_chr = "character",merge_itms_chr = "character"),
prototype =  list(write_type_1L_chr = "raw"))


methods::setValidity(methods::className("Ready4useRaw"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
