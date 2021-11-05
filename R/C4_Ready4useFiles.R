#' Ready4useFiles
#' @name Ready4useFiles
#' @description An S4 class to represent ready4 S4 class defining data to be saved in local directory.
#' @slot merge_itms_chr character
#' @export Ready4useFiles
#' @exportClass Ready4useFiles
Ready4useFiles <- methods::setClass("Ready4useFiles",
slots = c(merge_itms_chr = "character"),
prototype =  list(merge_itms_chr = NA_character_))


methods::setValidity(methods::className("Ready4useFiles"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
