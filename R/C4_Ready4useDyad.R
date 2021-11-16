#' Ready4useDyad
#' 
#' A dataset and data dictionary pair.
#' 
setOldClass(c("ready4use_dictionary","tbl_df", "tbl", "data.frame"))
#' @slot ds_tb Dataset (a tibble)
#' @slot dictionary_r3 Dictionary (a ready4 S3)
#' @import ready4
#' @name Ready4useDyad-class
#' @rdname Ready4useDyad-class
#' @export Ready4useDyad
#' @exportClass Ready4useDyad
Ready4useDyad <- methods::setClass("Ready4useDyad",
contains = "Ready4Launch",
slots = c(ds_tb = "tbl_df",dictionary_r3 = "ready4use_dictionary"),
prototype =  list(ds_tb = tibble::tibble(),dictionary_r3 = ready4use_dictionary()))


methods::setValidity(methods::className("Ready4useDyad"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
