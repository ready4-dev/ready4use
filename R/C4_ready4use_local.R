#' ready4use_local
#' @name ready4use_local
#' @description An S4 class to represent ready4 S4 class defining data to be saved in local directory.
#' @slot merge_with_chr_vec character
ready4use_local <- methods::setClass("ready4use_local",
slots = c(merge_with_chr_vec = "character"),
prototype =  list(merge_with_chr_vec = NA_character_))


methods::setValidity(methods::className("ready4use_local"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
