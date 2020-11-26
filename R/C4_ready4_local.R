#' ready4_local
#' @name ready4_local
#' @description An S4 class to represent Readyforwhatsnext S4 class defining data to be saved in local directory.
#' @slot merge_with_chr_vec character
ready4_local <- methods::setClass("ready4_local",
slots = c(merge_with_chr_vec = "character"),
prototype =  list(merge_with_chr_vec = NA_character_))


methods::setValidity(methods::className("ready4_local",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
