#' ready4_local_raw
#' @name ready4_local_raw
#' @description An S4 class to represent Readyforwhatsnext S4 class defining data to be saved in local directory in a raw (unprocessed) format.
#' @include C4_ready4_local.R
#' @slot save_type character
#' @slot merge_with_chr_vec character
#' @exportClass ready4_local_raw
ready4_local_raw <- methods::setClass("ready4_local_raw",
contains = "ready4_local",
slots = c(save_type = "character"),
prototype =  list(save_type = "raw"))


methods::setValidity(methods::className("ready4_local_raw",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
