#' ready4use_local_proc
#' @name ready4use_local_proc
#' @description An S4 class to represent ready4 S4 class defining data to be saved in local directory in a processed (R) format.
#' @include C4_ready4use_local.R
#' @slot save_type character
#' @slot merge_with_chr_vec character
ready4use_local_proc <- methods::setClass("ready4use_local_proc",
contains = "ready4use_local",
slots = c(save_type = "character",merge_with_chr_vec = "character"),
prototype =  list(save_type = "proc"))


methods::setValidity(methods::className("ready4use_local_proc"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
