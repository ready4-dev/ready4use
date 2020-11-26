#' ready4_local
#' @name ready4_local
#' @description An S4 class to represent Readyforwhatsnext S4 class defining data to be saved in local directory.
#' @slot merge_with_chr_vec character
#' @exportClass ready4_local
ready4_local <- methods::setClass("ready4_local",
slots = c(merge_with_chr_vec = "character"),
prototype =  list(merge_with_chr_vec = NA_character_))

