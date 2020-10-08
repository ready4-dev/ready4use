#' ready4_local_raw
#' @name ready4_local_raw
#' @description An S4 class to represent Readyforwhatsnext S4 class defining data to be saved in local directory in a raw (unprocessed) format.
#' @include C4_ready4_local.R
#' @slot save_type character
#' @slot merge_with_chr_vec character
methods::setClass("ready4_local_raw",
contains = "ready4_local",
slots = c(save_type = "character"),
prototype =  list(save_type = "raw"))

#' 
#' @description Create a new S4 object of the class:ready4_local_raw
#' @param merge_with_chr_vec character, Default: 'NA'
#' @param save_type character, Default: 'raw'
#' @return An S4 object of the 
#' @rdname ready4_local_raw
#' @export 
#' @importFrom methods new
ready4_local_raw <- function(merge_with_chr_vec = NA_character_,
save_type = "raw"){ 
methods::new("ready4_local_raw",
merge_with_chr_vec = merge_with_chr_vec,
save_type = save_type)
}

methods::setValidity(methods::className("ready4_local_raw",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})