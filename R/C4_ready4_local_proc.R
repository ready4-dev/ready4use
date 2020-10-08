#' ready4_local_proc
#' @name ready4_local_proc
#' @description An S4 class to represent Readyforwhatsnext S4 class defining data to be saved in local directory in a processed (R) format.
#' @include C4_ready4_local.R
#' @slot save_type character
#' @slot merge_with_chr_vec character
methods::setClass("ready4_local_proc",
contains = "ready4_local",
slots = c(save_type = "character"),
prototype =  list(save_type = "proc"))

#' 
#' @description Create a new S4 object of the class:ready4_local_proc
#' @param merge_with_chr_vec character, Default: 'NA'
#' @param save_type character, Default: 'proc'
#' @return An S4 object of the 
#' @rdname ready4_local_proc
#' @export 
#' @importFrom methods new
ready4_local_proc <- function(merge_with_chr_vec = NA_character_,
save_type = "proc"){ 
methods::new("ready4_local_proc",
merge_with_chr_vec = merge_with_chr_vec,
save_type = save_type)
}

methods::setValidity(methods::className("ready4_local_proc",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
