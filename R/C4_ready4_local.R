#' ready4_local
#' @name ready4_local
#' @description An S4 class to represent Readyforwhatsnext S4 class defining data to be saved in local directory.
#' @slot merge_with_chr_vec character
methods::setClass("ready4_local",
slots = c(merge_with_chr_vec = "character"),
prototype =  list(merge_with_chr_vec = NA_character_))

#' 
#' @description Create a new S4 object of the class:ready4_local
#' @param merge_with_chr_vec character, Default: 'NA'
#' @return An S4 object of the 
#' @rdname ready4_local
#' @export 
#' @importFrom methods new
ready4_local <- function(merge_with_chr_vec = NA_character_){ 
methods::new("ready4_local",
merge_with_chr_vec = merge_with_chr_vec)
}

methods::setValidity(methods::className("ready4_local",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
