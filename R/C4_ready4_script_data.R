#' ready4_script_data
#' @name ready4_script_data
#' @description An S4 class to represent Readyforwhatsnext S4 class containint data to be passed to a function that constructs a spatial object from a lookup table.
#' @include C4_ready4_local_proc.R
#' @slot crs_nbr_vec numeric
#' @slot save_type character
#' @slot merge_with_chr_vec character
methods::setClass("ready4_script_data",
contains = "ready4_local_proc",
slots = c(crs_nbr_vec = "numeric"),
prototype =  list(crs_nbr_vec = NA_real_))

#' 
#' @description Create a new S4 object of the class:ready4_script_data
#' @param save_type character, Default: 'NA'
#' @param merge_with_chr_vec character, Default: 'NA'
#' @param crs_nbr_vec numeric, Default: NA
#' @return An S4 object of the 
#' @rdname ready4_script_data
#' @export 
#' @importFrom methods new
ready4_script_data <- function(save_type = NA_character_,
merge_with_chr_vec = NA_character_,
crs_nbr_vec = NA_real_){ 
methods::new("ready4_script_data",
save_type = save_type,
merge_with_chr_vec = merge_with_chr_vec,
crs_nbr_vec = crs_nbr_vec)
}

methods::setValidity(methods::className("ready4_script_data",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
