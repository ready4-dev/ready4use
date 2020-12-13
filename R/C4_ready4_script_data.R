#' ready4_script_data
#' @name ready4_script_data
#' @description An S4 class to represent ready4 S4 class containing data to be passed to a function that constructs a spatial object from a lookup table.
#' @include C4_ready4_local_proc.R
#' @slot crs_nbr_vec numeric
#' @slot save_type character
#' @slot merge_with_chr_vec character
ready4_script_data <- methods::setClass("ready4_script_data",
contains = "ready4_local_proc",
slots = c(crs_nbr_vec = "numeric",save_type = "character",merge_with_chr_vec = "character"),
prototype =  list(crs_nbr_vec = NA_real_))


methods::setValidity(methods::className("ready4_script_data"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
