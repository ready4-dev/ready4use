#' Ready4useArguments
#' @name Ready4useArguments
#' @description An S4 class to represent ready4 S4 class containing data to be passed to a function that constructs a spatial object from a lookup table.
#' @include C4_Ready4useProcessed.R
#' @slot crs_nbr_dbl numeric
#' @slot write_type_1L_chr character
#' @slot merge_itms_chr character
#' @export Ready4useArguments
#' @exportClass Ready4useArguments
Ready4useArguments <- methods::setClass("Ready4useArguments",
contains = "Ready4useProcessed",
slots = c(crs_nbr_dbl = "numeric",write_type_1L_chr = "character",merge_itms_chr = "character"),
prototype =  list(crs_nbr_dbl = NA_real_))


methods::setValidity(methods::className("Ready4useArguments"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
