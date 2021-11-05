#' Ready4useArguments
#' @name Ready4useArguments
#' @description An S4 class to represent ready4 S4 class containing data to be passed to a function that constructs a spatial object from a lookup table.
#' @include C4_Ready4useProcessed.R
#' @slot crs_nbr_dbl numeric
#' @slot write_type_1L_chr character
#' @slot processed_fls_dir_1L_chr character
#' @slot imports_chr character
#' @slot path_to_seed_sf_1L_chr character
#' @slot imports_ls list
#' @slot merge_itms_chr character
#' @slot raw_fls_dir_1L_chr character
#' @slot pkg_1L_chr character
#' @slot overwrite_1L_lgl logical
#' @slot write_1L_lgl logical
#' @export Ready4useArguments
#' @exportClass Ready4useArguments
Ready4useArguments <- methods::setClass("Ready4useArguments",
contains = "Ready4useProcessed",
slots = c(crs_nbr_dbl = "numeric",write_type_1L_chr = "character",processed_fls_dir_1L_chr = "character",imports_chr = "character",path_to_seed_sf_1L_chr = "character",imports_ls = "list",merge_itms_chr = "character",raw_fls_dir_1L_chr = "character",pkg_1L_chr = "character",overwrite_1L_lgl = "logical",write_1L_lgl = "logical"),
prototype =  list(crs_nbr_dbl = NA_real_))


methods::setValidity(methods::className("Ready4useArguments"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
