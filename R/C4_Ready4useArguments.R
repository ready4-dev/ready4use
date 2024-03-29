#' Ready4useArguments
#' 
#' Arguments for a function that constructs a spatial object.
#' 
#' @include C4_Ready4useProcessed.R
#' @slot crs_nbr_dbl Coordinates reference system number (a double vector)
#' @slot write_type_1L_chr Write type (a character vector of length one)
#' @slot processed_fls_dir_1L_chr Processed files directory (a character vector of length one)
#' @slot imports_chr Imports (a character vector)
#' @slot path_to_seed_sf_1L_chr Path to seed simple features object (a character vector of length one)
#' @slot imports_ls Imports (a list)
#' @slot merge_itms_chr Merge items (a character vector)
#' @slot raw_fls_dir_1L_chr Raw files directory (a character vector of length one)
#' @slot pkg_1L_chr Package (a character vector of length one)
#' @slot overwrite_1L_lgl Overwrite (a logical vector of length one)
#' @slot write_1L_lgl Write (a logical vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name Ready4useArguments-class
#' @rdname Ready4useArguments-class
#' @export Ready4useArguments
#' @exportClass Ready4useArguments
Ready4useArguments <- methods::setClass("Ready4useArguments",
contains = "Ready4useProcessed",
slots = c(crs_nbr_dbl = "numeric",write_type_1L_chr = "character",processed_fls_dir_1L_chr = "character",imports_chr = "character",path_to_seed_sf_1L_chr = "character",imports_ls = "list",merge_itms_chr = "character",raw_fls_dir_1L_chr = "character",pkg_1L_chr = "character",overwrite_1L_lgl = "logical",write_1L_lgl = "logical",dissemination_1L_chr = "character"),
prototype =  list(crs_nbr_dbl = NA_real_))


methods::setValidity(methods::className("Ready4useArguments"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
