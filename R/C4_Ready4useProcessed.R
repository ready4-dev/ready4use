#' Ready4useProcessed
#'
#' An S4 class to represent ready4 S4 class defining data to be saved in local directory in a processed (R) format.
#'
#' @include C4_Ready4useFiles.R
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
#' @name Ready4useProcessed-class
#' @rdname Ready4useProcessed-class
#' @export Ready4useProcessed
#' @exportClass Ready4useProcessed
Ready4useProcessed <- methods::setClass("Ready4useProcessed",
contains = "Ready4useFiles",
slots = c(write_type_1L_chr = "character",processed_fls_dir_1L_chr = "character",imports_chr = "character",path_to_seed_sf_1L_chr = "character",imports_ls = "list",merge_itms_chr = "character",raw_fls_dir_1L_chr = "character",pkg_1L_chr = "character",overwrite_1L_lgl = "logical",write_1L_lgl = "logical"),
prototype =  list(write_type_1L_chr = "proc",processed_fls_dir_1L_chr = NA_character_,imports_chr = NA_character_,path_to_seed_sf_1L_chr = NA_character_,imports_ls = list(list())))


methods::setValidity(methods::className("Ready4useProcessed"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
