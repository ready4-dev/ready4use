#' Ready4useProcessed
#' 
#' Metadata for dataset(s) to be saved in local directory in a processed (R) format.
#' 
#' @include C4_Ready4useFiles.R
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
#' @name Ready4useProcessed-class
#' @rdname Ready4useProcessed-class
#' @export Ready4useProcessed
#' @exportClass Ready4useProcessed
Ready4useProcessed <- methods::setClass("Ready4useProcessed",
contains = "Ready4useFiles",
slots = c(write_type_1L_chr = "character",processed_fls_dir_1L_chr = "character",imports_chr = "character",path_to_seed_sf_1L_chr = "character",imports_ls = "list",merge_itms_chr = "character",raw_fls_dir_1L_chr = "character",pkg_1L_chr = "character",overwrite_1L_lgl = "logical",write_1L_lgl = "logical",dissemination_1L_chr = "character"),
prototype =  list(write_type_1L_chr = NA_character_,processed_fls_dir_1L_chr = NA_character_,imports_chr = NA_character_,path_to_seed_sf_1L_chr = NA_character_,imports_ls = list(list())))


methods::setValidity(methods::className("Ready4useProcessed"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
