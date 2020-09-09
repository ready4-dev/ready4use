#' ready4_local
#' @name ready4_local
#' @description An S4 class to represent Readyforwhatsnext S4 class defining data to be saved in local directory.
#' @slot merge_with_chr_vec character
#' @slot raw_data_dir_chr character
#' @slot pckg_chr character
#' @slot overwrite_lgl logical
#' @slot save_lgl logical
methods::setClass("ready4_local",
slots = c(merge_with_chr_vec = "character",raw_data_dir_chr = "character",pckg_chr = "character",overwrite_lgl = "logical",save_lgl = "logical"),
prototype =  list(merge_with_chr_vec = NA_character_,raw_data_dir_chr = NA_character_,pckg_chr = NA_character_,overwrite_lgl = NA,save_lgl = NA))

#' 
#' @description Create a new S4 object of the class:ready4_local
#' @param merge_with_chr_vec character, Default: 'NA'
#' @param raw_data_dir_chr character, Default: 'NA'
#' @param pckg_chr character, Default: 'NA'
#' @param overwrite_lgl logical, Default: NA
#' @param save_lgl logical, Default: NA
#' @return An S4 object of the 
#' @rdname ready4_local
#' @export 
#' @importFrom methods new
ready4_local <- function(merge_with_chr_vec = NA_character_,
raw_data_dir_chr = NA_character_,
pckg_chr = NA_character_,
overwrite_lgl = NA,
save_lgl = NA){ 
methods::new("ready4_local",
merge_with_chr_vec = merge_with_chr_vec,
raw_data_dir_chr = raw_data_dir_chr,
pckg_chr = pckg_chr,
overwrite_lgl = overwrite_lgl,
save_lgl = save_lgl)
}

methods::setValidity(methods::className("ready4_local",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
