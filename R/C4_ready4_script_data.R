#' ready4_script_data
#' @name ready4_script_data
#' @description An S4 class to represent Data to be passed to a function that constructs a spatial object from a lookup table.
#' @include C4_ready4_local_proc.R
#' @slot crs_nbr_vec numeric
#' @slot save_type character
#' @slot proc_data_dir_chr character
#' @slot import_chr_vec character
#' @slot path_to_starter_sf_chr character
#' @slot import_this_ls list
#' @slot merge_with_chr_vec character
#' @slot raw_data_dir_chr character
#' @slot pckg_chr character
#' @slot overwrite_lgl logical
#' @slot save_lgl logical
methods::setClass("ready4_script_data",
contains = "ready4_local_proc",
slots = c(crs_nbr_vec = "numeric"),
prototype =  list(crs_nbr_vec = NA_real_))

#' 
#' @description Create a new S4 object of the class:ready4_script_data
#' @param save_type character, Default: 'NA'
#' @param proc_data_dir_chr character, Default: 'NA'
#' @param import_chr_vec character, Default: 'NA'
#' @param path_to_starter_sf_chr character, Default: 'NA'
#' @param import_this_ls list, Default: list(list())
#' @param merge_with_chr_vec character, Default: 'NA'
#' @param raw_data_dir_chr character, Default: 'NA'
#' @param pckg_chr character, Default: 'NA'
#' @param overwrite_lgl logical, Default: NA
#' @param save_lgl logical, Default: NA
#' @param crs_nbr_vec numeric, Default: NA
#' @return An S4 object of the ready4_script_data class
#' @rdname ready4_script_data
#' @export 
#' @importFrom methods new
ready4_script_data <- function(save_type = NA_character_,
proc_data_dir_chr = NA_character_,
import_chr_vec = NA_character_,
path_to_starter_sf_chr = NA_character_,
import_this_ls = list(list()),
merge_with_chr_vec = NA_character_,
raw_data_dir_chr = NA_character_,
pckg_chr = NA_character_,
overwrite_lgl = NA,
save_lgl = NA,
crs_nbr_vec = NA_real_){ 
methods::new("ready4_script_data",
save_type = save_type,
proc_data_dir_chr = proc_data_dir_chr,
import_chr_vec = import_chr_vec,
path_to_starter_sf_chr = path_to_starter_sf_chr,
import_this_ls = import_this_ls,
merge_with_chr_vec = merge_with_chr_vec,
raw_data_dir_chr = raw_data_dir_chr,
pckg_chr = pckg_chr,
overwrite_lgl = overwrite_lgl,
save_lgl = save_lgl,
crs_nbr_vec = crs_nbr_vec)
}

methods::setValidity(methods::className("ready4_script_data",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
