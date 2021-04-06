#' @include C3_ready4_dv_import_lup.R

setOldClass(c("ready4_all_import_lup","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @description Create a new valid instance of the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import., Default: make_pt_ready4_all_import_lup()
#' @return A validated instance of the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @details ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @rdname ready4_all_import_lup
#' @export 

ready4_all_import_lup <- function(x = make_pt_ready4_all_import_lup()){ 
validate_ready4_all_import_lup(make_new_ready4_all_import_lup(x))
}
#' Make new ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @description Create a new unvalidated instance of the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @return An unvalidated instance of the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @details ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @rdname make_new_ready4_all_import_lup
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4_all_import_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_all_import_lup",setdiff(make_pt_ready4_all_import_lup() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @description Create a new prototype for the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param file_type_chr File type (a character vector), Default: character(0)
#' @param file_name_chr File name (a character vector), Default: character(0)
#' @param data_repo_chr Data repo (a character vector), Default: character(0)
#' @param data_repo_ui_chr Data repo ui (a character vector), Default: character(0)
#' @param data_repo_db_ui_chr Data repo database ui (a character vector), Default: character(0)
#' @param data_repo_file_ext_chr Data repo file ext (a character vector), Default: character(0)
#' @param data_repo_save_type_chr Data repo save type (a character vector), Default: character(0)
#' @param local_file_src_chr Local file source (a character vector), Default: character(0)
#' @param path_to_make_script_chr Path to make script (a character vector), Default: character(0)
#' @param download_url_chr Download url (a character vector), Default: character(0)
#' @param inc_file_main_chr Include file main (a character vector), Default: character(0)
#' @param inc_fls_to_rename_ls Include files to rename (a list), Default: list()
#' @param new_nms_for_inc_fls_ls New names for include files (a list), Default: list()
#' @return A prototype for ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @details ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @rdname make_pt_ready4_all_import_lup
#' @export 
#' @importFrom ready4class update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4_all_import_lup <- function(file_type_chr = character(0),
file_name_chr = character(0),
data_repo_chr = character(0),
data_repo_ui_chr = character(0),
data_repo_db_ui_chr = character(0),
data_repo_file_ext_chr = character(0),
data_repo_save_type_chr = character(0),
local_file_src_chr = character(0),
path_to_make_script_chr = character(0),
download_url_chr = character(0),
inc_file_main_chr = character(0),
inc_fls_to_rename_ls = list(),
new_nms_for_inc_fls_ls = list()){ 
args_ls <- list(file_type_chr = file_type_chr,
file_name_chr = file_name_chr,
data_repo_chr = data_repo_chr,
data_repo_ui_chr = data_repo_ui_chr,
data_repo_db_ui_chr = data_repo_db_ui_chr,
data_repo_file_ext_chr = data_repo_file_ext_chr,
data_repo_save_type_chr = data_repo_save_type_chr,
local_file_src_chr = local_file_src_chr,
path_to_make_script_chr = path_to_make_script_chr,
download_url_chr = download_url_chr,
inc_file_main_chr = inc_file_main_chr,
inc_fls_to_rename_ls = inc_fls_to_rename_ls,
new_nms_for_inc_fls_ls = new_nms_for_inc_fls_ls) %>% ready4class::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @description Validate an instance of the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param x An unvalidated instance of the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @return A prototpe for ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @details ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @rdname validate_ready4_all_import_lup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_all_import_lup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4_all_import_lup())],
names(make_pt_ready4_all_import_lup())))!=length(names(make_pt_ready4_all_import_lup()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4_all_import_lup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4_all_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4_all_import_lup())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_ready4_all_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4_all_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

x}
#' Is ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @description Check whether an object is a valid instance of the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @details ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @rdname is_ready4_all_import_lup
#' @export 

is_ready4_all_import_lup <- function(x) inherits(validate_ready4_all_import_lup(x), "ready4_all_import_lup")
