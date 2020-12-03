
setOldClass(c("ready4_dv_import_lup","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @description Create a new valid instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse., Default: make_pt_ready4_dv_import_lup()
#' @return A validated instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @details ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @rdname ready4_dv_import_lup
#' @export 

ready4_dv_import_lup <- function(x = make_pt_ready4_dv_import_lup()){ 
validate_ready4_dv_import_lup(make_new_ready4_dv_import_lup(x))
}
#' Make new ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @description Create a new unvalidated instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @return An unvalidated instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @details ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @rdname make_new_ready4_dv_import_lup
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4_dv_import_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_dv_import_lup",setdiff(make_pt_ready4_dv_import_lup() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @description Create a new prototype for the 
#' @param file_type PARAM_DESCRIPTION, Default: character(0)
#' @param file_name PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo_ui PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo_db_ui PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo_file_ext PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo_save_type PARAM_DESCRIPTION, Default: character(0)
#' @return A prototype for 
#' @details ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @rdname make_pt_ready4_dv_import_lup
#' @export 
#' @importFrom purrr map_dbl
#' @importFrom tibble tibble
make_pt_ready4_dv_import_lup <- function(file_type = character(0),
file_name = character(0),
data_repo = character(0),
data_repo_ui = character(0),
data_repo_db_ui = character(0),
data_repo_file_ext = character(0),
data_repo_save_type = character(0)){ 
arg_lgths_dbl <- list(file_type,
file_name,
data_repo,
data_repo_ui,
data_repo_db_ui,
data_repo_file_ext,
data_repo_save_type) %>% purrr::map_dbl(~length(.x))
arg_max_lgth_1L_dbl <- max(arg_lgths_dbl)
if(arg_max_lgth_1L_dbl >0){
if(0 == arg_lgths_dbl[1] & arg_lgths_dbl[1] != arg_max_lgth_1L_dbl){
file_type <- NA_character_
}

if(0 == arg_lgths_dbl[2] & arg_lgths_dbl[2] != arg_max_lgth_1L_dbl){
file_name <- NA_character_
}

if(0 == arg_lgths_dbl[3] & arg_lgths_dbl[3] != arg_max_lgth_1L_dbl){
data_repo <- NA_character_
}

if(0 == arg_lgths_dbl[4] & arg_lgths_dbl[4] != arg_max_lgth_1L_dbl){
data_repo_ui <- NA_character_
}

if(0 == arg_lgths_dbl[5] & arg_lgths_dbl[5] != arg_max_lgth_1L_dbl){
data_repo_db_ui <- NA_character_
}

if(0 == arg_lgths_dbl[6] & arg_lgths_dbl[6] != arg_max_lgth_1L_dbl){
data_repo_file_ext <- NA_character_
}

if(0 == arg_lgths_dbl[7] & arg_lgths_dbl[7] != arg_max_lgth_1L_dbl){
data_repo_save_type <- NA_character_
}
}
tibble::tibble(file_type = file_type,
file_name = file_name,
data_repo = data_repo,
data_repo_ui = data_repo_ui,
data_repo_db_ui = data_repo_db_ui,
data_repo_file_ext = data_repo_file_ext,
data_repo_save_type = data_repo_save_type)
}
#' Validate ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @description Validate an instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param x An unvalidated instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @return A prototpe for ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @details ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @rdname validate_ready4_dv_import_lup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_dv_import_lup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4_dv_import_lup())],
names(make_pt_ready4_dv_import_lup())))!=length(names(make_pt_ready4_dv_import_lup()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4_dv_import_lup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_pt_ready4_dv_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4_dv_import_lup())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_ready4_dv_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4_dv_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' Is ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @description Check whether an object is a valid instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @details ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @rdname is_ready4_dv_import_lup
#' @export 

is_ready4_dv_import_lup <- function(x) inherits(validate_ready4_dv_import_lup(x), "ready4_dv_import_lup")
