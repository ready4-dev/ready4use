
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
#' @description Create a new prototype for the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param file_type PARAM_DESCRIPTION, Default: character(0)
#' @param file_name PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo_ui PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo_db_ui PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo_file_ext PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo_save_type PARAM_DESCRIPTION, Default: character(0)
#' @return A prototype for ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @details ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @rdname make_pt_ready4_dv_import_lup
#' @export 
#' @importFrom ready4class update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4_dv_import_lup <- function(file_type = character(0),
file_name = character(0),
data_repo = character(0),
data_repo_ui = character(0),
data_repo_db_ui = character(0),
data_repo_file_ext = character(0),
data_repo_save_type = character(0)){ 
args_ls <- list(file_type = file_type,
file_name = file_name,
data_repo = data_repo,
data_repo_ui = data_repo_ui,
data_repo_db_ui = data_repo_db_ui,
data_repo_file_ext = data_repo_file_ext,
data_repo_save_type = data_repo_save_type) %>% ready4class::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
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
