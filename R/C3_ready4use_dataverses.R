
setOldClass(c("ready4use_dataverses","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @description Create a new valid instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse., Default: make_pt_ready4use_dataverses()
#' @return A validated instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @details ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @rdname ready4use_dataverses
#' @export 
ready4use_dataverses <- function(x = make_pt_ready4use_dataverses()){ 
validate_ready4use_dataverses(make_new_ready4use_dataverses(x))
}
#' Make new ready4use package dataverses ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @description Create a new unvalidated instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @return An unvalidated instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @details ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @rdname make_new_ready4use_dataverses
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4use_dataverses <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4use_dataverses",setdiff(make_pt_ready4use_dataverses() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4use package dataverses ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @description Create a new prototype for the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param file_type_chr File type (a character vector), Default: character(0)
#' @param file_name_chr File name (a character vector), Default: character(0)
#' @param data_repo_chr Data repository (a character vector), Default: character(0)
#' @param data_repo_ui_chr Data repository user interface (a character vector), Default: character(0)
#' @param data_repo_db_ui_chr Data repository database user interface (a character vector), Default: character(0)
#' @param data_repo_file_ext_chr Data repository file extension (a character vector), Default: character(0)
#' @param data_repo_save_type_chr Data repository save type (a character vector), Default: character(0)
#' @return A prototype for ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @details ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @rdname make_pt_ready4use_dataverses
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4use_dataverses <- function(file_type_chr = character(0),
file_name_chr = character(0),
data_repo_chr = character(0),
data_repo_ui_chr = character(0),
data_repo_db_ui_chr = character(0),
data_repo_file_ext_chr = character(0),
data_repo_save_type_chr = character(0)){ 
args_ls <- list(file_type_chr = file_type_chr,
file_name_chr = file_name_chr,
data_repo_chr = data_repo_chr,
data_repo_ui_chr = data_repo_ui_chr,
data_repo_db_ui_chr = data_repo_db_ui_chr,
data_repo_file_ext_chr = data_repo_file_ext_chr,
data_repo_save_type_chr = data_repo_save_type_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate ready4use package dataverses ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @description Validate an instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param x An unvalidated instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @return A prototpe for ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @details ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @rdname validate_ready4use_dataverses
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
validate_ready4use_dataverses <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4use_dataverses())],
names(make_pt_ready4use_dataverses())))!=length(names(make_pt_ready4use_dataverses()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4use_dataverses()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4use_dataverses() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4use_dataverses())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4use_dataverses() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class))
  vars_chr <- class_lup %>% dplyr::pull(1) %>% unique()
  classes_chr <- vars_chr %>%  purrr::map_chr(~dplyr::filter(class_lup, variable == .x) %>%  dplyr::pull(2) %>% paste0(collapse = ", "))
purrr::map2_chr(vars_chr,
classes_chr,
~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", 
")
}),
call. = FALSE)
}

x}
#' Is ready4use package dataverses ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @description Check whether an object is a valid instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @details ready4 S3 class for tibble object lookup table of files to be imported from a dataverse.
#' @rdname is_ready4use_dataverses
#' @export 
is_ready4use_dataverses <- function(x) inherits(validate_ready4use_dataverses(x), "ready4use_dataverses")
