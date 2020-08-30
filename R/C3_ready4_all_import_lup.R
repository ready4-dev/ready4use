#' @include C3_ready4_dv_import_lup.R

#' 
#' @description Create a new valid instance of the S3 class: ready4_all_import_lup
#' @param x PARAM_DESCRIPTION, Default: make_prototype_ready4_all_import_lup()
#' @return A validated instance of the ready4_all_import_lup class
#' @details Readyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @rdname ready4_all_import_lup
#' @export 

ready4_all_import_lup <- function(x = make_prototype_ready4_all_import_lup()){ 
validate_ready4_all_import_lup(new_ready4_all_import_lup(x))
}
#' 
#' @description Create a new unvalidated instance of the S3 class: new_ready4_all_import_lup
#' @param x PARAM_DESCRIPTION
#' @return An unvalidated instance of the ready4_all_import_lup class
#' @details Readyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @rdname new_ready4_all_import_lup
#' @export 
#' @importFrom tibble is_tibble
new_ready4_all_import_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_all_import_lup",setdiff(make_prototype_ready4_all_import_lup() %>% class(),class(x))),
class(x))
x
}
#' 
#' @description Create a new prototype for S3 class: make_prototype_ready4_all_import_lup

#' @return A prototpe for ready4_all_import_lup class
#' @details Readyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @rdname make_prototype_ready4_all_import_lup
#' @export 
#' @importFrom purrr reduce
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
make_prototype_ready4_all_import_lup <- function(){ 
purrr::reduce(names(tibble::tibble(local_file_src = character(0),
make_script_src = character(0),
download_url = character(0),
inc_file_main = character(0),
inc_files_to_rename = list(),
new_names_for_inc_files = list())),
.init = ready4_dv_import_lup(),
 ~ .x %>% dplyr::mutate(!!rlang::sym(.y) := eval(parse(text=tibble::tibble(local_file_src = character(0),
make_script_src = character(0),
download_url = character(0),
inc_file_main = character(0),
inc_files_to_rename = list(),
new_names_for_inc_files = list())[.y]))))%>% dplyr::select(c(local_file_src,make_script_src,download_url,inc_file_main,inc_files_to_rename,new_names_for_inc_files,file_type,file_name,data_repo,data_repo_ui,data_repo_db_ui,data_repo_file_ext,data_repo_save_type))
}
#' 
#' @description Validate an instance of the S3 class: validate_ready4_all_import_lup
#' @param x PARAM_DESCRIPTION
#' @return A prototpe for ready4_all_import_lup class
#' @details Readyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @rdname validate_ready4_all_import_lup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_all_import_lup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_prototype_ready4_all_import_lup())],
names(make_prototype_ready4_all_import_lup())))!=length(names(make_prototype_ready4_all_import_lup()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_prototype_ready4_all_import_lup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_prototype_ready4_all_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_prototype_ready4_all_import_lup())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_prototype_ready4_all_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_prototype_ready4_all_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' 
#' @description Check whether an object is a valid instance of the S3 class: is_ready4_all_import_lup
#' @param x PARAM_DESCRIPTION
#' @return A logical value, TRUE if a valid instance of the ready4_all_import_lup class
#' @details Readyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @rdname is_ready4_all_import_lup
#' @export 

is_ready4_all_import_lup <- function(x) inherits(validate_ready4_all_import_lup(x), "ready4_all_import_lup")
