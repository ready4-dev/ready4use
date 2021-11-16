
setOldClass(c("ready4use_dictionary","tbl_df", "tbl", "data.frame"))
#' ready4 s3 class defining a data dictionary tibble.
#' @description Create a new valid instance of the ready4 s3 class defining a data dictionary tibble.
#' @param x A prototype for the ready4 s3 class defining a data dictionary tibble., Default: make_pt_ready4use_dictionary()
#' @return A validated instance of the ready4 s3 class defining a data dictionary tibble.
#' @details A data dictionary tibble.
#' @rdname ready4use_dictionary
#' @export 
ready4use_dictionary <- function(x = make_pt_ready4use_dictionary()){ 
validate_ready4use_dictionary(make_new_ready4use_dictionary(x))
}
#' make new ready4use package dictionary ready4 s3 class defining a data dictionary tibble.
#' @description Create a new unvalidated instance of the ready4 s3 class defining a data dictionary tibble.
#' @param x A prototype for the ready4 s3 class defining a data dictionary tibble.
#' @return An unvalidated instance of the ready4 s3 class defining a data dictionary tibble.
#' @details A data dictionary tibble.
#' @rdname make_new_ready4use_dictionary
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4use_dictionary <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4use_dictionary",setdiff(make_pt_ready4use_dictionary() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4use package dictionary ready4 s3 class defining a data dictionary tibble.
#' @description Create a new prototype for the ready4 s3 class defining a data dictionary tibble.
#' @param var_nm_chr Variable name (a character vector), Default: character(0)
#' @param var_ctg_chr Variable category categories (a character vector), Default: character(0)
#' @param var_desc_chr Variable description (a character vector), Default: character(0)
#' @param var_type_chr Variable type (a character vector), Default: character(0)
#' @return A prototype for ready4 s3 class defining a data dictionary tibble.
#' @details A data dictionary tibble.
#' @rdname make_pt_ready4use_dictionary
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4use_dictionary <- function(var_nm_chr = character(0),
var_ctg_chr = character(0),
var_desc_chr = character(0),
var_type_chr = character(0)){ 
args_ls <- list(var_nm_chr = var_nm_chr,
var_ctg_chr = var_ctg_chr,
var_desc_chr = var_desc_chr,
var_type_chr = var_type_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate ready4use package dictionary ready4 s3 class defining a data dictionary tibble.
#' @description Validate an instance of the ready4 s3 class defining a data dictionary tibble.
#' @param x An unvalidated instance of the ready4 s3 class defining a data dictionary tibble.
#' @return A prototpe for ready4 s3 class defining a data dictionary tibble.
#' @details A data dictionary tibble.
#' @rdname validate_ready4use_dictionary
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
validate_ready4use_dictionary <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4use_dictionary())],
names(make_pt_ready4use_dictionary())))!=length(names(make_pt_ready4use_dictionary()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4use_dictionary()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4use_dictionary() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4use_dictionary())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4use_dictionary() %>% 
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
#' is ready4use package dictionary ready4 s3 class defining a data dictionary tibble.
#' @description Check whether an object is a valid instance of the ready4 s3 class defining a data dictionary tibble.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 s3 class defining a data dictionary tibble.
#' @details A data dictionary tibble.
#' @rdname is_ready4use_dictionary
#' @export 
is_ready4use_dictionary <- function(x) inherits(validate_ready4use_dictionary(x), "ready4use_dictionary")
