
#' ready4 S3 class for list object that summarises the parameters of each distribution
#' @description Create a new valid instance of the ready4 S3 class for list object that summarises the parameters of each distribution
#' @param x A prototype for the ready4 S3 class for list object that summarises the parameters of each distribution, Default: make_pt_ready4use_distributions()
#' @return A validated instance of the ready4 S3 class for list object that summarises the parameters of each distribution
#' @details ready4 S3 class for list object that summarises the parameters of each distribution
#' @rdname ready4use_distributions
#' @export 
ready4use_distributions <- function(x = make_pt_ready4use_distributions()){ 
validate_ready4use_distributions(make_new_ready4use_distributions(x))
}
#' make new ready4use package distributions ready4 S3 class for list object that summarises the parameters of each distribution
#' @description Create a new unvalidated instance of the ready4 S3 class for list object that summarises the parameters of each distribution
#' @param x A prototype for the ready4 S3 class for list object that summarises the parameters of each distribution
#' @return An unvalidated instance of the ready4 S3 class for list object that summarises the parameters of each distribution
#' @details ready4 S3 class for list object that summarises the parameters of each distribution
#' @rdname make_new_ready4use_distributions
#' @export 
make_new_ready4use_distributions <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4use_distributions",setdiff(make_pt_ready4use_distributions() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4use package distributions ready4 S3 class for list object that summarises the parameters of each distribution
#' @description Create a new prototype for the ready4 S3 class for list object that summarises the parameters of each distribution
#' @param distribution_chr Distribution (a character vector), Default: character(0)
#' @param dstr_param_1_dbl Distribution parameter 1 (a double vector), Default: numeric(0)
#' @param dstr_param_2_dbl Distribution parameter 2 (a double vector), Default: numeric(0)
#' @param dstr_param_3_dbl Distribution parameter 3 (a double vector), Default: numeric(0)
#' @param dstr_param_4_dbl Distribution parameter 4 (a double vector), Default: numeric(0)
#' @param transformation_chr Transformation (a character vector), Default: character(0)
#' @return A prototype for ready4 S3 class for list object that summarises the parameters of each distribution
#' @details ready4 S3 class for list object that summarises the parameters of each distribution
#' @rdname make_pt_ready4use_distributions
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
make_pt_ready4use_distributions <- function(distribution_chr = character(0),
dstr_param_1_dbl = numeric(0),
dstr_param_2_dbl = numeric(0),
dstr_param_3_dbl = numeric(0),
dstr_param_4_dbl = numeric(0),
transformation_chr = character(0)){ 
args_ls <- list(distribution_chr = distribution_chr,
dstr_param_1_dbl = dstr_param_1_dbl,
dstr_param_2_dbl = dstr_param_2_dbl,
dstr_param_3_dbl = dstr_param_3_dbl,
dstr_param_4_dbl = dstr_param_4_dbl,
transformation_chr = transformation_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' validate ready4use package distributions ready4 S3 class for list object that summarises the parameters of each distribution
#' @description Validate an instance of the ready4 S3 class for list object that summarises the parameters of each distribution
#' @param x An unvalidated instance of the ready4 S3 class for list object that summarises the parameters of each distribution
#' @return A prototpe for ready4 S3 class for list object that summarises the parameters of each distribution
#' @details ready4 S3 class for list object that summarises the parameters of each distribution
#' @rdname validate_ready4use_distributions
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom ready4 transform_cls_type_ls
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr filter arrange pull
#' @importFrom purrr map_chr map2_chr
validate_ready4use_distributions <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4use_distributions())],
names(make_pt_ready4use_distributions())))!=length(names(make_pt_ready4use_distributions()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4use_distributions()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4use_distributions() %>% 
lapply(class) %>% ready4::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% ready4::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4use_distributions())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4use_distributions() %>% 
lapply(class) %>% ready4::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
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
#' is ready4use package distributions ready4 S3 class for list object that summarises the parameters of each distribution
#' @description Check whether an object is a valid instance of the ready4 S3 class for list object that summarises the parameters of each distribution
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for list object that summarises the parameters of each distribution
#' @details ready4 S3 class for list object that summarises the parameters of each distribution
#' @rdname is_ready4use_distributions
#' @export 
is_ready4use_distributions <- function(x) inherits(validate_ready4use_distributions(x), "ready4use_distributions")
