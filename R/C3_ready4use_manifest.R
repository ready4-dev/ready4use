
#' ready4 s3 class defining a manifest of data required to create an R package.
#' @description Create a new valid instance of the ready4 s3 class defining a manifest of data required to create an R package.
#' @param x A prototype for the ready4 s3 class defining a manifest of data required to create an R package., Default: make_pt_ready4use_manifest()
#' @return A validated instance of the ready4 s3 class defining a manifest of data required to create an R package.
#' @details ready4 s3 class Manifest for packages containing datasets.
#' @rdname ready4use_manifest
#' @export 
ready4use_manifest <- function(x = make_pt_ready4use_manifest()){ 
validate_ready4use_manifest(make_new_ready4use_manifest(x))
}
#' Make new ready4use package manifest ready4 s3 class defining a manifest of data required to create an R package.
#' @description Create a new unvalidated instance of the ready4 s3 class defining a manifest of data required to create an R package.
#' @param x A prototype for the ready4 s3 class defining a manifest of data required to create an R package.
#' @return An unvalidated instance of the ready4 s3 class defining a manifest of data required to create an R package.
#' @details ready4 s3 class Manifest for packages containing datasets.
#' @rdname make_new_ready4use_manifest
#' @export 
make_new_ready4use_manifest <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4use_manifest",setdiff(make_pt_ready4use_manifest() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4use package manifest ready4 s3 class defining a manifest of data required to create an R package.
#' @description Create a new prototype for the ready4 s3 class defining a manifest of data required to create an R package.
#' @param fns_ready4fun_manifest PARAM_DESCRIPTION, Default: ready4fun::ready4fun_manifest()
#' @param constructor_r3 Constructor (a ready4 S3), Default: ready4class::ready4class_constructor()
#' @param pkg_ds_ls_ls Package dataset (a list of lists), Default: list()
#' @param clss_to_apply_ls Classes to apply (a list), Default: list()
#' @return A prototype for ready4 s3 class defining a manifest of data required to create an R package.
#' @details ready4 s3 class Manifest for packages containing datasets.
#' @rdname make_pt_ready4use_manifest
#' @export 
#' @importFrom ready4fun ready4fun_manifest update_pt_fn_args_ls
#' @importFrom ready4class ready4class_constructor
#' @importFrom rlang exec
make_pt_ready4use_manifest <- function(fns_ready4fun_manifest = ready4fun::ready4fun_manifest(),
constructor_r3 = ready4class::ready4class_constructor(),
pkg_ds_ls_ls = list(),
clss_to_apply_ls = list()){ 
args_ls <- list(fns_ready4fun_manifest = fns_ready4fun_manifest,
constructor_r3 = constructor_r3,
pkg_ds_ls_ls = pkg_ds_ls_ls,
clss_to_apply_ls = clss_to_apply_ls) %>% ready4fun::update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' Validate ready4use package manifest ready4 s3 class defining a manifest of data required to create an R package.
#' @description Validate an instance of the ready4 s3 class defining a manifest of data required to create an R package.
#' @param x An unvalidated instance of the ready4 s3 class defining a manifest of data required to create an R package.
#' @return A prototpe for ready4 s3 class defining a manifest of data required to create an R package.
#' @details ready4 s3 class Manifest for packages containing datasets.
#' @rdname validate_ready4use_manifest
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom ready4fun transform_cls_type_ls
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr filter arrange pull
#' @importFrom purrr map_chr map2_chr
validate_ready4use_manifest <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4use_manifest())],
names(make_pt_ready4use_manifest())))!=length(names(make_pt_ready4use_manifest()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4use_manifest()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4use_manifest() %>% 
lapply(class) %>% ready4fun::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% ready4fun::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4use_manifest())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4use_manifest() %>% 
lapply(class) %>% ready4fun::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
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
#' Is ready4use package manifest ready4 s3 class defining a manifest of data required to create an R package.
#' @description Check whether an object is a valid instance of the ready4 s3 class defining a manifest of data required to create an R package.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 s3 class defining a manifest of data required to create an R package.
#' @details ready4 s3 class Manifest for packages containing datasets.
#' @rdname is_ready4use_manifest
#' @export 
is_ready4use_manifest <- function(x) inherits(validate_ready4use_manifest(x), "ready4use_manifest")
