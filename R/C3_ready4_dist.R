
#' 
#' @description Create a new valid instance of the S3 class: ready4_dist
#' @param x PARAM_DESCRIPTION, Default: make_prototype_ready4_dist()
#' @return A validated instance of the ready4_dist class
#' @details Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @rdname ready4_dist
#' @export 

ready4_dist <- function(x = make_prototype_ready4_dist()){ 
validate_ready4_dist(new_ready4_dist(x))
}
#' 
#' @description Create a new unvalidated instance of the S3 class: new_ready4_dist
#' @param x PARAM_DESCRIPTION
#' @return An unvalidated instance of the ready4_dist class
#' @details Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @rdname new_ready4_dist
#' @export 

new_ready4_dist <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4_dist",setdiff(make_prototype_ready4_dist() %>% class(),class(x))),
class(x))
x
}
#' 
#' @description Create a new prototype for S3 class: make_prototype_ready4_dist

#' @return A prototpe for ready4_dist class
#' @details Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @rdname make_prototype_ready4_dist
#' @export 

make_prototype_ready4_dist <- function(){ 
list(distribution = character(0),
dist_param_1 = numeric(0),
dist_param_2 = numeric(0),
dist_param_3 = numeric(0),
dist_param_4 = numeric(0),
transformation = character(0))
}
#' 
#' @description Validate an instance of the S3 class: validate_ready4_dist
#' @param x PARAM_DESCRIPTION
#' @return A prototpe for ready4_dist class
#' @details Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @rdname validate_ready4_dist
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr arrange filter pull
#' @importFrom purrr map2_chr
validate_ready4_dist <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_prototype_ready4_dist())],
names(make_prototype_ready4_dist())))!=length(names(make_prototype_ready4_dist()))){
stop(paste0("LIST must include elements named: ",
names(make_prototype_ready4_dist()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_prototype_ready4_dist() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_prototype_ready4_dist())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
purrr::map2_chr(make_prototype_ready4_dist() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_prototype_ready4_dist() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' 
#' @description Check whether an object is a valid instance of the S3 class: is_ready4_dist
#' @param x PARAM_DESCRIPTION
#' @return A logical value, TRUE if a valid instance of the ready4_dist class
#' @details Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @rdname is_ready4_dist
#' @export 

is_ready4_dist <- function(x) inherits(validate_ready4_dist(x), "ready4_dist")
