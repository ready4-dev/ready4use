
#' Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @description Create a new valid instance of the Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @param x A prototype for the Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution, Default: make_prototype_ready4_dist()
#' @return A validated instance of the Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @details Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @rdname ready4_dist
#' @export 

ready4_dist <- function(x = make_prototype_ready4_dist()){ 
validate_ready4_dist(make_new_ready4_dist(x))
}
#' Make new Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @description Create a new unvalidated instance of the Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @param x A prototype for the Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @return An unvalidated instance of the Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @details Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @rdname make_new_ready4_dist
#' @export 

make_new_ready4_dist <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4_dist",setdiff(make_prototype_ready4_dist() %>% class(),class(x))),
class(x))
x
}
#' Make prototype Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @description Create a new prototype for the Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution

#' @return A prototype for Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
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
#' Validate Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @description Validate an instance of the Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @param x An unvalidated instance of the Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @return A prototpe for Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
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
#' Is Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @description Check whether an object is a valid instance of the Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @details Readyforwhatsnext S3 class for list object that summarises the parameters of each distribution
#' @rdname is_ready4_dist
#' @export 

is_ready4_dist <- function(x) inherits(validate_ready4_dist(x), "ready4_dist")
