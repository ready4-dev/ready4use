
setOldClass(c("ready4_par_struc_mape","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @description Create a new valid instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @param x A prototype for the ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors., Default: make_pt_ready4_par_struc_mape()
#' @return A validated instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @details ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @rdname ready4_par_struc_mape
#' @export 

ready4_par_struc_mape <- function(x = make_pt_ready4_par_struc_mape()){ 
validate_ready4_par_struc_mape(make_new_ready4_par_struc_mape(x))
}
#' Make new ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @description Create a new unvalidated instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @param x A prototype for the ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @return An unvalidated instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @details ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @rdname make_new_ready4_par_struc_mape
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4_par_struc_mape <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_par_struc_mape",setdiff(make_pt_ready4_par_struc_mape() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @description Create a new prototype for the ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @param param_name PARAM_DESCRIPTION, Default: character(0)
#' @param sex_age_band PARAM_DESCRIPTION, Default: character(0)
#' @param mape_05_yr_mde PARAM_DESCRIPTION, Default: numeric(0)
#' @param mape_10_yr_mde PARAM_DESCRIPTION, Default: numeric(0)
#' @param mape_15_yr_mde PARAM_DESCRIPTION, Default: numeric(0)
#' @param mape_05_yr_min PARAM_DESCRIPTION, Default: numeric(0)
#' @param mape_10_yr_min PARAM_DESCRIPTION, Default: numeric(0)
#' @param mape_15_yr_min PARAM_DESCRIPTION, Default: numeric(0)
#' @param mape_05_yr_max PARAM_DESCRIPTION, Default: numeric(0)
#' @param mape_10_yr_max PARAM_DESCRIPTION, Default: numeric(0)
#' @param mape_15_yr_max PARAM_DESCRIPTION, Default: numeric(0)
#' @param mape_05_yr_shp PARAM_DESCRIPTION, Default: numeric(0)
#' @param mape_10_yr_shp PARAM_DESCRIPTION, Default: numeric(0)
#' @param mape_15_yr_shp PARAM_DESCRIPTION, Default: numeric(0)
#' @return A prototype for ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @details ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @rdname make_pt_ready4_par_struc_mape
#' @export 
#' @importFrom ready4class update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4_par_struc_mape <- function(param_name = character(0),
sex_age_band = character(0),
mape_05_yr_mde = numeric(0),
mape_10_yr_mde = numeric(0),
mape_15_yr_mde = numeric(0),
mape_05_yr_min = numeric(0),
mape_10_yr_min = numeric(0),
mape_15_yr_min = numeric(0),
mape_05_yr_max = numeric(0),
mape_10_yr_max = numeric(0),
mape_15_yr_max = numeric(0),
mape_05_yr_shp = numeric(0),
mape_10_yr_shp = numeric(0),
mape_15_yr_shp = numeric(0)){ 
args_ls <- list(param_name = param_name,
sex_age_band = sex_age_band,
mape_05_yr_mde = mape_05_yr_mde,
mape_10_yr_mde = mape_10_yr_mde,
mape_15_yr_mde = mape_15_yr_mde,
mape_05_yr_min = mape_05_yr_min,
mape_10_yr_min = mape_10_yr_min,
mape_15_yr_min = mape_15_yr_min,
mape_05_yr_max = mape_05_yr_max,
mape_10_yr_max = mape_10_yr_max,
mape_15_yr_max = mape_15_yr_max,
mape_05_yr_shp = mape_05_yr_shp,
mape_10_yr_shp = mape_10_yr_shp,
mape_15_yr_shp = mape_15_yr_shp) %>% ready4class::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @description Validate an instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @param x An unvalidated instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @return A prototpe for ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @details ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @rdname validate_ready4_par_struc_mape
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_par_struc_mape <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4_par_struc_mape())],
names(make_pt_ready4_par_struc_mape())))!=length(names(make_pt_ready4_par_struc_mape()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4_par_struc_mape()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_pt_ready4_par_struc_mape() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4_par_struc_mape())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_ready4_par_struc_mape() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4_par_struc_mape() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' Is ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @description Check whether an object is a valid instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @details ready4 S3 class for tibble object that stores simulation structural parameters relating to Mean Absolute Prediction Errors.
#' @rdname is_ready4_par_struc_mape
#' @export 

is_ready4_par_struc_mape <- function(x) inherits(validate_ready4_par_struc_mape(x), "ready4_par_struc_mape")
