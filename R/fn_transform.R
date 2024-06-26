#' Transform comma separated variables file column to list column
#' @description transform_csv_col_to_ls_col() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform comma separated variables file column to list column. The function returns List column (a list).
#' @param csv_col_xx Comma separated variables file column (an output object of multiple potential types)
#' @return List column (a list)
#' @rdname transform_csv_col_to_ls_col
#' @export 
#' @importFrom purrr map
#' @importFrom stringr str_split str_sub
#' @keywords internal
transform_csv_col_to_ls_col <- function (csv_col_xx) 
{
    ls_col_ls <- purrr::map(csv_col_xx, ~ifelse(is.na(.x), .x, 
        ifelse(startsWith(.x, "c("), .x, paste0("c(\"", .x, "\")")))) %>% 
        purrr::map(~stringr::str_split(.x %>% stringr::str_sub(start = 3, 
            end = -2), ",") %>% unlist()) %>% purrr::map(~unlist(.x)) %>% 
        purrr::map(~parse(text = paste0("c(", paste0(.x, collapse = ","), 
            ")")) %>% eval())
    return(ls_col_ls)
}
#' Transform dates
#' @description transform_dates() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform dates. The function returns Dates (a date vector).
#' @param dates_chr Dates (a character vector)
#' @param drop_time_1L_lgl Drop time (a logical vector of length one), Default: F
#' @return Dates (a date vector)
#' @rdname transform_dates
#' @export 
#' @importFrom purrr map_chr
#' @importFrom stringr str_sub
#' @importFrom lubridate as_date
#' @keywords internal
transform_dates <- function (dates_chr, drop_time_1L_lgl = F) 
{
    dates_chr <- dates_chr %>% strsplit("/") %>% purrr::map_chr(~paste0(stringr::str_sub(.x[3], 
        end = 4), "-", .x[2], "-", .x[1], ifelse(nchar(.x[3]) > 
        4, stringr::str_sub(.x[3], start = 5), "")))
    if (drop_time_1L_lgl) {
        dates_chr <- dates_chr %>% stringr::str_sub(end = 10)
    }
    dates_dtm <- dates_chr %>% lubridate::as_date()
    return(dates_dtm)
}
#' Transform dyad to series
#' @description transform_dyad_to_series() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform dyad to series. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: Ready4useDyad()
#' @param timepoint_vals_chr Timepoint values (a character vector), Default: c("baseline", "follow_up")
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'uid_chr'
#' @param participation_var_1L_chr Participation variable (a character vector of length one), Default: 'participation'
#' @param timepoint_var_nm_1L_chr Timepoint variable name (a character vector of length one), Default: 'timing_fct'
#' @return Z (A longitudinal dataset and its associated dictionary, descriptive statistics and metadata.)
#' @rdname transform_dyad_to_series
#' @export 
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @keywords internal
transform_dyad_to_series <- function (X_Ready4useDyad = Ready4useDyad(), timepoint_vals_chr = c("baseline", 
    "follow_up"), id_var_nm_1L_chr = "uid_chr", participation_var_1L_chr = "participation", 
    timepoint_var_nm_1L_chr = "timing_fct") 
{
    Z_YouthvarsSeries <- YouthvarsSeries(a_Ready4useDyad = X_Ready4useDyad, 
        id_var_nm_1L_chr = id_var_nm_1L_chr, participation_var_1L_chr = participation_var_1L_chr, 
        timepoint_vals_chr = timepoint_vals_chr, timepoint_var_nm_1L_chr = timepoint_var_nm_1L_chr)
    Z_YouthvarsSeries@a_Ready4useDyad@ds_tb <- Z_YouthvarsSeries@a_Ready4useDyad@ds_tb %>% 
        dplyr::filter(!!rlang::sym(timepoint_var_nm_1L_chr) %in% 
            Z_YouthvarsSeries@timepoint_vals_chr)
    return(Z_YouthvarsSeries)
}
#' Transform raw dataframe
#' @description transform_raw_df() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform raw dataframe. The function returns Dataset (a tibble).
#' @param data_df Data (a data.frame)
#' @param cases_start_at_1L_int Cases start at (an integer vector of length one), Default: 1
#' @return Dataset (a tibble)
#' @rdname transform_raw_df
#' @export 
#' @importFrom dplyr slice
#' @importFrom tibble as_tibble
#' @keywords internal
transform_raw_df <- function (data_df, cases_start_at_1L_int = 1L) 
{
    ds_tb <- data_df %>% dplyr::slice(cases_start_at_1L_int:nrow(.)) %>% 
        tibble::as_tibble()
    return(ds_tb)
}
