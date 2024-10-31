transform_csv_col_to_ls_col <- function(csv_col_xx){
  ls_col_ls <- purrr::map(csv_col_xx,
                       ~ ifelse(is.na(.x),
                              .x,
                              ifelse(startsWith(.x,"c("),
                                     .x,
                                     paste0("c(\"",.x,"\")")))) %>%
    purrr::map(~ stringr::str_split(.x %>% stringr::str_sub(start=3,end=-2),",") %>% unlist()) %>% purrr::map(~unlist(.x)) %>%
    purrr::map( ~ parse(text=paste0("c(",paste0(.x,collapse = ','),")")) %>% eval())
  return(ls_col_ls)
}
transform_dates <- function(dates_chr,
                            drop_time_1L_lgl = F){
  dates_chr <- dates_chr %>% strsplit("/") %>% purrr::map_chr(~paste0(stringr::str_sub(.x[3], end =4), "-", .x[2], "-", .x[1],
                                                                      ifelse(nchar(.x[3]) > 4,stringr::str_sub(.x[3], start =5),"")))
  if(drop_time_1L_lgl){
    dates_chr <- dates_chr %>% stringr::str_sub(end = 10)
  }
  dates_dtm <- dates_chr %>% lubridate::as_date()
  return(dates_dtm)
}
# transform_dyad_to_series <- function(X_Ready4useDyad = Ready4useDyad(),
#                                      timepoint_vals_chr = c("baseline","follow_up"),
#                                      id_var_nm_1L_chr = "uid_chr",
#                                      participation_var_1L_chr = "participation",
#                                      timepoint_var_nm_1L_chr = "timing_fct"){
#   Z_YouthvarsSeries <- YouthvarsSeries(a_Ready4useDyad = X_Ready4useDyad, id_var_nm_1L_chr = id_var_nm_1L_chr, participation_var_1L_chr = participation_var_1L_chr,
#                                        timepoint_vals_chr = timepoint_vals_chr, timepoint_var_nm_1L_chr = timepoint_var_nm_1L_chr)
#   Z_YouthvarsSeries@a_Ready4useDyad@ds_tb  <- Z_YouthvarsSeries@a_Ready4useDyad@ds_tb %>% dplyr::filter(!!rlang::sym(timepoint_var_nm_1L_chr) %in% Z_YouthvarsSeries@timepoint_vals_chr)
#   return(Z_YouthvarsSeries)
# }
transform_raw_df <- function(data_df,
                             cases_start_at_1L_int = 1L){
  ds_tb <- data_df %>% dplyr::slice(cases_start_at_1L_int:nrow(.)) %>% tibble::as_tibble()
  return(ds_tb)
}
