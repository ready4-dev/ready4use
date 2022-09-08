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
