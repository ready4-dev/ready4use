#' Transform csv column to list column
#' @description transform_csv_col_to_ls_col() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform csv col to a list col. Function argument csv_col specifies the object to be updated. Argument NA provides the object to be updated.The function returns a list col (a list).
#' @param csv_col PARAM_DESCRIPTION
#' @return List col (a list)
#' @rdname transform_csv_col_to_ls_col
#' @export 
#' @importFrom purrr map
#' @importFrom stringr str_split str_sub
#' @keywords internal
transform_csv_col_to_ls_col <- function (csv_col) 
{
    ls_col_ls <- purrr::map(csv_col, ~ifelse(is.na(.x), .x, ifelse(startsWith(.x, 
        "c("), .x, paste0("c(\"", .x, "\")")))) %>% purrr::map(~stringr::str_split(.x %>% 
        stringr::str_sub(start = 3, end = -2), ",") %>% unlist()) %>% 
        purrr::map(~unlist(.x)) %>% purrr::map(~parse(text = paste0("c(", 
        paste0(.x, collapse = ","), ")")) %>% eval())
    return(ls_col_ls)
}
