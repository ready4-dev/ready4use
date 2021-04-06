#' Transform comma separated variables file column to list column
#' @description transform_csv_col_to_ls_col() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform comma separated variables file column to list column. Function argument csv_col specifies the object to be updated. The function returns List column (a list).
#' @param csv_col_xx PARAM_DESCRIPTION
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
