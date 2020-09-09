#' Make readyforwhatsnext S3 from comma separated variables file
#' @description make_r3_from_csv_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make readyforwhatsnext s3 from comma separated variables file tibble. The function returns Tibble readyforwhatsnext S3 (a ready4 S3 extension of tibble).
#' @param csv_tb Comma separated variables file (a tibble)
#' @param r3_fn Readyforwhatsnext S3 (a function)
#' @return Tibble readyforwhatsnext S3 (a ready4 S3 extension of tibble)
#' @rdname make_r3_from_csv_tb
#' @export 
#' @importFrom rlang exec
#' @importFrom dplyr select_if mutate_at select
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @keywords internal
make_r3_from_csv_tb <- function (csv_tb, r3_fn) 
{
    list_cols <- rlang::exec(r3_fn) %>% dplyr::select_if(is.list) %>% 
        names()
    char_cols <- rlang::exec(r3_fn) %>% dplyr::select_if(is.character) %>% 
        names()
    tb <- csv_tb %>% tibble::as_tibble() %>% dplyr::mutate_at(.vars = list_cols, 
        ~purrr::map(., ~.x)) %>% dplyr::mutate_at(.vars = list_cols, 
        .funs = transform_csv_col_to_ls_col) %>% dplyr::mutate_at(.vars = list_cols, 
        .funs = ~purrr::map(., ~if (all(is.na(.x))) {
            NULL
        }
        else {
            .x
        })) %>% dplyr::mutate_at(.vars = char_cols, .funs = as.character) %>% 
        dplyr::select(names(rlang::exec(r3_fn)))
    tb_r3 <- rlang::exec(r3_fn, tb)
    return(tb_r3)
}
