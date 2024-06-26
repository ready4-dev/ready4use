#' Remove labels from dataset
#' @description remove_labels_from_ds() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove labels from dataset. The function returns Unlabelled dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @return Unlabelled dataset (a tibble)
#' @rdname remove_labels_from_ds
#' @export 
#' @importFrom purrr reduce
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
remove_labels_from_ds <- function (ds_tb) 
{
    unlabelled_ds_tb <- seq_along(ds_tb) %>% purrr::reduce(.init = ds_tb, 
        ~{
            col_vec <- .x[[names(.x[.y])]]
            class(col_vec) <- setdiff(class(col_vec), "labelled")
            attr(col_vec, "label") <- NULL
            .x %>% dplyr::mutate(`:=`(!!rlang::sym(names(.x[.y])), 
                col_vec))
        })
    return(unlabelled_ds_tb)
}
