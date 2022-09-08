remove_labels_from_ds <- function(ds_tb){ # NEEDS WORK TO WORK WITH GROUPED DATA. See make_item_plt fn from youthvars
  unlabelled_ds_tb <- seq_along(ds_tb) %>%
    purrr::reduce(.init = ds_tb,
                  ~ {
                    col_vec <- .x[[names(.x[.y])]]
                    class(col_vec) <- setdiff(class(col_vec), "labelled")
                    attr(col_vec, "label") <- NULL
                    .x %>%
                      dplyr::mutate(!!rlang::sym(names(.x[.y])) := col_vec)
                  })
  return(unlabelled_ds_tb)
}
