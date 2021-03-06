remove_labels_from_ds <- function(ds_tb){
  unlabelled_ds_tb <- seq_along(ds_tb) %>%
    purrr::reduce(.init = ds_tb,
                  ~ {
                    col_vec <- .x[[names(.x[.y])]]
                    class(col_vec) <- setdiff(class(col_vec), "labelled")
                    attr(col_vec, "label") <- NULL
                    .x
                  })
  return(unlabelled_ds_tb)
}
