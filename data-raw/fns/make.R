make_r3_from_csv_tb <- function(csv_tb,
                                r3_fn){ ## NEED TO EDIT inc_files_to_rename list col logic
  list_cols <- rlang::exec(r3_fn) %>%
    dplyr::select_if(is.list) %>% names()
  char_cols <- rlang::exec(r3_fn) %>%
    dplyr::select_if(is.character) %>% names()
  tb <- csv_tb %>%
    tibble::as_tibble() %>%
    dplyr::mutate_at(.vars = list_cols, ~ purrr::map(.,~.x)) %>%
    dplyr::mutate_at(.vars = list_cols, .funs = transform_csv_col_to_ls_col) %>%
    dplyr::mutate_at(.vars = list_cols, .funs = ~ purrr::map(.,~if(all(is.na(.x))){NULL}else{.x})) %>%
    dplyr::mutate_at(.vars = char_cols, .funs = as.character) %>%
    dplyr::select(names(rlang::exec(r3_fn)))
  tb_r3 <- rlang::exec(r3_fn, tb)
  return(tb_r3)
}
