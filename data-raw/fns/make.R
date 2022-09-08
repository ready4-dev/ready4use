make_r3_from_csv_tb <- function(csv_tb,
                                r3_fn){ ## NEED TO EDIT inc_fls_to_rename_ls list col logic
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
# make_files_tb <- function(paths_to_dirs_chr,
#                           recode_ls,
#                           inc_fl_types_chr = NA_character_){
#   lifecycle::deprecate_soft("0.0.0.9149", "make_files_tb()", "ready4::make_files_tb()")
#   files_tb <- purrr::map_dfr(paths_to_dirs_chr,
#                              ~{
#                                files_chr_vec <- list.files(.x)
#                                if(!identical(files_chr_vec,character(0))){
#                                  tb <- tibble::tibble(dir_chr = rep(.x,length(files_chr_vec)),
#                                                       file_chr = files_chr_vec %>%
#                                                         purrr::map_chr(~stringr::str_sub(.x,
#                                                                                          end = as.vector(stringi::stri_locate_last_regex(.x, "\\.")[,1])-1)),
#                                                       file_type_chr = files_chr_vec %>%
#                                                         purrr::map_chr(~stringr::str_sub(.x,
#                                                                                          start = as.vector(stringi::stri_locate_last_regex(.x, "\\.")[,1]))))
#
#                                  tb
#                                }
#                              })
#   if(!is.na(inc_fl_types_chr))
#     files_tb <- files_tb %>%
#       dplyr::filter(file_type_chr %in% inc_fl_types_chr)
#   files_tb <- files_tb %>%
#     dplyr::filter(file_chr %in% names(recode_ls))
#   description_chr <- purrr::map_chr(files_tb$file_chr,
#                                     ~ {
#                                       arg_ls <- append(list(EXPR=.x),recode_ls)
#                                       rlang::exec(.fn = switch, !!!arg_ls)
#                                     })
#   files_tb <- files_tb %>%
#     dplyr::mutate(description_chr = description_chr,
#                   ds_file_ext_chr = purrr::map_chr(file_type_chr,
#                                                    ~ ifelse(.x %in% c(".csv", ".xls",".xlsx"),
#                                                             ".tab",
#                                                             ".zip")))
#   assertthat::are_equal(nrow(files_tb),
#                         paste0(files_tb$file_chr,
#                                files_tb$file_type_chr) %>%
#                           unique() %>%
#                           length())
#   return(files_tb)
# }
