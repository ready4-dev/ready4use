#' Make files tibble
#' @description make_files_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make files tibble. The function returns Files (a tibble).
#' @param paths_to_dirs_chr Paths to directories (a character vector)
#' @param recode_ls Recode (a list)
#' @param inc_fl_types_chr Include file types (a character vector), Default: 'NA'
#' @return Files (a tibble)
#' @rdname make_files_tb
#' @export 
#' @importFrom purrr map_dfr map_chr
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @importFrom stringi stri_locate_last_regex
#' @importFrom dplyr filter mutate
#' @importFrom rlang exec
#' @importFrom assertthat are_equal
#' @keywords internal
make_files_tb <- function (paths_to_dirs_chr, recode_ls, inc_fl_types_chr = NA_character_) 
{
    files_tb <- purrr::map_dfr(paths_to_dirs_chr, ~{
        files_chr_vec <- list.files(.x)
        if (!identical(files_chr_vec, character(0))) {
            tb <- tibble::tibble(dir_chr = rep(.x, length(files_chr_vec)), 
                file_chr = files_chr_vec %>% purrr::map_chr(~stringr::str_sub(.x, 
                  end = as.vector(stringi::stri_locate_last_regex(.x, 
                    "\\.")[, 1]) - 1)), file_type_chr = files_chr_vec %>% 
                  purrr::map_chr(~stringr::str_sub(.x, start = as.vector(stringi::stri_locate_last_regex(.x, 
                    "\\.")[, 1]))))
            tb
        }
    })
    if (!is.na(inc_fl_types_chr)) 
        files_tb <- files_tb %>% dplyr::filter(file_type_chr %in% 
            inc_fl_types_chr)
    files_tb <- files_tb %>% dplyr::filter(file_chr %in% names(recode_ls))
    description_chr <- purrr::map_chr(files_tb$file_chr, ~{
        arg_ls <- append(list(EXPR = .x), recode_ls)
        rlang::exec(.fn = switch, !!!arg_ls)
    })
    files_tb <- files_tb %>% dplyr::mutate(description_chr = description_chr, 
        ds_file_ext_chr = purrr::map_chr(file_type_chr, ~ifelse(.x %in% 
            c(".csv", ".xls", ".xlsx"), ".tab", ".zip")))
    assertthat::are_equal(nrow(files_tb), paste0(files_tb$file_chr, 
        files_tb$file_type_chr) %>% unique() %>% length())
    return(files_tb)
}
#' Make ready4 S3 from comma separated variables file tibble
#' @description make_r3_from_csv_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make ready4 s3 from comma separated variables file tibble. The function returns Tibble ready4 S3 (a ready4 S3 extension of tibble).
#' @param csv_tb Comma separated variables file (a tibble)
#' @param r3_fn Ready4 S3 (a function)
#' @return Tibble ready4 S3 (a ready4 S3 extension of tibble)
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
#' Make method applied to ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import..
#' @description make.ready4use_all_import_lup() is a Make method that creates a new R object. This method is implemented for the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param forced_choice_chr Forced choice (a character vector), Default: 'NA'
#' @param script_args_ls Script arguments (a list), Default: NULL
#' @return NULL
#' @rdname make.ready4use_all_import_lup
#' @export 
#' @importFrom rlang exec
make.ready4use_all_import_lup <- function (x, forced_choice_chr = NA_character_, script_args_ls = NULL) 
{
    assert_single_row_tb(x)
    import_type_ls <- procure(x, inc_script_lgl = !is.null(script_args_ls), 
        forced_choice_chr = forced_choice_chr)
    switch(names(import_type_ls), script_chr = rlang::exec(ready4use_script_data, 
        x, !!!script_args_ls), local_chr = get_valid_path_chr(import_type_ls[[1]]), 
        repo_chr = make(x), source_url_chr = url(import_type_ls[[1]]))
}
#' Make.ready4use sp import lookup table
#' @description make.ready4use_sp_import_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make.ready4use sp import lookup table. The function is called for its side effects and does not return a value.
#' @param x An object
#' @return NULL
#' @rdname make.ready4use_sp_import_lup
#' @export 
#' @importFrom dplyr select
make.ready4use_sp_import_lup <- function (x) 
{
    ready4use_dv_import_lup(x %>% dplyr::select(names(ready4use_dv_import_lup())))
}
