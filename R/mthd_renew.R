#' Renew (update) values
#' @description renew.ready4use_dictionary() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 s3 class defining a data dictionary tibble. The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4use_dictionary`, a ready4 s3 class defining a data dictionary tibble.
#' @param var_nm_chr Variable name (a character vector), Default: 'NA'
#' @param var_ctg_chr Variable category (a character vector), Default: 'NA'
#' @param var_desc_chr Variable description (a character vector), Default: 'NA'
#' @param var_type_chr Variable type (a character vector), Default: 'NA'
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param new_cases_r3 New cases (a ready4 submodule), Default: NULL
#' @param new_ready4_dict_r3 New ready4 dictionary (a ready4 submodule), Default: deprecated()
#' @param slice_indcs_int Slice indices (an integer vector), Default: NA
#' @return x (An object)
#' @rdname renew-methods
#' @export
#' @importFrom lifecycle is_present deprecate_warn
#' @importFrom rlang current_env
#' @importFrom ready4 update_tb_r3 add_lups renew
renew.ready4use_dictionary <- function (x, var_nm_chr = NA_character_, var_ctg_chr = NA_character_,
    var_desc_chr = NA_character_, var_type_chr = NA_character_,
    filter_cdn_1L_chr = NA_character_, new_cases_r3 = NULL, new_ready4_dict_r3 = deprecated(),
    slice_indcs_int = NA_integer_)
{
    if (lifecycle::is_present(new_ready4_dict_r3)) {
        lifecycle::deprecate_warn("0.0.0.9211", "ready4use::renew.ready4use_dictionary(new_ready4_dict_r3)",
            details = "Please use `ready4use::renew.ready4use_dictionary(new_cases_r3)` instead.")
    }
    fn_env_ls <- as.list(rlang::current_env())[-1]
    x <- ready4::update_tb_r3(x, filter_cdn_1L_chr = filter_cdn_1L_chr,
        fn = renew.ready4use_dictionary, fn_env_ls = fn_env_ls,
        slice_indcs_int = slice_indcs_int)
    if (!is.null(new_cases_r3)) {
        x <- ready4::add_lups(x, new_lup = new_cases_r3, key_var_nm_1L_chr = "var_nm_chr")
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4use_dictionary-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4use_dictionary", package = "ready4use"), renew.ready4use_dictionary)
#' Renew (update) values
#' @description renew.ready4use_imports() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 submodule class for tibble object lookup table of sources of raw (un-processed) data to import. The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4use_imports`, a ready4 submodule class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param local_file_src_chr Local file source (a character vector), Default: 'NA'
#' @param path_to_make_script_chr Path to make script (a character vector), Default: 'NA'
#' @param download_url_chr Download url (a character vector), Default: 'NA'
#' @param inc_file_main_chr Include file main (a character vector), Default: 'NA'
#' @param inc_fls_to_rename_ls Include files to rename (a list), Default: list()
#' @param new_nms_for_inc_fls_ls New names for include files (a list), Default: list()
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param local_to_url_vec_chr Local to url vector (a character vector), Default: 'NA'
#' @param slice_indcs_int Slice indices (an integer vector), Default: NA
#' @param urls_vec_chr Urls vector (a character vector), Default: 'NA'
#' @return x (An object)
#' @rdname renew-methods
#' @export
#' @importFrom rlang current_env
#' @importFrom ready4 update_tb_r3 renew
#' @importFrom purrr reduce
renew.ready4use_imports <- function (x, local_file_src_chr = NA_character_, path_to_make_script_chr = NA_character_,
    download_url_chr = NA_character_, inc_file_main_chr = NA_character_,
    inc_fls_to_rename_ls = list(), new_nms_for_inc_fls_ls = list(),
    filter_cdn_1L_chr = NA_character_, local_to_url_vec_chr = NA_character_,
    slice_indcs_int = NA_integer_, urls_vec_chr = NA_character_)
{
    fn_env_ls <- as.list(rlang::current_env())[-1]
    x <- ready4::update_tb_r3(x, filter_cdn_1L_chr = filter_cdn_1L_chr,
        fn = renew.ready4use_imports, fn_env_ls = fn_env_ls,
        slice_indcs_int = slice_indcs_int)
    if (!is.na(local_to_url_vec_chr) & !is.na(urls_vec_chr))
        x <- purrr::reduce(1:length(local_to_url_vec_chr), .init = x,
            ~update_tb_src_loc_to_url_sngl_tb(x = .x, y = .y,
                local_to_url_vec_chr = local_to_url_vec_chr,
                urls_vec_chr = urls_vec_chr))
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4use_imports-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4use_imports", package = "ready4use"), renew.ready4use_imports)
#'
#' Renew (update) values
#' @name renew-Ready4useDyad
#' @description renew method applied to Ready4useDyad
#' @param x An object of class Ready4useDyad
#' @param arrange_by_1L_chr Arrange by (a character vector of length one), Default: c("category", "name", "both")
#' @param categories_chr Categories (a character vector), Default: character(0)
#' @param drop_chr Drop (a character vector), Default: character(0)
#' @param dictionary_lups_ls Dictionary lookup tables (a list), Default: list()
#' @param dictionary_r3 Dictionary (a ready4 submodule), Default: ready4use_dictionary()
#' @param dummys_ls Dummys (a list), Default: NULL
#' @param dyad_ls Dummys (a list), Default: NULL
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param factors_chr Factors (a character vector), Default: character(0)
#' @param fn Function (a function), Default: NULL
#' @param fn_args_ls Function arguments (a list), Default: NULL
#' @param lup_tb Lookup table (a tibble), Default: NULL
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one), Default: character(0)
#' @param method_1L_chr Method (a character vector of length one), Default: c("first", "sample")
#' @param names_chr Names (a character vector), Default: character(0)
#' @param new_val_xx New value (an output object of multiple potential types), Default: NULL
#' @param remove_old_lbls_1L_lgl Remove old labels (a logical vector of length one), Default: T
#' @param tfmn_1L_chr Transformation (a character vector of length one), Default: 'capitalise'
#' @param type_1L_chr Type (a character vector of length one), Default: c("label", "base", "case", "drop", "dummys", "join", "keep",
#'    "levels", "mutate", "new", "rbind", "unlabel", "update",
#'    "sequential", "batch", "self")
#' @param uid_var_nm_1L_chr Unique identifier variable name (a character vector of length one), Default: character(0)
#' @param var_ctg_chr Variable category (a character vector), Default: 'Uncategorised'
#' @param vars_chr Variables (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: c("all", "dataset", "dictionary")
#' @param ... Additional arguments
#' @return x (An object of class Ready4useDyad)
#' @rdname renew-methods
#' @aliases renew,Ready4useDyad-method
#' @export
#' @importFrom assertthat assert_that
#' @importFrom purrr map_lgl reduce map_chr
#' @importFrom ready4show is_ready4show_correspondences
#' @importFrom Hmisc capitalize
#' @importFrom stringr str_to_title
#' @importFrom ready4 remove_lbls_from_df get_from_lup_obj renew
#' @importFrom dplyr pull mutate
#' @importFrom rlang sym
#' @importFrom stringi stri_replace_first_fixed
methods::setMethod("renew", "Ready4useDyad", function (x, arrange_by_1L_chr = c("category", "name", "both"),
    categories_chr = character(0), drop_chr = character(0), dictionary_lups_ls = list(),
    dictionary_r3 = ready4use_dictionary(), dummys_ls = NULL, dyad_ls = NULL,
    exclude_chr = character(0), factors_chr = character(0), fn = NULL,
    fn_args_ls = NULL, lup_tb = NULL, match_var_nm_1L_chr = character(0),
    method_1L_chr = c("first", "sample"), names_chr = character(0),
    new_val_xx = NULL, remove_old_lbls_1L_lgl = T, tfmn_1L_chr = "capitalise",
    type_1L_chr = c("label", "base", "case", "drop", "dummys",
        "join", "keep", "levels", "mutate", "new", "rbind", "unlabel",
        "update", "sequential", "batch", "self"), uid_var_nm_1L_chr = character(0),
    var_ctg_chr = "Uncategorised", vars_chr = character(0), what_1L_chr = c("all",
        "dataset", "dictionary"), ...)
{
    arrange_by_1L_chr <- match.arg(arrange_by_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    assertthat::assert_that((is.list(dictionary_lups_ls) & (dictionary_lups_ls %>%
        purrr::map_lgl(~ready4show::is_ready4show_correspondences(.x)) %>%
        all())), msg = "dictionary_lups_ls must be comprised solely of elements that are ready4show_correspondences.")
    if (what_1L_chr %in% c("all", "dataset") & type_1L_chr %in%
        c("label", "base", "case", "dummys", "levels", "unlabel")) {
        if (type_1L_chr %in% c("label", "case")) {
            dictionary_tb <- x@dictionary_r3
            if (tfmn_1L_chr == "capitalise") {
                dictionary_tb$var_desc_chr <- dictionary_tb$var_desc_chr %>%
                  Hmisc::capitalize()
            }
            if (tfmn_1L_chr == "title") {
                dictionary_tb$var_desc_chr <- dictionary_tb$var_desc_chr %>%
                  stringr::str_to_title()
            }
        }
        if (type_1L_chr == "case") {
            x@dictionary_r3 <- dictionary_tb
        }
        if (type_1L_chr == "label") {
            tfd_ds_tb <- add_labels_from_dictionary(x@ds_tb,
                dictionary_tb = dictionary_tb %>% ready4::remove_lbls_from_df(),
                remove_old_lbls_1L_lgl = remove_old_lbls_1L_lgl)
            x@ds_tb <- tfd_ds_tb
        }
        if (type_1L_chr == "unlabel") {
            x@ds_tb <- remove_labels_from_ds(x@ds_tb)
        }
        if (type_1L_chr %in% c("base", "dummys", "levels")) {
            if (is.null(dummys_ls)) {
                dummys_ls <- new_val_xx
            }
            dummys_dict_r3 <- manufacture(x, dummys_ls = dummys_ls,
                flatten_1L_lgl = F, type_1L_chr = ifelse(type_1L_chr ==
                  "levels", "all", type_1L_chr), what_1L_chr = "factors-d")
            x@dictionary_r3 <- renew.ready4use_dictionary(x@dictionary_r3,
                new_cases_r3 = dummys_dict_r3)
            x@ds_tb <- purrr::reduce(dummys_dict_r3$var_ctg_chr %>%
                unique(), .init = x@ds_tb, ~{
                var_nm_1L_chr <- .y
                val_1_1L_chr <- if ("base" %in% ready4::get_from_lup_obj(dummys_dict_r3,
                  match_value_xx = .y, match_var_nm_1L_chr = "var_ctg_chr",
                  target_var_nm_1L_chr = "var_type_chr")) {
                  character(0)
                }
                else {
                  levels(.x %>% dplyr::pull(!!rlang::sym(.y)))[1]
                }
                .x %>% dplyr::mutate(`:=`(!!rlang::sym(.y), factor(!!rlang::sym(.y),
                  labels = c(val_1_1L_chr, ready4::get_from_lup_obj(dummys_dict_r3,
                    match_value_xx = .y, match_var_nm_1L_chr = "var_ctg_chr",
                    target_var_nm_1L_chr = "var_nm_chr") %>%
                    purrr::map_chr(~stringi::stri_replace_first_fixed(.x,
                      var_nm_1L_chr, ""))))))
            })
        }
    }
    else {
        if (!is.null(dyad_ls)) {
            dyad_ls <- list(x) %>% append(dyad_ls)
        }
    }
    if (what_1L_chr == "dictionary" & type_1L_chr == "new") {
        x <- add_dictionary(x, new_cases_r3 = dictionary_r3,
            var_ctg_chr = var_ctg_chr, arrange_by_1L_chr = ifelse(arrange_by_1L_chr ==
                "both", "category", arrange_by_1L_chr))
    }
    if (type_1L_chr %in% c("drop", "keep", "mutate", "sequential",
        "batch", "self") | (what_1L_chr == "dictionary" & type_1L_chr ==
        "update")) {
        x <- update_dyad(x, arrange_1L_chr = arrange_by_1L_chr,
            categories_chr = categories_chr, dictionary_lups_ls = dictionary_lups_ls,
            dictionary_r3 = dictionary_r3, fn = fn, fn_args_ls = fn_args_ls,
            exclude_chr = exclude_chr, lup_prototype_tb = lup_tb,
            match_var_nm_1L_chr = match_var_nm_1L_chr, method_1L_chr = method_1L_chr,
            names_chr = names_chr, type_1L_chr = type_1L_chr,
            vars_chr = vars_chr, what_1L_chr = what_1L_chr)
    }
    if (type_1L_chr == "join") {
        x <- purrr::reduce(dyad_ls, ~add_with_join(.x, .y))
    }
    if (type_1L_chr == "rbind") {
        if (is.null(fn)) {
            tfmn_fn <- identity
        }
        else {
            tfmn_fn <- fn
        }
        x <- bind_dyads(dyad_ls, drop_chr = drop_chr, factors_chr = factors_chr,
            tfmn_fn = tfmn_fn, uid_var_nm_1L_chr = uid_var_nm_1L_chr)
    }
    return(x)
})
