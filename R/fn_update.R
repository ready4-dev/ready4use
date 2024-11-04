#' Update character variables
#' @description update_character_vars() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update character variables. The function returns Dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @param var_nms_chr Variable names (a character vector)
#' @param as_missing_chr As missing (a character vector), Default: character(0)
#' @param missing_1L_chr Missing (a character vector of length one), Default: 'NA'
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: character(0)
#' @param remove_end_chr Remove end (a character vector), Default: character(0)
#' @param remove_start_chr Remove start (a character vector), Default: character(0)
#' @param replacement_fn_ls Replacement (a list of functions), Default: list(end = stringi::stri_replace_last_fixed, start = stringi::stri_replace_first_fixed)
#' @param x_ready4show_correspondences PARAM_DESCRIPTION, Default: ready4show::ready4show_correspondences()
#' @return Dataset (a tibble)
#' @rdname update_character_vars
#' @export 
#' @importFrom stringi stri_replace_last_fixed stri_replace_first_fixed
#' @importFrom ready4show ready4show_correspondences
#' @importFrom dplyr mutate across
#' @importFrom purrr reduce map_chr
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
update_character_vars <- function (ds_tb, var_nms_chr, as_missing_chr = character(0), 
    missing_1L_chr = NA_character_, prefix_1L_chr = character(0), 
    remove_end_chr = character(0), remove_start_chr = character(0), 
    replacement_fn_ls = list(end = stringi::stri_replace_last_fixed, 
        start = stringi::stri_replace_first_fixed), x_ready4show_correspondences = ready4show::ready4show_correspondences()) 
{
    if (!identical(prefix_1L_chr, character(0))) {
        ds_tb <- ds_tb %>% dplyr::mutate(dplyr::across(var_nms_chr, 
            ~.x, .names = paste0(prefix_1L_chr, "_{.col}")))
        var_nms_chr <- paste0(prefix_1L_chr, "_", var_nms_chr)
    }
    if (!identical(as_missing_chr, character(0))) {
        ds_tb <- ds_tb %>% dplyr::mutate(dplyr::across(var_nms_chr, 
            ~ifelse(.x %in% as_missing_chr, missing_1L_chr, .x)))
    }
    if (!identical(remove_end_chr, character(0))) {
        ds_tb <- remove_end_chr %>% purrr::reduce(.init = ds_tb, 
            ~{
                pattern_1L_chr <- .y
                .x %>% dplyr::mutate(dplyr::across(var_nms_chr, 
                  ~ifelse(endsWith(.x, pattern_1L_chr), replacement_fn_ls$end(.x, 
                    pattern_1L_chr, ""), .x)))
            })
    }
    if (!identical(remove_start_chr, character(0))) {
        ds_tb <- remove_start_chr %>% purrr::reduce(.init = ds_tb, 
            ~{
                pattern_1L_chr <- .y
                .x %>% dplyr::mutate(dplyr::across(var_nms_chr, 
                  ~ifelse(startsWith(.x, pattern_1L_chr), replacement_fn_ls$start(.x, 
                    pattern_1L_chr, ""), .x)))
            })
    }
    if (!identical(x_ready4show_correspondences, ready4show::ready4show_correspondences())) {
        ds_tb <- ds_tb %>% dplyr::mutate(dplyr::across(var_nms_chr, 
            ~.x %>% purrr::map_chr(~ifelse(.x %in% x_ready4show_correspondences$old_nms_chr, 
                ready4::get_from_lup_obj(x_ready4show_correspondences, 
                  match_var_nm_1L_chr = "old_nms_chr", match_value_xx = .x, 
                  target_var_nm_1L_chr = "new_nms_chr"), .x))))
    }
    return(ds_tb)
}
#' Update column names
#' @description update_column_names() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update column names. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param patterns_ls Patterns (a list), Default: list(c("[[:space:]]", ""))
#' @param update_desc_1L_lgl Update description (a logical vector of length one), Default: FALSE
#' @return X (A dataset and data dictionary pair.)
#' @rdname update_column_names
#' @export 
#' @importFrom purrr reduce
#' @importFrom stringr str_replace_all
update_column_names <- function (X_Ready4useDyad, patterns_ls = list(c("[[:space:]]", 
    "")), update_desc_1L_lgl = FALSE) 
{
    X_Ready4useDyad <- purrr::reduce(patterns_ls, .init = X_Ready4useDyad, 
        ~{
            ds_tb <- .x@ds_tb
            names(ds_tb) <- names(ds_tb) %>% stringr::str_replace_all(.y[1], 
                .y[2])
            dict_r3 <- .x@dictionary_r3
            dict_r3$var_nm_chr <- dict_r3$var_nm_chr %>% stringr::str_replace_all(.y[1], 
                .y[2])
            if (update_desc_1L_lgl) {
                dict_r3$var_desc_chr <- dict_r3$var_desc_chr %>% 
                  stringr::str_replace_all(.y[1], .y[2])
            }
            Ready4useDyad(ds_tb = ds_tb, dictionary_r3 = dict_r3, 
                dissemination_1L_chr = X_Ready4useDyad@dissemination_1L_chr)
        })
    return(X_Ready4useDyad)
}
#' Update correspondences
#' @description update_correspondences() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update correspondences. The function returns Correspondences (a list).
#' @param correspondences_ls Correspondences (a list)
#' @param dyad_ls Dyad (a list), Default: NULL
#' @param new_ls New (a list), Default: NULL
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: '!is.na(new_nms_chr)'
#' @param range_int Range (an integer vector), Default: 1L:12L
#' @param reference_1L_int Reference (an integer vector of length one), Default: 2
#' @param spaced_1L_lgl Spaced (a logical vector of length one), Default: TRUE
#' @param type_1L_chr Type (a character vector of length one), Default: 'sequence'
#' @param units_chr Units (a character vector), Default: c("minute", "hour", "week", "month", "year")
#' @return Correspondences (a list)
#' @rdname update_correspondences
#' @export 
#' @importFrom purrr map2 discard_at map_chr discard map_dfr map
#' @importFrom dplyr filter mutate
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
update_correspondences <- function (correspondences_ls, dyad_ls = NULL, new_ls = NULL, 
    filter_cdn_1L_chr = "!is.na(new_nms_chr)", range_int = 1L:12L, 
    reference_1L_int = 2L, spaced_1L_lgl = TRUE, type_1L_chr = "sequence", 
    units_chr = c("minute", "hour", "week", "month", "year")) 
{
    if (!is.null(dyad_ls)) {
        if (type_1L_chr == "sequence") {
            correspondences_ls <- correspondences_ls %>% purrr::map2(if (length(correspondences_ls) < 
                length(dyad_ls)) {
                dyad_ls %>% purrr::discard_at(reference_1L_int)
            }
            else {
                dyad_ls
            }, ~{
                x <- .x
                Y <- .y
                new_chr <- x$new_nms_chr
                descs_chr <- x$old_nms_chr %>% purrr::map_chr(~ifelse(.x %in% 
                  new_chr, .x, NA_character_)) %>% purrr::discard(is.na)
                if (!identical(descs_chr, character(0))) {
                  x <- x %>% dplyr::filter(new_nms_chr %in% descs_chr)
                  z <- x$old_nms_chr %>% purrr::map_dfr(~{
                    z <- ready4show::ready4show_correspondences()
                    if (.x %in% dyad_ls[[reference_1L_int]]@dictionary_r3$var_desc_chr) {
                      z <- ready4show::renew.ready4show_correspondences(z, 
                        old_nms_chr = ready4::get_from_lup_obj(Y@dictionary_r3, 
                          match_value_xx = .x, match_var_nm_1L_chr = "var_desc_chr", 
                          target_var_nm_1L_chr = "var_nm_chr"), 
                        new_nms_chr = ready4::get_from_lup_obj(dyad_ls[[reference_1L_int]]@dictionary_r3, 
                          match_value_xx = .x, match_var_nm_1L_chr = "var_desc_chr", 
                          target_var_nm_1L_chr = "var_nm_chr"))
                    }
                    z
                  })
                }
                else {
                  z <- ready4show::ready4show_correspondences()
                }
                missing_chr <- setdiff(z$new_nms_chr, z$old_nms_chr)
                if (!identical(missing_chr, character(0))) {
                  y <- missing_chr %>% purrr::map_dfr(~{
                    desc_1L_chr <- ready4::get_from_lup_obj(Y@dictionary_r3, 
                      match_value_xx = .x, match_var_nm_1L_chr = "var_nm_chr", 
                      target_var_nm_1L_chr = "var_desc_chr")
                    match_1L_chr <- ready4::get_from_lup_obj(dyad_ls[[reference_1L_int]]@dictionary_r3, 
                      match_value_xx = desc_1L_chr, match_var_nm_1L_chr = "var_nm_chr", 
                      target_var_nm_1L_chr = "var_desc_chr")
                    ready4show::renew.ready4show_correspondences(ready4show::ready4show_correspondences(), 
                      new_nms_chr = ifelse(identical(match_1L_chr, 
                        character(0)), NA_character_, match_1L_chr), 
                      old_nms_chr = .x)
                  })
                  z <- rbind(z, y)
                }
                missing_chr <- setdiff(z$old_nms_chr, z$new_nms_chr)
                if (!identical(missing_chr, character(0))) {
                  y <- missing_chr %>% purrr::map_dfr(~{
                    desc_1L_chr <- ready4::get_from_lup_obj(dyad_ls[[reference_1L_int]]@dictionary_r3, 
                      match_value_xx = .x, match_var_nm_1L_chr = "var_nm_chr", 
                      target_var_nm_1L_chr = "var_desc_chr")
                    match_1L_chr <- ready4::get_from_lup_obj(Y@dictionary_r3, 
                      match_value_xx = desc_1L_chr, match_var_nm_1L_chr = "var_desc_chr", 
                      target_var_nm_1L_chr = "var_nm_chr")
                    ready4show::renew.ready4show_correspondences(ready4show::ready4show_correspondences(), 
                      old_nms_chr = ifelse(identical(match_1L_chr, 
                        character(0)), NA_character_, match_1L_chr), 
                      new_nms_chr = .x)
                  })
                  z <- rbind(z, y)
                }
                z
            })
        }
        if (type_1L_chr %in% c("multiple", "interval")) {
            reference_descs_chr <- get_reference_descs(correspondences_ls)
            reference_nms_chr <- reference_descs_chr %>% purrr::map_chr(~ready4::get_from_lup_obj(dyad_ls[[reference_1L_int]]@dictionary_r3, 
                match_value_xx = .x, match_var_nm_1L_chr = "var_desc_chr", 
                target_var_nm_1L_chr = "var_nm_chr"))
            if (length(correspondences_ls) < length(dyad_ls)) {
                dyad_ls <- dyad_ls %>% purrr::discard_at(reference_1L_int)
            }
            if (type_1L_chr == "interval") {
                x <- make_period_correspondences(reference_descs_chr, 
                  range_int = range_int, spaced_1L_lgl = spaced_1L_lgl, 
                  units_chr = units_chr)
                correspondences_ls <- correspondences_ls %>% 
                  purrr::map(~{
                    if (!identical(.x, ready4show::ready4show_correspondences())) {
                      y <- .x %>% dplyr::mutate(new_nms_chr = purrr::map_chr(.x$new_nms_chr, 
                        ~ready4::get_from_lup_obj(x, match_value_xx = .x, 
                          match_var_nm_1L_chr = "old_nms_chr", 
                          target_var_nm_1L_chr = "new_nms_chr")))
                      rbind(x %>% dplyr::filter(!new_nms_chr %in% 
                        y$new_nms_chr), y)
                    }
                    else {
                      x
                    }
                  })
            }
            if (type_1L_chr == "multiple") {
                x <- ready4show::ready4show_correspondences() %>% 
                  ready4show::renew.ready4show_correspondences(old_nms_chr = reference_nms_chr, 
                    new_nms_chr = paste0(reference_nms_chr, "_", 
                      LETTERS[reference_1L_int]))
                correspondences_ls <- correspondences_ls %>% 
                  purrr::map2(1:length(correspondences_ls), ~{
                    index_1L_int <- .y
                    Y <- dyad_ls[[index_1L_int]]
                    if (!identical(.x, ready4show::ready4show_correspondences())) {
                      names_chr <- .x$old_nms_chr %>% purrr::map_chr(~ready4::get_from_lup_obj(Y@dictionary_r3, 
                        match_value_xx = .x, match_var_nm_1L_chr = "var_desc_chr", 
                        target_var_nm_1L_chr = "var_nm_chr"))
                      y <- ready4show::ready4show_correspondences() %>% 
                        ready4show::renew.ready4show_correspondences(old_nms_chr = names_chr, 
                          new_nms_chr = paste0(names_chr, "_", 
                            LETTERS[index_1L_int]))
                      rbind(x %>% dplyr::filter(!old_nms_chr %in% 
                        y$old_nms_chr), y)
                    }
                    else {
                      x
                    }
                  })
            }
        }
    }
    if (!is.null(new_ls)) {
        correspondences_ls <- purrr::map2(correspondences_ls, 
            new_ls, ~{
                if (!identical(.y, ready4show_correspondences())) {
                  ready4show::renew.ready4show_correspondences(.x, 
                    old_nms_chr = .y$old_nms_chr, new_nms_chr = .y$new_nms_chr, 
                    filter_cdn_1L_chr = filter_cdn_1L_chr)
                }
                else {
                  .x
                }
            })
    }
    return(correspondences_ls)
}
#' Update data dictionary
#' @description update_data_dict() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update data dictionary. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: Ready4useDyad()
#' @param dictionary_lups_ls Dictionary lookup tables (a list), Default: list()
#' @param arrange_by_1L_chr Arrange by (a character vector of length one), Default: c("category", "var_ctg_chr", "name", "var_nm_chr", "both", "var_ctg_chr, var_nm_chr")
#' @return X (A dataset and data dictionary pair.)
#' @rdname update_data_dict
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom purrr map_lgl reduce
#' @importFrom ready4show is_ready4show_correspondences manufacture.ready4show_correspondences
#' @importFrom dplyr mutate arrange filter
#' @importFrom rlang sym
#' @keywords internal
update_data_dict <- function (X_Ready4useDyad = Ready4useDyad(), dictionary_lups_ls = list(), 
    arrange_by_1L_chr = c("category", "var_ctg_chr", "name", 
        "var_nm_chr", "both", "var_ctg_chr, var_nm_chr")) 
{
    assertthat::assert_that((is.list(dictionary_lups_ls) & (dictionary_lups_ls %>% 
        purrr::map_lgl(~ready4show::is_ready4show_correspondences(.x)) %>% 
        all())), msg = "dictionary_lups_ls must be comprised solely of elements that are ready4show_correspondences.")
    arrange_by_1L_chr <- match.arg(arrange_by_1L_chr)
    arrange_by_1L_chr <- ifelse(arrange_by_1L_chr == "var_ctg_chr", 
        "category", ifelse(arrange_by_1L_chr == "var_nm_chr", 
            "name", ifelse(arrange_by_1L_chr %in% c("both", "var_ctg_chr, var_nm_chr"), 
                "category", arrange_by_1L_chr)))
    if (!identical(dictionary_lups_ls, list())) {
        X_Ready4useDyad <- 1:length(dictionary_lups_ls) %>% purrr::reduce(.init = X_Ready4useDyad, 
            ~{
                var_1L_chr <- names(dictionary_lups_ls)[.y]
                values_lup <- dictionary_lups_ls[[.y]]
                values_chr <- ready4show::manufacture.ready4show_correspondences(values_lup, 
                  .x@dictionary_r3$var_nm_chr, flatten_1L_lgl = T)
                renewSlot(.x, "dictionary_r3", .x@dictionary_r3 %>% 
                  dplyr::mutate(`:=`(!!rlang::sym(var_1L_chr), 
                    values_chr)))
            })
    }
    X_Ready4useDyad@dictionary_r3 <- X_Ready4useDyad@dictionary_r3 %>% 
        dplyr::arrange(!!rlang::sym(ifelse(arrange_by_1L_chr == 
            "name", "var_nm_chr", "var_ctg_chr")), !!rlang::sym(ifelse(arrange_by_1L_chr == 
            "name", "var_ctg_chr", "var_nm_chr")))
    X_Ready4useDyad@dictionary_r3 <- X_Ready4useDyad@dictionary_r3 %>% 
        dplyr::filter(var_nm_chr %in% names(X_Ready4useDyad@ds_tb))
    return(X_Ready4useDyad)
}
#' Update dyad
#' @description update_dyad() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update dyad. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param arrange_1L_chr Arrange (a character vector of length one), Default: c("var_ctg_chr, var_nm_chr", "category", "name", "both", "var_ctg_chr", 
#'    "var_nm_chr")
#' @param categories_chr Categories (a character vector), Default: character(0)
#' @param dictionary_lups_ls Dictionary lookup tables (a list), Default: list()
#' @param dictionary_r3 Dictionary (a ready4 submodule), Default: ready4use_dictionary()
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param fn Function (a function), Default: NULL
#' @param fn_args_ls Function arguments (a list), Default: NULL
#' @param lup_prototype_tb Lookup table prototype (a tibble), Default: NULL
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one), Default: character(0)
#' @param method_1L_chr Method (a character vector of length one), Default: c("first", "sample")
#' @param names_chr Names (a character vector), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("keep", "drop", "mutate", "update", "sequential", "batch", 
#'    "self")
#' @param vars_chr Variables (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: c("all", "dataset", "dictionary")
#' @return X (A dataset and data dictionary pair.)
#' @rdname update_dyad
#' @export 
#' @importFrom dplyr mutate filter pull select bind_rows
#' @importFrom purrr discard_at reduce
#' @importFrom rlang sym exec
#' @importFrom tidyselect all_of
#' @keywords internal
update_dyad <- function (X_Ready4useDyad, arrange_1L_chr = c("var_ctg_chr, var_nm_chr", 
    "category", "name", "both", "var_ctg_chr", "var_nm_chr"), 
    categories_chr = character(0), dictionary_lups_ls = list(), 
    dictionary_r3 = ready4use_dictionary(), exclude_chr = character(0), 
    fn = NULL, fn_args_ls = NULL, lup_prototype_tb = NULL, match_var_nm_1L_chr = character(0), 
    method_1L_chr = c("first", "sample"), names_chr = character(0), 
    type_1L_chr = c("keep", "drop", "mutate", "update", "sequential", 
        "batch", "self"), vars_chr = character(0), what_1L_chr = c("all", 
        "dataset", "dictionary")) 
{
    arrange_1L_chr <- match.arg(arrange_1L_chr)
    arrange_1L_chr <- ifelse(arrange_1L_chr == "category", "var_ctg_chr", 
        ifelse(arrange_1L_chr == "name", "var_nm_chr", ifelse(arrange_1L_chr == 
            "both", "var_ctg_chr, var_nm_chr", arrange_1L_chr)))
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr %in% c("all", "dataset")) {
        if (type_1L_chr == "mutate") {
            fn <- dplyr::mutate
        }
        if (type_1L_chr %in% c("sequential", "batch", "self")) {
            fn <- add_from_lup_prototype
            if (is.null(fn_args_ls)) {
                fn_args_ls <- list
            }
            fn_args_ls <- append(fn_args_ls, list(exclude_chr = exclude_chr, 
                lup_prototype_tb = lup_prototype_tb, match_var_nm_1L_chr = match_var_nm_1L_chr, 
                method_1L_chr = method_1L_chr, type_1L_chr = type_1L_chr, 
                vars_chr = vars_chr) %>% purrr::discard_at(names(fn_args_ls)))
        }
        if (!is.null(fn)) {
            if (identical(fn, dplyr::mutate)) {
                X_Ready4useDyad@ds_tb <- purrr::reduce(1:length(fn_args_ls), 
                  .init = X_Ready4useDyad@ds_tb, ~fn(.x, `:=`(!!rlang::sym(names(fn_args_ls)[.y]), 
                    eval(parse(text = fn_args_ls[[.y]])))))
            }
            else {
                X_Ready4useDyad@ds_tb <- rlang::exec(fn, X_Ready4useDyad@ds_tb, 
                  !!!fn_args_ls)
            }
        }
        if (type_1L_chr %in% c("keep", "drop")) {
            names_chr <- c(names_chr, X_Ready4useDyad@dictionary_r3 %>% 
                dplyr::filter(var_ctg_chr %in% categories_chr) %>% 
                dplyr::pull(var_nm_chr))
            if (type_1L_chr == "keep") {
                names_chr <- names(X_Ready4useDyad@ds_tb)[names(X_Ready4useDyad@ds_tb) %in% 
                  names_chr]
            }
            else {
                names_chr <- names(X_Ready4useDyad@ds_tb)[!names(X_Ready4useDyad@ds_tb) %in% 
                  names_chr]
            }
            X_Ready4useDyad@ds_tb <- dplyr::select(X_Ready4useDyad@ds_tb, 
                tidyselect::all_of(names_chr))
        }
    }
    if (what_1L_chr %in% c("all", "dictionary")) {
        if (!identical(dictionary_r3, ready4use_dictionary())) {
            X_Ready4useDyad@dictionary_r3 <- dplyr::bind_rows(X_Ready4useDyad@dictionary_r3 %>% 
                dplyr::filter(!var_nm_chr %in% dictionary_r3$var_nm_chr), 
                dictionary_r3)
            X_Ready4useDyad@dictionary_r3 <- eval(parse(text = paste0("dplyr::arrange(X_Ready4useDyad@dictionary_r3,", 
                arrange_1L_chr, ")")))
        }
        if (!identical(dictionary_lups_ls, list())) {
            X_Ready4useDyad <- update_data_dict(X_Ready4useDyad, 
                arrange_by_1L_chr = arrange_1L_chr, dictionary_lups_ls = dictionary_lups_ls)
        }
        X_Ready4useDyad@dictionary_r3 <- X_Ready4useDyad@dictionary_r3 %>% 
            dplyr::filter(var_nm_chr %in% names(X_Ready4useDyad@ds_tb))
    }
    return(X_Ready4useDyad)
}
#' Update dyad list
#' @description update_dyad_ls() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update dyad list. The function returns Dyad (a list).
#' @param dyad_ls Dyad (a list)
#' @param add_lups_1L_lgl Add lookup tables (a logical vector of length one), Default: F
#' @param arrange_1L_chr Arrange (a character vector of length one), Default: c("var_ctg_chr, var_nm_chr")
#' @param factors_chr Factors (a character vector), Default: character(0)
#' @param range_int Range (an integer vector), Default: 1L:12L
#' @param recode_ls Recode (a list), Default: NULL
#' @param reference_1L_int Reference (an integer vector of length one), Default: 2
#' @param spaced_1L_lgl Spaced (a logical vector of length one), Default: TRUE
#' @param standard_spaces_1L_lgl Standard spaces (a logical vector of length one), Default: F
#' @param tfmn_cls_1L_chr Transformation class (a character vector of length one), Default: 'character'
#' @param tfmns_ls Transformations (a list), Default: list(bind = identity, class = as.character)
#' @param type_1L_chr Type (a character vector of length one), Default: c("sequence", "composite", "bind", "class", "default", "interval", 
#'    "reference")
#' @param units_chr Units (a character vector), Default: c("minute", "hour", "week", "month", "year")
#' @param uid_var_nm_1L_chr Unique identifier variable name (a character vector of length one), Default: character(0)
#' @return Dyad (a list)
#' @rdname update_dyad_ls
#' @export 
#' @importFrom purrr map2 map_chr map map_lgl discard_at map_dfr reduce
#' @importFrom dplyr mutate case_when rename filter select distinct pull
#' @importFrom ready4 get_from_lup_obj
#' @importFrom stats setNames
#' @importFrom tidyselect any_of
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences
#' @importFrom rlang sym
#' @importFrom stringr str_replace_all
#' @keywords internal
update_dyad_ls <- function (dyad_ls, add_lups_1L_lgl = F, arrange_1L_chr = c("var_ctg_chr, var_nm_chr"), 
    factors_chr = character(0), range_int = 1L:12L, recode_ls = NULL, 
    reference_1L_int = 2L, spaced_1L_lgl = TRUE, standard_spaces_1L_lgl = F, 
    tfmn_cls_1L_chr = "character", tfmns_ls = list(bind = identity, 
        class = as.character), type_1L_chr = c("sequence", "composite", 
        "bind", "class", "default", "interval", "reference"), 
    units_chr = c("minute", "hour", "week", "month", "year"), 
    uid_var_nm_1L_chr = character(0)) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    append_ls <- NULL
    if (!is.null(recode_ls)) {
        if (type_1L_chr %in% c("interval", "reference")) {
            if (type_1L_chr == "interval" && add_lups_1L_lgl) {
                append_ls <- list(temporal_lup = make_temporal_lup(dyad_ls, 
                  recode_ls = recode_ls))
            }
            dyad_ls <- recode_ls %>% purrr::map2(dyad_ls, ~{
                Y <- .y
                if (!identical(.x, ready4show_correspondences())) {
                  x <- .x
                  Y@dictionary_r3 <- Y@dictionary_r3 %>% dplyr::mutate(var_desc_chr = dplyr::case_when(.data$var_desc_chr %in% 
                    x$old_nms_chr ~ .data$var_desc_chr %>% purrr::map_chr(~ifelse(.x %in% 
                    x$old_nms_chr, ready4::get_from_lup_obj(x, 
                    match_var_nm_1L_chr = "old_nms_chr", match_value_xx = .x, 
                    target_var_nm_1L_chr = "new_nms_chr"), .x)), 
                    T ~ .data$var_desc_chr))
                }
                Y
            }) %>% stats::setNames(names(dyad_ls))
            if (type_1L_chr == "interval" && add_lups_1L_lgl) {
                dyad_ls <- append(dyad_ls, append_ls)
            }
        }
        if (type_1L_chr == "sequence") {
            dyad_ls <- recode_ls %>% purrr::map2(dyad_ls, ~{
                Y <- .y
                if (!identical(.x, ready4show_correspondences())) {
                  x <- .x
                  Y@ds_tb <- Y@ds_tb %>% dplyr::rename(tidyselect::any_of(x$old_nms_chr %>% 
                    stats::setNames(x$new_nms_chr)))
                  Y@dictionary_r3 <- Y@dictionary_r3 %>% dplyr::mutate(var_nm_chr = dplyr::case_when(.data$var_nm_chr %in% 
                    x$old_nms_chr ~ .data$var_nm_chr %>% purrr::map_chr(~ifelse(.x %in% 
                    x$old_nms_chr, ready4::get_from_lup_obj(x, 
                    match_var_nm_1L_chr = "old_nms_chr", match_value_xx = .x, 
                    target_var_nm_1L_chr = "new_nms_chr"), .x)), 
                    T ~ .data$var_nm_chr))
                }
                Y
            }) %>% stats::setNames(names(dyad_ls))
        }
    }
    else {
        if (type_1L_chr == "composite") {
            dyad_ls <- update_dyad_ls(dyad_ls, arrange_1L_chr = arrange_1L_chr, 
                reference_1L_int = reference_1L_int, standard_spaces_1L_lgl = standard_spaces_1L_lgl, 
                type_1L_chr = "sequence")
            dyad_ls <- update_dyad_ls(dyad_ls, add_lups_1L_lgl = add_lups_1L_lgl, 
                arrange_1L_chr = arrange_1L_chr, units_chr = units_chr, 
                range_int = range_int, reference_1L_int = reference_1L_int, 
                standard_spaces_1L_lgl = standard_spaces_1L_lgl, 
                tfmn_cls_1L_chr = tfmn_cls_1L_chr, tfmns_ls = list(class = identity), 
                type_1L_chr = "interval")
            dyad_ls <- update_dyad_ls(dyad_ls, add_lups_1L_lgl = add_lups_1L_lgl, 
                arrange_1L_chr = arrange_1L_chr, reference_1L_int = reference_1L_int, 
                standard_spaces_1L_lgl = standard_spaces_1L_lgl, 
                tfmn_cls_1L_chr = tfmn_cls_1L_chr, tfmns_ls = list(class = identity), 
                type_1L_chr = "default")
            dyad_ls <- update_dyad_ls(dyad_ls, add_lups_1L_lgl = add_lups_1L_lgl, 
                arrange_1L_chr = arrange_1L_chr, reference_1L_int = reference_1L_int, 
                standard_spaces_1L_lgl = standard_spaces_1L_lgl, 
                tfmn_cls_1L_chr = tfmn_cls_1L_chr, tfmns_ls = tfmns_ls, 
                type_1L_chr = "class")
            dyad_ls <- update_dyad_ls(dyad_ls, add_lups_1L_lgl = add_lups_1L_lgl, 
                arrange_1L_chr = arrange_1L_chr, factors_chr = factors_chr, 
                reference_1L_int = reference_1L_int, standard_spaces_1L_lgl = standard_spaces_1L_lgl, 
                tfmns_ls = tfmns_ls, type_1L_chr = "bind", uid_var_nm_1L_chr = uid_var_nm_1L_chr)
        }
        if (type_1L_chr %in% c("sequence")) {
            recode_ls <- make_correspondences(dyad_ls, names_1L_lgl = T, 
                reference_1L_int = reference_1L_int)
            dyad_ls <- update_dyad_ls(dyad_ls, recode_ls = recode_ls, 
                standard_spaces_1L_lgl = standard_spaces_1L_lgl, 
                type_1L_chr = "sequence")
        }
        if (type_1L_chr %in% c("interval")) {
            multiples_ls <- make_correspondences(dyad_ls, names_1L_lgl = F, 
                reference_1L_int = reference_1L_int)
            correspondences_r3 <- make_period_correspondences(get_reference_descs(multiples_ls), 
                range_int = range_int, spaced_1L_lgl = spaced_1L_lgl, 
                units_chr = units_chr)
            exclude_ls <- multiples_ls %>% purrr::map(~.x %>% 
                dplyr::filter(!new_nms_chr %in% correspondences_r3$old_nms_chr))
            multiples_ls <- multiples_ls %>% purrr::map(~.x %>% 
                dplyr::filter(new_nms_chr %in% correspondences_r3$old_nms_chr))
            period_correspondences_ls <- multiples_ls %>% update_correspondences(dyad_ls = dyad_ls, 
                range_int = range_int, type_1L_chr = "interval", 
                units_chr = units_chr)
            mismatches_ls <- 1:length(multiples_ls) %>% purrr::map(~{
                x <- multiples_ls[[.x]]
                y <- period_correspondences_ls[[.x]]
                z <- make_period_correspondences(x$old_nms_chr, 
                  range_int = range_int, spaced_1L_lgl = spaced_1L_lgl, 
                  units_chr = units_chr)
                dplyr::mutate(x, keep_1L_lgl = x$old_nms_chr %>% 
                  purrr::map_lgl(~{
                    ready4::get_from_lup_obj(z, match_var_nm_1L_chr = "old_nms_chr", 
                      match_value_xx = .x, target_var_nm_1L_chr = "new_nms_chr") == 
                      ready4::get_from_lup_obj(y, match_var_nm_1L_chr = "old_nms_chr", 
                        match_value_xx = .x, target_var_nm_1L_chr = "new_nms_chr")
                  })) %>% dplyr::filter(!keep_1L_lgl) %>% dplyr::select((-keep_1L_lgl))
            }) %>% stats::setNames(names(multiples_ls))
            period_correspondences_ls <- period_correspondences_ls %>% 
                purrr::map2(mismatches_ls, ~{
                  x <- .x
                  y <- .y
                  if (!identical(y, ready4show::ready4show_correspondences())) {
                    z <- make_period_correspondences(y$old_nms_chr, 
                      range_int = range_int, spaced_1L_lgl = spaced_1L_lgl, 
                      units_chr = units_chr)
                    x <- x %>% dplyr::mutate(new_nms_chr = dplyr::case_when(.data$old_nms_chr %in% 
                      y$old_nms_chr ~ .data$old_nms_chr %>% purrr::map_chr(~ifelse(.x %in% 
                      z$old_nms_chr, ready4::get_from_lup_obj(z, 
                      match_var_nm_1L_chr = "old_nms_chr", match_value_xx = .x, 
                      target_var_nm_1L_chr = "new_nms_chr"), 
                      NA_character_)), T ~ new_nms_chr))
                  }
                  else {
                    x
                  }
                })
            recode_ls <- 1:length(mismatches_ls) %>% purrr::map(~{
                x <- mismatches_ls[[.x]]
                Y <- dyad_ls[[.x]]
                z <- period_correspondences_ls[[.x]]
                if (!identical(x, ready4show::ready4show_correspondences())) {
                  x <- ready4show::ready4show_correspondences() %>% 
                    ready4show::renew.ready4show_correspondences(old_nms_chr = x$old_nms_chr %>% 
                      purrr::map_chr(~ready4::get_from_lup_obj(Y@dictionary_r3, 
                        match_var_nm_1L_chr = "var_desc_chr", 
                        match_value_xx = .x, target_var_nm_1L_chr = "var_nm_chr")), 
                      new_nms_chr = x$old_nms_chr %>% purrr::map_chr(~{
                        standardised_1L_chr <- ready4::get_from_lup_obj(z, 
                          match_var_nm_1L_chr = "old_nms_chr", 
                          match_value_xx = .x, target_var_nm_1L_chr = "new_nms_chr")
                        new_desc_1L_chr <- ready4::get_from_lup_obj(period_correspondences_ls[[reference_1L_int]], 
                          match_var_nm_1L_chr = "new_nms_chr", 
                          match_value_xx = standardised_1L_chr, 
                          target_var_nm_1L_chr = "old_nms_chr")
                        ready4::get_from_lup_obj(dyad_ls[[reference_1L_int]]@dictionary_r3, 
                          match_var_nm_1L_chr = "var_desc_chr", 
                          match_value_xx = new_desc_1L_chr, target_var_nm_1L_chr = "var_nm_chr")
                      }))
                }
                else {
                  x
                }
            })
            dyad_ls <- update_dyad_ls(dyad_ls, recode_ls = recode_ls, 
                standard_spaces_1L_lgl = standard_spaces_1L_lgl, 
                tfmn_cls_1L_chr = tfmn_cls_1L_chr, tfmns_ls = tfmns_ls, 
                type_1L_chr = "sequence")
            dyad_ls <- update_dyad_ls(dyad_ls, add_lups_1L_lgl = add_lups_1L_lgl, 
                recode_ls = period_correspondences_ls, standard_spaces_1L_lgl = standard_spaces_1L_lgl, 
                tfmn_cls_1L_chr = tfmn_cls_1L_chr, tfmns_ls = tfmns_ls, 
                type_1L_chr = "interval")
        }
    }
    if (!is.null(dyad_ls$temporal_lup)) {
        append_ls <- list(temporal_lup = dyad_ls$temporal_lup)
        dyad_ls <- dyad_ls %>% purrr::discard_at("temporal_lup")
    }
    if (type_1L_chr %in% c("interval")) {
        dyad_ls <- update_dyad_ls(dyad_ls, standard_spaces_1L_lgl = standard_spaces_1L_lgl, 
            tfmn_cls_1L_chr = tfmn_cls_1L_chr, tfmns_ls = tfmns_ls, 
            type_1L_chr = "class")
    }
    if (type_1L_chr == "bind") {
        dyad_ls <- list(X = bind_dyads(dyad_ls, factors_chr = factors_chr, 
            tfmn_fn = tfmns_ls$bind, uid_var_nm_1L_chr = uid_var_nm_1L_chr))
    }
    if (type_1L_chr == "class") {
        combined_tb <- dyad_ls %>% purrr::map_dfr(~.x@dictionary_r3 %>% 
            dplyr::select(var_nm_chr, var_type_chr)) %>% dplyr::distinct()
        combined_tb <- eval(parse(text = paste0("dplyr::arrange(combined_tb,", 
            "var_nm_chr", ")")))
        names_chr <- combined_tb %>% dplyr::pull(var_nm_chr)
        duplicated_tb <- combined_tb %>% dplyr::filter(var_nm_chr %in% 
            names_chr[duplicated(names_chr)])
        dyad_ls <- dyad_ls %>% purrr::map(~{
            Y <- .x
            duplicated_tb$var_nm_chr %>% unique() %>% purrr::reduce(.init = Y, 
                ~{
                  Z <- .x
                  if (!identical(tfmns_ls$class, identity)) {
                    Z@ds_tb <- Z@ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(.y), 
                      tfmns_ls$class(!!rlang::sym(.y))))
                    Z@dictionary_r3 <- Z@dictionary_r3 %>% dplyr::mutate(var_type_chr = dplyr::case_when(.data$var_nm_chr == 
                      .y ~ tfmn_cls_1L_chr, T ~ .data$var_type_chr))
                  }
                  Z
                })
        })
    }
    if (type_1L_chr == "default") {
        multiples_ls <- make_correspondences(dyad_ls, names_1L_lgl = F, 
            reference_1L_int = reference_1L_int)
        dyad_ls <- dyad_ls %>% update_dyad_ls(add_lups_1L_lgl = F, 
            recode_ls = multiples_ls, type_1L_chr = "reference")
    }
    if (standard_spaces_1L_lgl) {
        dyad_ls <- purrr::map(dyad_ls, ~{
            X <- .x
            X@dictionary_r3 <- X@dictionary_r3 %>% dplyr::mutate(var_desc_chr = var_desc_chr %>% 
                purrr::map_chr(~stringr::str_replace_all(.x, 
                  "\\s", " ")))
            X
        })
    }
    if (!identical(arrange_1L_chr, character(0))) {
        dyad_ls <- purrr::map(dyad_ls, ~{
            X <- .x
            X@dictionary_r3 <- eval(parse(text = paste0("dplyr::arrange(X@dictionary_r3,", 
                arrange_1L_chr, ")")))
            X
        })
    }
    if (add_lups_1L_lgl) {
        dyad_ls <- append(dyad_ls, append_ls)
    }
    return(dyad_ls)
}
#' Update pairs list
#' @description update_pairs_ls() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update pairs list. The function returns Pairs (a list).
#' @param pairs_ls Pairs (a list)
#' @param append_ls Append (a list), Default: NULL
#' @param correspondences_r3 Correspondences (a ready4 submodule), Default: ready4show::ready4show_correspondences()
#' @param datestamp_chr Datestamp (a character vector), Default: character(0)
#' @param discard_chr Discard (a character vector), Default: character(0)
#' @return Pairs (a list)
#' @rdname update_pairs_ls
#' @export 
#' @importFrom ready4show ready4show_correspondences
#' @importFrom purrr map reduce discard
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
update_pairs_ls <- function (pairs_ls, append_ls = NULL, correspondences_r3 = ready4show::ready4show_correspondences(), 
    datestamp_chr = character(0), discard_chr = character(0)) 
{
    if (nrow(correspondences_r3) > 0) {
        pairs_ls <- purrr::map(pairs_ls, ~c(ifelse(.x[1] %in% 
            correspondences_r3$old_nms_chr, ready4::get_from_lup_obj(correspondences_r3, 
            match_value_xx = .x[1], match_var_nm_1L_chr = "old_nms_chr", 
            target_var_nm_1L_chr = "new_nms_chr"), .x[1]), .x[2]))
    }
    if (!identical(datestamp_chr, character(0))) {
        pairs_ls <- pairs_ls %>% purrr::map(~c(ifelse(.x[1] %in% 
            datestamp_chr, datestamp_chr[1], .x[1]), .x[2]))
    }
    if (!identical(discard_chr, character(0))) {
        pairs_ls <- purrr::reduce(discard_chr, .init = pairs_ls, 
            ~{
                match_1L_chr <- .y
                .x %>% purrr::discard(~.x[1] == match_1L_chr)
            })
    }
    if (!is.null(append_ls)) {
        pairs_ls <- append(append_ls, pairs_ls)
    }
    return(pairs_ls)
}
#' Update raw data
#' @description update_raw_data() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update raw data. The function returns Dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @param assignment_lup Assignment (a lookup table), Default: NULL
#' @param correspondences_r3 Correspondences (a ready4 submodule), Default: ready4show_correspondences()
#' @param datestamp_chr Datestamp (a character vector), Default: character(0)
#' @param drop_date_copy_1L_lgl Drop date copy (a logical vector of length one), Default: F
#' @param drop_time_1L_lgl Drop time (a logical vector of length one), Default: F
#' @param force_dates_1L_lgl Force dates (a logical vector of length one), Default: F
#' @param force_integers_1L_lgl Force integers (a logical vector of length one), Default: F
#' @param integers_chr Integers (a character vector), Default: character(0)
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one), Default: character(0)
#' @param non_integers_chr Non integers (a character vector), Default: character(0)
#' @param recode_lgl_chr Recode logical vector (a character vector), Default: character(0)
#' @param recode_lgl_fn Recode logical vector (a function), Default: NULL
#' @param recode_vals_fn_ls Recode values (a list of functions), Default: NULL
#' @param response_id_var_nm_1L_chr Response identity variable name (a character vector of length one), Default: character(0)
#' @return Dataset (a tibble)
#' @rdname update_raw_data
#' @export 
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter rename pull mutate across select
#' @importFrom rlang sym
#' @importFrom purrr reduce map_lgl map_chr discard
#' @importFrom ready4 get_from_lup_obj
#' @importFrom tidyselect all_of
#' @importFrom lubridate is.Date
#' @keywords internal
update_raw_data <- function (ds_tb, assignment_lup = NULL, correspondences_r3 = ready4show_correspondences(), 
    datestamp_chr = character(0), drop_date_copy_1L_lgl = F, 
    drop_time_1L_lgl = F, force_dates_1L_lgl = F, force_integers_1L_lgl = F, 
    integers_chr = character(0), match_var_nm_1L_chr = character(0), 
    non_integers_chr = character(0), recode_lgl_chr = character(0), 
    recode_lgl_fn = NULL, recode_vals_fn_ls = NULL, response_id_var_nm_1L_chr = character(0)) 
{
    if (!is.null(assignment_lup)) {
        capture_lgl <- c(assertthat::assert_that(!identical(match_var_nm_1L_chr, 
            character(0))), assertthat::assert_that(!identical(response_id_var_nm_1L_chr, 
            character(0))))
        ds_tb <- ds_tb %>% dplyr::filter(!!rlang::sym(response_id_var_nm_1L_chr) %in% 
            assignment_lup[[match_var_nm_1L_chr]])
    }
    if (nrow(correspondences_r3) > 0) {
        if (!identical(intersect(correspondences_r3$old_nms_chr, 
            names(ds_tb)), character(0))) {
            ds_tb <- purrr::reduce(intersect(correspondences_r3$old_nms_chr, 
                names(ds_tb)), .init = ds_tb, ~.x %>% dplyr::rename(`:=`(!!rlang::sym(ready4::get_from_lup_obj(correspondences_r3, 
                match_value_xx = .y, match_var_nm_1L_chr = "old_nms_chr", 
                target_var_nm_1L_chr = "new_nms_chr")), !!rlang::sym(.y))))
        }
    }
    if (force_integers_1L_lgl) {
        force_to_integers_chr <- names(ds_tb)[names(ds_tb) %>% 
            purrr::map_lgl(~{
                values_xx <- ds_tb %>% dplyr::pull(.x)
                if (is.character(values_xx)) 
                  values_xx <- values_xx %>% purrr::map_chr(~ifelse(.x == 
                    "", NA_character_, .x)) %>% purrr::discard(is.na)
                go_1L_lgl <- all(!is.na(suppressWarnings(as.numeric(values_xx))))
                if (go_1L_lgl) 
                  go_1L_lgl <- all(as.numeric(values_xx) == floor(as.numeric(values_xx)))
                go_1L_lgl
            })]
        force_to_integers_chr <- setdiff(force_to_integers_chr, 
            c(non_integers_chr, datestamp_chr) %>% unique())
        integers_chr <- c(integers_chr, force_to_integers_chr) %>% 
            unique()
    }
    if (length(integers_chr) > 0) {
        ds_tb <- ds_tb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(integers_chr), 
            as.integer))
    }
    if (!identical(recode_lgl_chr, character(0))) {
        if (is.null(recode_lgl_fn)) {
            recode_lgl_fn <- function(x) {
                as.logical(x - 1)
            }
        }
        ds_tb <- ds_tb %>% dplyr::mutate(dplyr::across(tidyselect::all_of(recode_lgl_chr), 
            recode_lgl_fn))
    }
    if (!is.null(recode_vals_fn_ls)) {
        ds_tb <- purrr::reduce(1:length(recode_vals_fn_ls), .init = ds_tb, 
            ~recode_vals_fn_ls[[.y]](.x))
    }
    if (force_dates_1L_lgl) {
        if (!identical(datestamp_chr, character(0))) {
            if (length(datestamp_chr) == 1) {
                datestamp_chr <- rep(datestamp_chr, 2)
            }
            if (!lubridate::is.Date(ds_tb %>% dplyr::pull(datestamp_chr[2]) %>% 
                class())) {
                ds_tb <- ds_tb %>% dplyr::mutate(`:=`(!!rlang::sym(datestamp_chr[1]), 
                  !!rlang::sym(datestamp_chr[2]) %>% transform_dates(drop_time_1L_lgl = drop_time_1L_lgl)))
                if (drop_date_copy_1L_lgl && length(unique(datestamp_chr)) == 
                  2) 
                  ds_tb <- ds_tb %>% dplyr::select(-tidyselect::all_of(datestamp_chr[2]))
            }
        }
    }
    return(ds_tb)
}
#' Update tibble source local to url single tibble
#' @description update_tb_src_loc_to_url_sngl_tb() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update tibble source local to url single tibble. The function returns Updated (a tibble).
#' @param x An object
#' @param y An object
#' @param local_to_url_vec_chr Local to url vector (a character vector)
#' @param urls_vec_chr Urls vector (a character vector)
#' @return Updated (a tibble)
#' @rdname update_tb_src_loc_to_url_sngl_tb
#' @export 
#' @importFrom dplyr mutate
#' @importFrom purrr map2_chr map_chr
#' @keywords internal
update_tb_src_loc_to_url_sngl_tb <- function (x, y, local_to_url_vec_chr, urls_vec_chr) 
{
    updated_tb <- x %>% dplyr::mutate(download_url_chr = purrr::map2_chr(local_file_src_chr, 
        download_url_chr, ~ifelse(.x %in% local_to_url_vec_chr, 
            urls_vec_chr[y], .y))) %>% dplyr::mutate(local_file_src_chr = purrr::map_chr(local_file_src_chr, 
        ~ifelse(.x %in% local_to_url_vec_chr, NA_character_, 
            .x)))
    return(updated_tb)
}
