#' Make correspondences
#' @description make_correspondences() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make correspondences. The function returns Correspondences (a list).
#' @param dyad_ls Dyad (a list)
#' @param append_1L_lgl Append (a logical vector of length one), Default: T
#' @param correspondences_ls Correspondences (a list), Default: NULL
#' @param names_1L_lgl Names (a logical vector of length one), Default: F
#' @param reference_1L_int Reference (an integer vector of length one), Default: 2
#' @return Correspondences (a list)
#' @rdname make_correspondences
#' @export 
#' @importFrom purrr map map2_dfr
#' @importFrom ready4 get_from_lup_obj
#' @importFrom ready4show renew.ready4show_correspondences ready4show_correspondences
#' @importFrom stats setNames
#' @keywords internal
make_correspondences <- function (dyad_ls, append_1L_lgl = T, correspondences_ls = NULL, 
    names_1L_lgl = F, reference_1L_int = 2L) 
{
    items_int <- length(dyad_ls)
    if (items_int > 1) {
        if (is.null(correspondences_ls)) {
            correspondences_ls <- setdiff(1:items_int, reference_1L_int) %>% 
                purrr::map(~{
                  dyad_ls[[.x]]@dictionary_r3$var_nm_chr %>% 
                    purrr::map2_dfr(dyad_ls[[.x]]@dictionary_r3$var_desc_chr, 
                      ~{
                        x <- ready4show_correspondences()
                        test_1L_chr <- ready4::get_from_lup_obj(dyad_ls[[reference_1L_int]]@dictionary_r3, 
                          match_value_xx = .x, match_var_nm_1L_chr = "var_nm_chr", 
                          target_var_nm_1L_chr = "var_desc_chr")
                        if (!identical(.y, test_1L_chr)) {
                          x <- ready4show::renew.ready4show_correspondences(x, 
                            new_nms_chr = test_1L_chr, old_nms_chr = .y)
                        }
                        x
                      })
                }) %>% stats::setNames(names(dyad_ls)[setdiff(1:items_int, 
                reference_1L_int)])
            if (names_1L_lgl) {
                correspondences_ls <- make_correspondences(dyad_ls, 
                  append_1L_lgl = append_1L_lgl, correspondences_ls = correspondences_ls, 
                  reference_1L_int = reference_1L_int)
            }
        }
        else {
            correspondences_ls <- update_correspondences(correspondences_ls, 
                dyad_ls = dyad_ls, reference_1L_int = reference_1L_int)
        }
        if (append_1L_lgl && length(correspondences_ls) < length(dyad_ls)) 
            correspondences_ls <- correspondences_ls %>% append(list(ready4show::ready4show_correspondences()) %>% 
                stats::setNames(names(dyad_ls)[reference_1L_int]), 
                after = reference_1L_int - 1)
    }
    else {
        correspondences_ls <- NULL
    }
    return(correspondences_ls)
}
#' Make imputed distinct cases
#' @description make_imputed_distinct_cases() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make imputed distinct cases. The function returns Distinct (a tibble).
#' @param data_tb Data (a tibble)
#' @param method_1L_chr Method (a character vector of length one), Default: c("first", "sample")
#' @param uid_1L_chr Unique identifier (a character vector of length one), Default: 'UID_chr'
#' @return Distinct (a tibble)
#' @rdname make_imputed_distinct_cases
#' @export 
#' @importFrom dplyr filter distinct group_by summarise across everything ungroup bind_rows
#' @importFrom rlang sym
make_imputed_distinct_cases <- function (data_tb, method_1L_chr = c("first", "sample"), uid_1L_chr = "UID_chr") 
{
    method_1L_chr <- match.arg(method_1L_chr)
    distinct_tb <- data_tb %>% dplyr::filter(!is.na(!!rlang::sym(uid_1L_chr))) %>% 
        dplyr::distinct()
    most_complete_tb <- distinct_tb %>% dplyr::filter(!!rlang::sym(uid_1L_chr) %in% 
        distinct_tb[, uid_1L_chr][[1]][duplicated(distinct_tb[, 
            uid_1L_chr][[1]])]) %>% dplyr::group_by(!!rlang::sym(uid_1L_chr)) %>% 
        dplyr::summarise(dplyr::across(dplyr::everything(), ~if (method_1L_chr == 
            "first") {
            .x[which(!is.na(.x))[1]]
        }
        else {
            if (identical(which(!is.na(.x)), integer(0))) {
                .x[1]
            }
            else {
                .x[which(!is.na(.x)) %>% sample(1)]
            }
        })) %>% dplyr::ungroup()
    distinct_tb <- distinct_tb %>% dplyr::filter(!(!!rlang::sym(uid_1L_chr) %in% 
        most_complete_tb[, uid_1L_chr][[1]])) %>% dplyr::bind_rows(most_complete_tb)
    return(distinct_tb)
}
#' Make keep logical vector
#' @description make_keep_lgl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make keep logical vector. The function returns Keep (a logical vector).
#' @param ds_tb Dataset (a tibble)
#' @param filter_fn Filter (a function), Default: is.na
#' @param summary_fn Summary (a function), Default: any
#' @param var_nms_chr Variable names (a character vector)
#' @return Keep (a logical vector)
#' @rdname make_keep_lgl
#' @export 
#' @importFrom purrr map_dfc flatten_lgl
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang sym
#' @importFrom dplyr pull summarise_all
#' @keywords internal
make_keep_lgl <- function (ds_tb, filter_fn = is.na, summary_fn = any, var_nms_chr) 
{
    keep_lgl <- var_nms_chr %>% purrr::map_dfc(~tibble::tibble(`:=`(!!rlang::sym(.x), 
        ds_tb %>% dplyr::pull(.x) %>% filter_fn()))) %>% t() %>% 
        tibble::as_tibble() %>% dplyr::summarise_all(summary_fn) %>% 
        as.vector() %>% purrr::flatten_lgl()
    return(keep_lgl)
}
#' Make missing report
#' @description make_missing_report() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make missing report. The function returns Missing report (an output object of multiple potential types).
#' @param ds_xx Dataset (an output object of multiple potential types)
#' @return Missing report (an output object of multiple potential types)
#' @rdname make_missing_report
#' @export 
#' @importFrom purrr map discard map2_dfr
#' @importFrom naniar miss_var_summary
#' @importFrom dplyr filter mutate select everything
make_missing_report <- function (ds_xx) 
{
    if (!inherits(ds_xx, "list")) {
        dss_ls <- list(ds_xx)
    }
    else {
        dss_ls <- ds_xx
    }
    missing_report_ls <- purrr::map(dss_ls, ~{
        if (inherits(.x, "Ready4useDyad")) {
            ds_tb <- .x@ds_tb
        }
        else {
            ds_tb <- .x
        }
        naniar::miss_var_summary(ds_tb) %>% dplyr::filter(n_miss > 
            1)
    })
    if (is.null(names(missing_report_ls))) {
        names(missing_report_ls) <- paste0("Dataset ", 1:length(missing_report_ls))
    }
    missing_report_ls <- missing_report_ls %>% purrr::discard(~nrow(.x) == 
        0)
    if (identical(unname(missing_report_ls), list())) {
        missing_report_xx <- NULL
    }
    else {
        missing_report_xx <- missing_report_ls %>% purrr::map2_dfr(names(missing_report_ls), 
            ~.x %>% dplyr::mutate(dataset = .y) %>% dplyr::select(dataset, 
                dplyr::everything()))
    }
    return(missing_report_xx)
}
#' Make period correspondences
#' @description make_period_correspondences() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make period correspondences. The function returns Correspondences (an output object of multiple potential types).
#' @param descriptions_chr Descriptions (a character vector)
#' @param integers_1L_lgl Integers (a logical vector of length one), Default: TRUE
#' @param name_1L_chr Name (a character vector of length one), Default: 'period'
#' @param plural_chr Plural (a character vector), Default: 's'
#' @param range_int Range (an integer vector), Default: 1L:12L
#' @param reference_1L_int Reference (an integer vector of length one), Default: integer(0)
#' @param spaced_1L_lgl Spaced (a logical vector of length one), Default: TRUE
#' @param type_1L_chr Type (a character vector of length one), Default: 'descriptions'
#' @param units_chr Units (a character vector), Default: c("minute", "hour", "week", "month", "year")
#' @return Correspondences (an output object of multiple potential types)
#' @rdname make_period_correspondences
#' @export 
#' @importFrom purrr map2 map_chr reduce flatten_chr
#' @importFrom english words
#' @importFrom ready4show ready4show_correspondences renew.ready4show_correspondences
#' @importFrom stringr str_replace_all
#' @importFrom dplyr filter
#' @keywords internal
make_period_correspondences <- function (descriptions_chr, integers_1L_lgl = TRUE, name_1L_chr = "period", 
    plural_chr = "s", range_int = 1L:12L, reference_1L_int = integer(0), 
    spaced_1L_lgl = TRUE, type_1L_chr = "descriptions", units_chr = c("minute", 
        "hour", "week", "month", "year")) 
{
    patterns_ls <- get_patterns(descriptions_chr, flatten_1L_lgl = F, 
        integers_1L_lgl = integers_1L_lgl, plural_chr = plural_chr, 
        range_int = range_int, reference_1L_int = reference_1L_int, 
        replace_blanks_1L_lgl = T, spaced_1L_lgl = spaced_1L_lgl, 
        units_chr = units_chr)
    new_xx <- descriptions_chr %>% purrr::map2(patterns_ls, ~{
        description_1L_chr <- .x
        patterns_chr <- .y
        concepts_chr <- patterns_chr %>% strsplit(" ") %>% purrr::map_chr(~{
            parts_chr <- if (suppressWarnings(!is.na(as.numeric(.x[1])))) {
                c(english::words(as.numeric(.x[1])), .x[-1])
            }
            else {
                .x
            }
            paste0(parts_chr, collapse = " ")
        })
        unique_chr <- unique(concepts_chr)
        if (length(unique_chr) > 1) {
            options_chr <- paste0(name_1L_chr, "_", 1:length(unique_chr))
            replacements_chr <- patterns_chr %>% strsplit(" ") %>% 
                purrr::map_chr(~{
                  parts_chr <- if (suppressWarnings(!is.na(as.numeric(.x[1])))) {
                    c(english::words(as.numeric(.x[1])), .x[-1])
                  }
                  else {
                    .x
                  }
                  options_chr[which(paste0(parts_chr, collapse = " ") == 
                    unique_chr)]
                })
        }
        else {
            replacements_chr <- rep(name_1L_chr, length(patterns_chr))
        }
        if (type_1L_chr == "periods") {
            ready4show::ready4show_correspondences() %>% ready4show::renew.ready4show_correspondences(old_nms_chr = unique_chr, 
                new_nms_chr = replacements_chr)
        }
        else {
            purrr::reduce(1:length(patterns_chr), .init = description_1L_chr, 
                ~stringr::str_replace_all(.x, patterns_chr[.y], 
                  replacements_chr[.y]))
        }
    })
    if (type_1L_chr == "descriptions") {
        correspondences_xx <- ready4show::ready4show_correspondences() %>% 
            ready4show::renew.ready4show_correspondences(old_nms_chr = descriptions_chr, 
                new_nms_chr = new_xx %>% purrr::flatten_chr()) %>% 
            dplyr::filter(!is.na(new_nms_chr))
    }
    if (type_1L_chr == "periods") {
        correspondences_xx <- new_xx
    }
    return(correspondences_xx)
}
#' Make ready4 submodule from comma separated variables file tibble
#' @description make_r3_from_csv_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make ready4 submodule from comma separated variables file tibble. The function returns Tibble ready4 submodule (a ready4 submodule extension of tibble).
#' @param csv_tb Comma separated variables file (a tibble)
#' @param r3_fn Ready4 submodule (a function)
#' @return Tibble ready4 submodule (a ready4 submodule extension of tibble)
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
#' Make significance dataframe
#' @description make_significance_df() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make significance dataframe. The function returns Data (a data.frame).
#' @param data_tb Data (a tibble)
#' @param by_1L_chr By (a character vector of length one)
#' @param vars_chr Variables (a character vector)
#' @param sort_1L_lgl Sort (a logical vector of length one), Default: T
#' @return Data (a data.frame)
#' @rdname make_significance_df
#' @export 
#' @importFrom stats formula
#' @importFrom rlang exec
#' @importFrom arsenal tableby
#' @importFrom dplyr arrange filter select mutate
#' @importFrom gtools stars.pval
#' @keywords internal
make_significance_df <- function (data_tb, by_1L_chr, vars_chr, sort_1L_lgl = T) 
{
    args_ls <- list(formula = paste0(by_1L_chr, " ~ ", paste0(paste0(vars_chr, 
        collapse = " + "))) %>% stats::formula(), data = data_tb)
    data_xx <- rlang::exec(arsenal::tableby, !!!args_ls)
    data_df <- data_xx %>% as.data.frame()
    if (sort_1L_lgl) {
        data_df <- data_df %>% dplyr::arrange(p.value)
    }
    data_df <- data_df %>% dplyr::filter(!term %in% c("countpct", 
        "Nmiss")) %>% dplyr::select(variable, test, p.value) %>% 
        dplyr::mutate(stars = gtools::stars.pval(p.value))
    return(data_df)
}
#' Make temporal lookup table
#' @description make_temporal_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make temporal lookup table. The function returns Temporal (a lookup table).
#' @param dyad_ls Dyad (a list)
#' @param recode_ls Recode (a list)
#' @param spaced_1L_lgl Spaced (a logical vector of length one), Default: TRUE
#' @return Temporal (a lookup table)
#' @rdname make_temporal_lup
#' @export 
#' @importFrom purrr map_dfr map2_dfr
#' @importFrom tibble tibble
#' @importFrom ready4 get_from_lup_obj
#' @importFrom dplyr mutate
#' @importFrom stringr word
#' @keywords internal
make_temporal_lup <- function (dyad_ls, recode_ls, spaced_1L_lgl = TRUE) 
{
    temporal_lup <- purrr::map_dfr(1:length(dyad_ls), ~{
        if (!identical(.x, ready4show_correspondences())) {
            Y <- dyad_ls[[.x]]
            x <- recode_ls[[.x]]
            grouping_1L_chr <- names(dyad_ls)[.x]
            new_lup <- x$old_nms_chr %>% make_period_correspondences(spaced_1L_lgl = spaced_1L_lgl, 
                type_1L_chr = "periods") %>% purrr::map2_dfr(x$old_nms_chr, 
                ~{
                  lup_tb <- tibble::tibble(grouping_chr = grouping_1L_chr, 
                    variable_chr = ready4::get_from_lup_obj(Y@dictionary_r3, 
                      match_var_nm_1L_chr = "var_desc_chr", match_value_xx = .y, 
                      target_var_nm_1L_chr = "var_nm_chr"), periods_chr = .x$old_nms_chr, 
                    standardised_chr = .x$new_nms_chr)
                  lup_tb <- lup_tb %>% dplyr::mutate(value_int = periods_chr %>% 
                    stringr::word(), unit_chr = periods_chr %>% 
                    stringr::word(start = 2))
                  lup_tb
                })
        }
        else {
            NULL
        }
    })
    return(temporal_lup)
}
