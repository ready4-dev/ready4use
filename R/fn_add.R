#' Add dictionary
#' @description add_dictionary() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add dictionary. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: Ready4useDyad()
#' @param new_cases_r3 New cases (a ready4 submodule), Default: ready4use_dictionary()
#' @param var_ctg_chr Variable category (a character vector), Default: 'Uncategorised'
#' @param arrange_by_1L_chr Arrange by (a character vector of length one), Default: c("category", "name")
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_dictionary
#' @export
#' @importFrom purrr map_chr
#' @importFrom dplyr pull arrange filter
#' @importFrom rlang sym
add_dictionary <- function (X_Ready4useDyad = Ready4useDyad(), new_cases_r3 = ready4use_dictionary(),
    var_ctg_chr = "Uncategorised", arrange_by_1L_chr = c("category",
        "name"))
{
    arrange_by_1L_chr <- match.arg(arrange_by_1L_chr)
    if (identical(new_cases_r3, ready4use_dictionary())) {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "dictionary_r3",
            var_nm_chr = names(X_Ready4useDyad@ds_tb), var_ctg_chr = var_ctg_chr,
            var_desc_chr = names(X_Ready4useDyad@ds_tb), var_type_chr = names(X_Ready4useDyad@ds_tb) %>%
                purrr::map_chr(~class(X_Ready4useDyad@ds_tb %>%
                  dplyr::pull(.x))[1]))
    }
    else {
        X_Ready4useDyad <- renewSlot(X_Ready4useDyad, "dictionary_r3",
            new_cases_r3 = new_cases_r3)
    }
    X_Ready4useDyad@dictionary_r3 <- X_Ready4useDyad@dictionary_r3 %>%
        dplyr::arrange(!!rlang::sym(ifelse(arrange_by_1L_chr ==
            "name", "var_nm_chr", "var_ctg_chr")))
    X_Ready4useDyad@dictionary_r3 <- X_Ready4useDyad@dictionary_r3 %>%
        dplyr::filter(var_nm_chr %in% names(X_Ready4useDyad@ds_tb))
    return(X_Ready4useDyad)
}
#' Add discrete palette
#' @description add_discrete_palette() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add discrete palette. The function returns Plot (a plot).
#' @param plot_plt Plot (a plot)
#' @param colours_chr Colours (a character vector), Default: c("#de2d26", "#fc9272")
#' @param missing_1L_chr Missing (a character vector of length one), Default: 'grey50'
#' @param type_1L_chr Type (a character vector of length one), Default: c("ggsci", "manual", "viridis")
#' @param what_1L_chr What (a character vector of length one), Default: 'lancet'
#' @return Plot (a plot)
#' @rdname add_discrete_palette
#' @export
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom viridis scale_color_viridis scale_fill_viridis
#' @keywords internal
add_discrete_palette <- function (plot_plt, colours_chr = c("#de2d26", "#fc9272"), missing_1L_chr = "grey50",
    type_1L_chr = c("ggsci", "manual", "viridis"), what_1L_chr = "lancet")
{
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "ggsci") {
        one_fn <- get_journal_palette_fn("colour", what_1L_chr = what_1L_chr)
        two_fn <- get_journal_palette_fn("fill", what_1L_chr = what_1L_chr)
        plot_plt <- plot_plt + one_fn(na.value = missing_1L_chr) +
            two_fn(na.value = missing_1L_chr)
    }
    if (type_1L_chr == "manual") {
        plot_plt <- plot_plt + ggplot2::scale_fill_manual(values = colours_chr)
    }
    if (type_1L_chr == "viridis") {
        plot_plt <- plot_plt + viridis::scale_color_viridis(discrete = TRUE,
            option = what_1L_chr) + viridis::scale_fill_viridis(discrete = TRUE,
            option = what_1L_chr)
    }
    return(plot_plt)
}
#' Add dataset to dataverse repository
#' @description add_ds_to_dv_repo() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add dataset to dataverse repository. The function returns Dataset url (a character vector of length one).
#' @param dv_1L_chr Dataverse (a character vector of length one)
#' @param ds_meta_ls Dataset meta (a list)
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Dataset url (a character vector of length one)
#' @rdname add_ds_to_dv_repo
#' @export
#' @importFrom dataverse get_dataverse dataverse_contents get_dataset update_dataset
#' @importFrom purrr map_chr pluck discard map_lgl
#' @importFrom utils getFromNamespace
#' @keywords internal
add_ds_to_dv_repo <- function (dv_1L_chr, ds_meta_ls, key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER"))
{
    dv <- dataverse::get_dataverse(dv_1L_chr)
    dv_ls <- dataverse::dataverse_contents(dv)
    per_chr_vec <- purrr::map_chr(dv_ls, ~{
        per_chr <- .x %>% purrr::pluck("persistentUrl")
        ifelse(is.null(per_chr), NA_character_, per_chr)
    }) %>% purrr::discard(is.na) %>% unname()
    add_ds_lgl <- T
    update_ds_lgl <- F
    if (!identical(per_chr_vec, character(0))) {
        db_nm_chr_vec <- purrr::map_chr(per_chr_vec, ~{
            ds_ls <- dataverse::get_dataset(.x)
            ds_ls$metadataBlocks$citation$fields$value[[1]]
        })
        add_ds_lgl <- !(ds_meta_ls$title %in% db_nm_chr_vec)
    }
    if (add_ds_lgl) {
        add_sword_ds <- utils::getFromNamespace("initiate_sword_dataset",
            "dataverse")
        add_sword_ds(dv_1L_chr, body = ds_meta_ls, key = key_1L_chr,
            server = server_1L_chr)
        dv_ls <- dataverse::dataverse_contents(dv)
    }
    else {
        ds_ls <- dataverse::get_dataset(per_chr_vec[ds_meta_ls$title ==
            db_nm_chr_vec])
        update_ds_lgl <- purrr::map_lgl(names(ds_meta_ls), ~{
            type_name_chr <- {
                tmp_chr <- switch(.x, creator = "author", description = "dsDescription",
                  subject = "keyword")
                ifelse(is.null(tmp_chr), ifelse(.x %in% ds_ls$metadataBlocks$citation$fields$typeName,
                  .x, NA_character_), tmp_chr)
            }
            new_val_chr <- ds_meta_ls %>% purrr::pluck(.x)
            idx_dbl <- which(type_name_chr == ds_ls$metadataBlocks$citation$fields$typeName)
            purrr::map_lgl(1:length(ds_ls$metadataBlocks$citation$fields$value[idx_dbl]),
                ~{
                  if (class(ds_ls$metadataBlocks$citation$fields$value[idx_dbl][[.x]]) ==
                    "character") {
                    (new_val_chr != ds_ls$metadataBlocks$citation$fields$value[idx_dbl])
                  }
                  else {
                    if (class(ds_ls$metadataBlocks$citation$fields$value[idx_dbl][[.x]]) ==
                      "data.frame")
                      (new_val_chr != ds_ls$metadataBlocks$citation$fields$value[idx_dbl][[.x]][[1]]$value)
                  }
                }) %>% any()
        }) %>% any()
        if (update_ds_lgl & F)
            dataverse::update_dataset(dataset = ds_ls, body = ds_meta_ls,
                key = key_1L_chr, server = server_1L_chr)
        dv_ls <- dataverse::dataverse_contents(dv)
    }
    ds_url_1L_chr <- dv_ls[[1]]$persistentUrl
    return(ds_url_1L_chr)
}
#' Add dataverse meta to import lookup table
#' @description add_dv_meta_to_imp_lup() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add dataverse meta to import lookup table. The function returns Import (a lookup table).
#' @param imp_lup Import (a lookup table)
#' @param ds_ui_1L_chr Dataset user interface (a character vector of length one)
#' @param file_type_1L_chr File type (a character vector of length one)
#' @param save_type_1L_chr Save type (a character vector of length one)
#' @return Import (a lookup table)
#' @rdname add_dv_meta_to_imp_lup
#' @export
#' @importFrom dplyr mutate
#' @keywords internal
add_dv_meta_to_imp_lup <- function (imp_lup, ds_ui_1L_chr, file_type_1L_chr, save_type_1L_chr)
{
    assert_single_row_tb(imp_lup)
    imp_lup <- imp_lup %>% dplyr::mutate(data_repo_db_ui_chr = ds_ui_1L_chr,
        data_repo_file_ext_chr = file_type_1L_chr, data_repo_save_type_chr = save_type_1L_chr)
    return(imp_lup)
}
#' Add fields from lookup table
#' @description add_fields_from_lup() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add fields from lookup table. The function returns Dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @param lup_tb Lookup table (a tibble)
#' @param match_chr Match (a character vector)
#' @param target_1L_chr Target (a character vector of length one)
#' @param vars_chr Variables (a character vector)
#' @return Dataset (a tibble)
#' @rdname add_fields_from_lup
#' @export
#' @importFrom purrr reduce map_chr
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
add_fields_from_lup <- function (ds_tb, lup_tb, match_chr, target_1L_chr, vars_chr)
{
    ds_tb <- purrr::reduce(vars_chr, .init = ds_tb, ~{
        target_1L_chr <- .y
        .x %>% dplyr::mutate(`:=`(!!rlang::sym(target_1L_chr),
            !!rlang::sym(match_chr[1]) %>% purrr::map_chr(~ready4::get_from_lup_obj(lup_tb,
                match_var_nm_1L_chr = match_chr[2], match_value_xx = .x,
                target_var_nm_1L_chr = target_1L_chr) %>% as.character)))
    })
    return(ds_tb)
}
#' Add files to dataverse
#' @description add_files_to_dv() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add files to dataverse. The function returns File identities (an integer vector).
#' @param files_tb Files (a tibble)
#' @param data_dir_rt_1L_chr Data directory root (a character vector of length one), Default: '.'
#' @param ds_url_1L_chr Dataset url (a character vector of length one)
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return File identities (an integer vector)
#' @rdname add_files_to_dv
#' @export
#' @importFrom lifecycle deprecate_soft
#' @importFrom dataverse get_dataset
#' @importFrom purrr pmap_int
#' @importFrom ready4 write_fls_to_dv
#' @keywords internal
add_files_to_dv <- function (files_tb, data_dir_rt_1L_chr = ".", ds_url_1L_chr,
    key_1L_chr = Sys.getenv("DATAVERSE_KEY"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER"))
{
    lifecycle::deprecate_soft("0.0.0.9149", "add_files_to_dv()",
        "ready4::write_to_dv_from_tbl()")
    ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
    is_draft_1L_lgl <- ds_ls$versionState == "DRAFT"
    nms_chr <- ds_ls$files$filename
    fl_ids_int <- purrr::pmap_int(files_tb, ~{
        path_1L_chr <- paste0(ifelse(identical(character(0),
            data_dir_rt_1L_chr), "", paste0(data_dir_rt_1L_chr,
            "/")), ..1, "/", ..2, ..3)
        fl_nm_1L_chr <- paste0(..2, ..3)
        ready4::write_fls_to_dv(path_1L_chr, descriptions_chr = ..4,
            ds_url_1L_chr = ds_url_1L_chr, ds_ls = ds_ls, key_1L_chr = key_1L_chr,
            server_1L_chr = server_1L_chr)
    })
    return(fl_ids_int)
}
#' Add from lookup table prototype
#' @description add_from_lup_prototype() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add from lookup table prototype. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param arrange_1L_chr Arrange (a character vector of length one), Default: character(0)
#' @param exclude_chr Exclude (a character vector), Default: character(0)
#' @param lup_prototype_tb Lookup table prototype (a tibble), Default: NULL
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one), Default: 'UID_chr'
#' @param method_1L_chr Method (a character vector of length one), Default: c("first", "sample")
#' @param type_1L_chr Type (a character vector of length one), Default: c("sequential", "batch", "self")
#' @param vars_chr Variables (a character vector), Default: character(0)
#' @return Data (a tibble)
#' @rdname add_from_lup_prototype
#' @export
#' @importFrom purrr reduce
#' @importFrom dplyr left_join select distinct filter bind_rows arrange
#' @importFrom tidyselect all_of
#' @importFrom rlang sym
add_from_lup_prototype <- function (data_tb, arrange_1L_chr = character(0), exclude_chr = character(0),
    lup_prototype_tb = NULL, match_var_nm_1L_chr = "UID_chr",
    method_1L_chr = c("first", "sample"), type_1L_chr = c("sequential",
        "batch", "self"), vars_chr = character(0))
{
    method_1L_chr <- match.arg(method_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    if (type_1L_chr == "sequential") {
        data_tb <- purrr::reduce(vars_chr, .init = data_tb, ~.x %>%
            dplyr::left_join(lup_prototype_tb %>% dplyr::select(tidyselect::all_of(c(match_var_nm_1L_chr,
                .y))) %>% dplyr::distinct()))
    }
    if (type_1L_chr == "batch") {
        distinct_tb <- lup_prototype_tb %>% dplyr::select(tidyselect::all_of(c(match_var_nm_1L_chr,
            setdiff(setdiff(names(lup_prototype_tb), names(data_tb)),
                exclude_chr)))) %>% make_imputed_distinct_cases(uid_1L_chr = match_var_nm_1L_chr,
            method_1L_chr = method_1L_chr)
        data_tb <- data_tb %>% dplyr::left_join(distinct_tb)
    }
    if (type_1L_chr == "self") {
        if (identical(vars_chr, character(0))) {
            vars_chr <- setdiff(names(data_tb), c(match_var_nm_1L_chr,
                exclude_chr))
        }
        data_tb <- purrr::reduce(vars_chr, .init = data_tb, ~{
            distinct_tb <- .x %>% dplyr::select(tidyselect::all_of(c(match_var_nm_1L_chr,
                .y))) %>% make_imputed_distinct_cases(uid_1L_chr = match_var_nm_1L_chr,
                method_1L_chr = method_1L_chr)
            complete_tb <- .x %>% dplyr::filter(!is.na(!!rlang::sym(.y)))
            missing_tb <- .x %>% dplyr::filter(is.na(!!rlang::sym(.y)))
            imputed_tb <- missing_tb %>% dplyr::select(-tidyselect::all_of(.y)) %>%
                dplyr::left_join(distinct_tb %>% dplyr::select(tidyselect::all_of(c(match_var_nm_1L_chr,
                  .y))))
            dplyr::bind_rows(complete_tb, imputed_tb)
        })
    }
    if (!identical(arrange_1L_chr, character(0)))
        data_tb <- data_tb %>% dplyr::arrange(!!rlang::sym(arrange_1L_chr))
    return(data_tb)
}
#' Add labels from dictionary
#' @description add_labels_from_dictionary() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add labels from dictionary. The function returns Labelled dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @param dictionary_tb Dictionary (a tibble)
#' @param remove_old_lbls_1L_lgl Remove old labels (a logical vector of length one), Default: F
#' @return Labelled dataset (a tibble)
#' @rdname add_labels_from_dictionary
#' @export
#' @importFrom ready4 remove_lbls_from_df
#' @importFrom dplyr filter mutate case_when
#' @importFrom purrr reduce
#' @importFrom Hmisc label
add_labels_from_dictionary <- function (ds_tb, dictionary_tb, remove_old_lbls_1L_lgl = F)
{
    if (remove_old_lbls_1L_lgl)
        ds_tb <- ds_tb %>% ready4::remove_lbls_from_df()
    data_dictionary_tb <- dictionary_tb %>% dplyr::filter(var_nm_chr %in%
        names(ds_tb)) %>% dplyr::mutate(var_desc_chr = dplyr::case_when(is.na(var_desc_chr) ~
        var_nm_chr, TRUE ~ var_desc_chr)) %>% ready4::remove_lbls_from_df()
    if (nrow(data_dictionary_tb) > 0) {
        labelled_ds_tb <- seq_len(nrow(data_dictionary_tb)) %>%
            purrr::reduce(.init = ds_tb, ~{
                Hmisc::label(.x[[data_dictionary_tb$var_nm_chr[.y]]]) <- data_dictionary_tb$var_desc_chr[.y]
                .x
            })
    }
    else {
        labelled_ds_tb <- ds_tb
    }
    return(labelled_ds_tb)
}
#' Add latest match
#' @description add_latest_match() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add latest match. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param dynamic_lup Dynamic (a lookup table)
#' @param target_var_nm_1L_chr Target variable name (a character vector of length one)
#' @param date_var_1L_chr Date variable (a character vector of length one), Default: 'Date'
#' @param invert_1L_lgl Invert (a logical vector of length one), Default: FALSE
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one), Default: 'UID_chr'
#' @param type_1L_chr Type (a character vector of length one), Default: c("chr", "dbl", "int", "lgl")
#' @return Data (a tibble)
#' @rdname add_latest_match
#' @export
#' @importFrom purrr map2_chr map2_dbl map2_int map2_lgl
#' @importFrom dplyr mutate pull filter
#' @importFrom rlang sym
#' @importFrom ready4 get_from_lup_obj
add_latest_match <- function (data_tb, dynamic_lup, target_var_nm_1L_chr, date_var_1L_chr = "Date",
    invert_1L_lgl = FALSE, match_var_nm_1L_chr = "UID_chr", type_1L_chr = c("chr",
        "dbl", "int", "lgl"))
{
    type_1L_chr <- match.arg(type_1L_chr)
    exec_ls <- switch(type_1L_chr, chr = list(fn = purrr::map2_chr,
        missing = NA_character_), dbl = list(fn = purrr::map2_dbl,
        missing = NA_real_), int = list(fn = purrr::map2_int,
        missing = NA_integer_), lgl = list(fn = purrr::map2_lgl,
        missing = NA))
    test_fn <- ifelse(invert_1L_lgl, `>=`, `<=`)
    data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(target_var_nm_1L_chr),
        !!rlang::sym(match_var_nm_1L_chr) %>% exec_ls$fn(!!rlang::sym(date_var_1L_chr),
            ~ifelse(is.na(.x) | !.x %in% (dynamic_lup %>% dplyr::pull(!!rlang::sym(match_var_nm_1L_chr))),
                exec_ls$missing, ready4::get_from_lup_obj(dynamic_lup %>%
                  dplyr::filter(test_fn(!!rlang::sym(date_var_1L_chr),
                    .y)), match_var_nm_1L_chr = match_var_nm_1L_chr,
                  match_value_xx = .x, target_var_nm_1L_chr = target_var_nm_1L_chr)))))
    return(data_tb)
}
#' Add with join
#' @description add_with_join() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add with join. The function is called for its side effects and does not return a value.
#' @param X_Ready4useDyad PARAM_DESCRIPTION
#' @param Y_Ready4useDyad PARAM_DESCRIPTION
#' @return X (A dataset and data dictionary pair.)
#' @rdname add_with_join
#' @export
#' @importFrom dplyr left_join filter
add_with_join <- function (X_Ready4useDyad, Y_Ready4useDyad)
{
    X_Ready4useDyad@ds_tb <- dplyr::left_join(X_Ready4useDyad@ds_tb,
        Y_Ready4useDyad@ds_tb)
    X_Ready4useDyad <- add_dictionary(X_Ready4useDyad,
        new_cases_r3 = Y_Ready4useDyad@dictionary_r3 %>% dplyr::filter(!var_nm_chr %in%
            X_Ready4useDyad@dictionary_r3$var_nm_chr))
    return(X_Ready4useDyad)
}
