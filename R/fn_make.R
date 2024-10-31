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
            ifelse(identical(which(!is.na(.x)), integer(0)), 
                .x[1], .x[which(!is.na(.x)) %>% sample(1)])
        })) %>% dplyr::ungroup()
    distinct_tb <- distinct_tb %>% dplyr::filter(!(!!rlang::sym(uid_1L_chr) %in% 
        most_complete_tb[, uid_1L_chr][[1]])) %>% dplyr::bind_rows(most_complete_tb)
    return(distinct_tb)
}
#' Make journal plot
#' @description make_journal_plot() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make journal plot. The function returns Plot (a plot).
#' @param data_tb Data (a tibble)
#' @param as_percent_1L_lgl As percent (a logical vector of length one), Default: FALSE
#' @param by_1L_chr By (a character vector of length one), Default: character(0)
#' @param colours_chr Colours (a character vector), Default: c("#de2d26", "#fc9272")
#' @param drop_legend_1L_lgl Drop legend (a logical vector of length one), Default: FALSE
#' @param drop_missing_1L_lgl Drop missing (a logical vector of length one), Default: FALSE
#' @param drop_ticks_1L_lgl Drop ticks (a logical vector of length one), Default: FALSE
#' @param fill_single_1L_lgl Fill single (a logical vector of length one), Default: FALSE
#' @param label_fill_1L_chr Label fill (a character vector of length one), Default: character(0)
#' @param line_1L_chr Line (a character vector of length one), Default: 'black'
#' @param position_xx Position (an output object of multiple potential types), Default: NULL
#' @param recode_lup_r3 Recode (a ready4 submodule extension of lookup table), Default: ready4show::ready4show_correspondences()
#' @param style_1L_chr Style (a character vector of length one), Default: get_styles()
#' @param title_1L_chr Title (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("ggsci", "manual", "viridis")
#' @param x_1L_chr X (a character vector of length one), Default: character(0)
#' @param x_label_1L_chr X label (a character vector of length one), Default: character(0)
#' @param y_1L_chr Y (a character vector of length one), Default: character(0)
#' @param y_label_1L_chr Y label (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: get_journal_plot_fn("names")
#' @param ... Additional arguments
#' @return Plot (a plot)
#' @rdname make_journal_plot
#' @export 
#' @importFrom ready4show ready4show_correspondences manufacture.ready4show_correspondences
#' @importFrom dplyr select pull mutate rename
#' @importFrom tidyselect all_of any_of
#' @importFrom tidyr drop_na
#' @importFrom rlang sym exec
#' @importFrom purrr discard
#' @importFrom ggplot2 position_dodge aes scale_y_continuous labs theme element_blank
#' @importFrom tibble as_tibble
#' @importFrom scales label_percent
#' @importFrom ggpubr gradient_fill
#' @keywords internal
make_journal_plot <- function (data_tb, as_percent_1L_lgl = FALSE, by_1L_chr = character(0), 
    colours_chr = c("#de2d26", "#fc9272"), drop_legend_1L_lgl = FALSE, 
    drop_missing_1L_lgl = FALSE, drop_ticks_1L_lgl = FALSE, fill_single_1L_lgl = FALSE, 
    label_fill_1L_chr = character(0), line_1L_chr = "black", 
    position_xx = NULL, recode_lup_r3 = ready4show::ready4show_correspondences(), 
    style_1L_chr = get_styles(), title_1L_chr = character(0), 
    type_1L_chr = c("ggsci", "manual", "viridis"), x_1L_chr = character(0), 
    x_label_1L_chr = character(0), y_1L_chr = character(0), y_label_1L_chr = character(0), 
    what_1L_chr = get_journal_plot_fn("names"), ...) 
{
    style_1L_chr <- match.arg(style_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    if (what_1L_chr %in% c("donutchart", "pie") & !identical(by_1L_chr, 
        character(0)) & drop_missing_1L_lgl) {
        message("Ignoring drop_missing_1L_lgl argument value - this is only used when not directly supplying a frequency table")
        drop_missing_1L_lgl <- FALSE
    }
    custom_args_ls <- args_ls <- list(...)
    call_ls <- sys.call()
    load_pkg_1L_lgl <- F
    if ("add" %in% names(custom_args_ls)) {
        if (startsWith(custom_args_ls$add, "mean") & custom_args_ls$add != 
            "mean") {
            load_pkg_1L_lgl <- !(paste("package", "ggpubr", sep = ":") %in% 
                search())
        }
    }
    if (what_1L_chr %in% c("errorplot")) {
        load_pkg_1L_lgl <- !(paste("package", "ggpubr", sep = ":") %in% 
            search())
    }
    if (load_pkg_1L_lgl) {
        message("You need to load the package ggpubr for this function call to execute correctly.")
    }
    if ("fill" %in% names(call_ls)) {
        if (!"fill_single_1L_lgl" %in% names(call_ls)) {
            fill_single_1L_lgl <- FALSE
        }
        else {
            fill_single_1L_lgl <- call_ls$fill_single_1L_lgl %>% 
                as.character() %>% as.logical()
        }
        custom_args_ls$fill <- args_ls$fill <- call_ls$fill %>% 
            as.character()
        custom_args_ls$fill_single_1L_lgl <- args_ls$fill_single_1L_lgl <- NULL
    }
    if ("title" %in% names(call_ls)) {
        if (!"title_1L_chr" %in% names(call_ls)) {
            title_1L_chr <- character(0)
        }
        else {
            title_1L_chr <- call_ls$title_1L_chr %>% as.character()
        }
        custom_args_ls$title <- args_ls$title <- call_ls$title %>% 
            as.character()
        custom_args_ls$title_1L_chr <- args_ls$title_1L_chr <- NULL
    }
    if ("facet.by" %in% names(custom_args_ls)) {
        extras_chr <- custom_args_ls$facet.by
    }
    else {
        extras_chr <- character(0)
    }
    data_xx <- data_tb %>% dplyr::select(tidyselect::all_of(c(x_1L_chr, 
        y_1L_chr, by_1L_chr, extras_chr)))
    if (drop_missing_1L_lgl) {
        data_xx <- tidyr::drop_na(data_xx, tidyselect::all_of(c(x_1L_chr, 
            y_1L_chr, by_1L_chr, extras_chr)))
    }
    plot_fn <- get_journal_plot_fn(what_1L_chr)
    colour_1L_int <- 1
    pick_1L_int <- integer(0)
    if (!what_1L_chr %in% c("balloonplot")) {
        if (what_1L_chr %in% c("barplot", "density", "histogram", 
            "donutchart", "pie", "ecdf", "errorplot", "line", 
            "qqplot", "scatter", "scatterhist", "stripchart", 
            "violin")) {
            if ((what_1L_chr %in% c("barplot", "qqplot", "stripchart", 
                "violin", "donutchart", "pie") & identical(by_1L_chr, 
                character(0)))) {
                var_1L_chr <- x_1L_chr
            }
            else {
                var_1L_chr <- by_1L_chr
            }
        }
        else {
            var_1L_chr <- x_1L_chr
        }
        if (!identical(var_1L_chr, character(0))) {
            colour_1L_int <- pick_1L_int <- data_xx %>% dplyr::pull(!!rlang::sym(var_1L_chr)) %>% 
                unique() %>% length()
        }
    }
    if (what_1L_chr %in% c("balloonplot") & !fill_single_1L_lgl) {
        colour_1L_int <- 3
    }
    if (what_1L_chr %in% c("scatter") & identical(by_1L_chr, 
        character(0))) {
        colour_1L_int <- 2
    }
    colour_codes_chr <- get_colour_codes(colour_1L_int = colour_1L_int, 
        manual_chr = colours_chr, pick_1L_int = pick_1L_int, 
        single_1L_lgl = FALSE, style_1L_chr = style_1L_chr, type_1L_chr = type_1L_chr)
    if (what_1L_chr %in% c("barplot", "boxplot", "dotplot", "paired") & 
        identical(by_1L_chr, character(0))) {
        by_1L_chr <- x_1L_chr
    }
    if (!("palette" %in% names(custom_args_ls)) & !fill_single_1L_lgl & 
        !(type_1L_chr == "manual" & length(colours_chr) == 1)) {
        args_ls <- append(args_ls, list(palette = colour_codes_chr))
    }
    if (what_1L_chr %in% c("balloonplot") | fill_single_1L_lgl | 
        (identical(by_1L_chr, character(0)) & !what_1L_chr %in% 
            c("donutchart", "pie"))) {
        fill_1L_chr <- ifelse(what_1L_chr %in% c("balloonplot") & 
            !fill_single_1L_lgl, by_1L_chr, colour_codes_chr[1])
    }
    else {
        fill_1L_chr <- ifelse(what_1L_chr %in% c("donutchart", 
            "pie") & identical(by_1L_chr, character(0)), x_1L_chr, 
            by_1L_chr)
    }
    if (!fill_single_1L_lgl & !("fill" %in% names(custom_args_ls))) {
        if (what_1L_chr %in% c("barplot", "boxplot")) {
            line_1L_chr <- ifelse(!identical(by_1L_chr, character(0)), 
                by_1L_chr, x_1L_chr)
        }
        if (what_1L_chr %in% c("density", "histogram", "dotchart", 
            "ecdf", "errorplot", "qqplot", "scatter", "stripchart", 
            "violin", "baloonplot") & !identical(by_1L_chr, character(0))) {
            line_1L_chr <- by_1L_chr
        }
        else {
            if (what_1L_chr %in% c("dotchart")) {
                line_1L_chr <- x_1L_chr
            }
        }
        if (what_1L_chr %in% c("ecdf", "qqplot", "scatter", "scatterhist", 
            "stripchart", "violin", "errorplot") & identical(by_1L_chr, 
            character(0))) {
            line_1L_chr <- ifelse(what_1L_chr %in% c("stripchart", 
                "violin"), ifelse((type_1L_chr == "manual" & 
                length(colours_chr) == 1), colour_codes_chr[1], 
                x_1L_chr), colour_codes_chr[1])
        }
    }
    else {
        if ("fill" %in% names(custom_args_ls)) {
            line_1L_chr <- custom_args_ls$fill
        }
        else {
            line_1L_chr <- colour_codes_chr[1]
        }
    }
    if (!"add.params" %in% names(custom_args_ls) & what_1L_chr %in% 
        c("scatter") & identical(by_1L_chr, character(0))) {
        if ("add" %in% names(custom_args_ls)) {
            if (custom_args_ls$add %in% c("loess", "reg.line")) {
                args_ls <- list(add.params = list(color = colour_codes_chr[max(2, 
                  length(colour_codes_chr))], fill = "lightgray")) %>% 
                  append(args_ls)
            }
        }
    }
    if (!"bins" %in% names(custom_args_ls) & what_1L_chr %in% 
        "histogram") {
        args_ls <- list(bins = min(data_xx %>% dplyr::pull(!!rlang::sym(x_1L_chr)) %>% 
            purrr::discard(is.na) %>% unique() %>% length(), 
            30)) %>% append(args_ls)
    }
    if (!"color" %in% names(custom_args_ls)) {
        args_ls <- list(color = ifelse(what_1L_chr %in% c("dotchart", 
            "line", "paired", "scatterhist") & !identical(by_1L_chr, 
            character(0)), by_1L_chr, ifelse(what_1L_chr %in% 
            c("line"), colour_codes_chr[1], line_1L_chr))) %>% 
            append(args_ls)
    }
    if (!"fill" %in% names(custom_args_ls) & !what_1L_chr %in% 
        c("boxplot", "errorplot", "paired", "qqplot", "scatterhist", 
            "stripchart", "violin")) {
        args_ls <- list(fill = fill_1L_chr) %>% append(args_ls)
    }
    if (!"group" %in% names(custom_args_ls) & what_1L_chr %in% 
        c("dotchart") & !identical(by_1L_chr, character(0))) {
        args_ls <- list(group = by_1L_chr) %>% append(args_ls)
    }
    if (!"line.color" %in% names(custom_args_ls) & what_1L_chr %in% 
        c("paired")) {
        args_ls <- list(line.color = line_1L_chr) %>% append(args_ls)
    }
    if (!"linetype" %in% names(custom_args_ls) & what_1L_chr %in% 
        c("ecdf", "line") & !identical(by_1L_chr, character(0))) {
        args_ls <- list(linetype = by_1L_chr) %>% append(args_ls)
    }
    if (!"margin.params" %in% names(custom_args_ls) & what_1L_chr %in% 
        c("scatterhist")) {
        if (!identical(by_1L_chr, character(0))) {
            args_ls <- list(margin.params = list(fill = by_1L_chr, 
                color = line_1L_chr)) %>% append(args_ls)
        }
        else {
            args_ls <- list(margin.params = list(fill = line_1L_chr)) %>% 
                append(args_ls)
        }
    }
    if (!"position" %in% names(custom_args_ls)) {
        if (what_1L_chr %in% c("barplot") & is.null(position_xx)) {
            position_xx <- ggplot2::position_dodge()
        }
        if (!is.null(position_xx)) {
            args_ls <- list(position = position_xx) %>% append(args_ls)
        }
    }
    if (!"shape" %in% names(custom_args_ls) & what_1L_chr %in% 
        c("line") & !identical(by_1L_chr, character(0))) {
        args_ls <- list(shape = by_1L_chr) %>% append(args_ls)
    }
    if (!"title" %in% names(custom_args_ls) & !identical(title_1L_chr, 
        character(0))) {
        args_ls <- list(title = title_1L_chr) %>% append(args_ls)
    }
    if (!"xlab" %in% names(custom_args_ls) & (what_1L_chr %in% 
        c("paired") | !identical(x_label_1L_chr, character(0)))) {
        args_ls <- list(xlab = ifelse(what_1L_chr %in% c("paired") & 
            identical(y_label_1L_chr, character(0)), x_1L_chr, 
            x_label_1L_chr)) %>% append(args_ls)
    }
    if (!"ylab" %in% names(custom_args_ls) & (what_1L_chr %in% 
        c("barplot", "paired", "qqplot") | !identical(y_label_1L_chr, 
        character(0)))) {
        args_ls <- list(ylab = ifelse(what_1L_chr %in% c("qqplot") & 
            identical(y_label_1L_chr, character(0)), x_1L_chr, 
            ifelse(what_1L_chr %in% c("paired") & identical(y_label_1L_chr, 
                character(0)), y_1L_chr, ifelse(what_1L_chr %in% 
                c("barplot") & identical(y_1L_chr, character(0)) & 
                identical(y_label_1L_chr, character(0)), "Count", 
                ifelse(identical(y_label_1L_chr, character(0)), 
                  "", y_label_1L_chr))))) %>% append(args_ls)
    }
    if ((what_1L_chr %in% c("donutchart", "pie") & identical(by_1L_chr, 
        character(0)))) {
        args_ls <- append(args_ls, list(x = "Freq"))
    }
    else {
        if (!identical(x_1L_chr, character(0)) & !"x" %in% names(custom_args_ls)) {
            args_ls <- append(args_ls, list(x = x_1L_chr))
        }
    }
    if (!"y" %in% names(custom_args_ls) & (!identical(y_1L_chr, 
        character(0)) | (what_1L_chr %in% c("barplot", "histogram") & 
        identical(y_1L_chr, character(0))))) {
        if (what_1L_chr %in% c("barplot") & identical(y_1L_chr, 
            character(0))) {
            args_ls <- append(args_ls, list(y = "Freq"))
        }
        else {
            if (what_1L_chr %in% c("histogram") & identical(y_1L_chr, 
                character(0))) {
                args_ls <- append(args_ls, list(y = ifelse(as_percent_1L_lgl, 
                  "density", "count")))
            }
            else {
                args_ls <- append(args_ls, list(y = y_1L_chr))
            }
        }
    }
    if (!identical(recode_lup_r3, ready4show::ready4show_correspondences())) {
        if (!is.numeric(data_xx %>% dplyr::pull(!!rlang::sym(x_1L_chr)))) {
            data_xx <- data_xx %>% dplyr::mutate(`:=`(!!rlang::sym(x_1L_chr), 
                recode_lup_r3 %>% ready4show::manufacture.ready4show_correspondences(data_xx %>% 
                  dplyr::select(!!rlang::sym(x_1L_chr)), flatten_1L_lgl = TRUE)))
        }
        if (!identical(by_1L_chr, character(0))) {
            if (!is.numeric(data_xx %>% dplyr::pull(!!rlang::sym(by_1L_chr)))) {
                data_xx <- data_xx %>% dplyr::mutate(`:=`(!!rlang::sym(by_1L_chr), 
                  recode_lup_r3 %>% ready4show::manufacture.ready4show_correspondences(data_xx %>% 
                    dplyr::select(!!rlang::sym(by_1L_chr)), flatten_1L_lgl = TRUE)))
            }
        }
    }
    if ((what_1L_chr %in% c("donutchart", "pie") & identical(by_1L_chr, 
        character(0))) | (what_1L_chr %in% c("barplot") & identical(y_1L_chr, 
        character(0)))) {
        data_xx <- table(data_xx %>% dplyr::select(tidyselect::all_of(c(x_1L_chr, 
            by_1L_chr))), useNA = "ifany") %>% tibble::as_tibble() %>% 
            dplyr::rename(Freq = n)
        if (drop_missing_1L_lgl) {
            data_xx <- tidyr::drop_na(data_xx, tidyselect::any_of(c(x_1L_chr, 
                by_1L_chr, "Freq")))
        }
        new_by_1L_chr <- "Freq"
    }
    else {
        new_by_1L_chr <- ifelse(what_1L_chr %in% c("donutchart", 
            "pie"), x_1L_chr, by_1L_chr)
    }
    if (what_1L_chr %in% c("donutchart", "pie") & as_percent_1L_lgl) {
        data_xx <- data_xx %>% dplyr::mutate(`:=`(!!rlang::sym(new_by_1L_chr), 
            round(!!rlang::sym(new_by_1L_chr)/sum(!!rlang::sym(new_by_1L_chr)) * 
                100, 0)))
    }
    if (!"label" %in% names(custom_args_ls) & what_1L_chr %in% 
        c("donutchart", "pie") & as_percent_1L_lgl) {
        args_ls <- list(label = paste0(data_xx %>% dplyr::pull(!!rlang::sym(new_by_1L_chr)), 
            "%")) %>% append(args_ls)
    }
    if (what_1L_chr %in% "balloonplot" & !fill_single_1L_lgl) {
        palette_chr <- args_ls$palette
        args_ls$palette <- NULL
    }
    plot_plt <- rlang::exec(plot_fn, data_xx, !!!args_ls)
    if (as_percent_1L_lgl) {
        if (what_1L_chr %in% c("barplot")) {
            plot_plt <- plot_plt + ggplot2::aes(y = !!rlang::sym(new_by_1L_chr)/sum(!!rlang::sym(new_by_1L_chr)))
        }
        if (!what_1L_chr %in% c("donutchart", "pie")) {
            plot_plt <- plot_plt + ggplot2::scale_y_continuous(labels = scales::label_percent()) + 
                ggplot2::labs(y = y_label_1L_chr)
        }
    }
    if (what_1L_chr %in% "balloonplot" & !fill_single_1L_lgl) {
        plot_plt <- plot_plt + ggpubr::gradient_fill(palette_chr)
    }
    if (!identical(label_fill_1L_chr, character(0))) {
        plot_plt <- plot_plt + ggplot2::labs(fill = label_fill_1L_chr, 
            color = label_fill_1L_chr, shape = label_fill_1L_chr, 
            linetype = label_fill_1L_chr)
    }
    if (drop_legend_1L_lgl | fill_single_1L_lgl & !what_1L_chr %in% 
        c("balloonplot")) {
        plot_plt <- plot_plt + ggplot2::theme(legend.position = "none")
    }
    if (drop_ticks_1L_lgl) {
        plot_plt <- plot_plt + ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
            axis.ticks.x = ggplot2::element_blank())
    }
    return(plot_plt)
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
