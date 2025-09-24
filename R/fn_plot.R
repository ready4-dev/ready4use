#' Plot for journal
#' @description plot_for_journal() is a Plot function that plots data. Specifically, this function implements an algorithm to plot for journal. The function returns Plot (a plot).
#' @param data_tb Data (a tibble)
#' @param as_percent_1L_lgl As percent (a logical vector of length one), Default: FALSE
#' @param by_1L_chr By (a character vector of length one), Default: character(0)
#' @param colours_chr Colours (a character vector), Default: c("#de2d26", "#fc9272")
#' @param drop_legend_1L_lgl Drop legend (a logical vector of length one), Default: FALSE
#' @param drop_missing_1L_lgl Drop missing (a logical vector of length one), Default: FALSE
#' @param drop_ticks_1L_lgl Drop ticks (a logical vector of length one), Default: FALSE
#' @param fill_single_1L_lgl Fill single (a logical vector of length one), Default: FALSE
#' @param flip_1L_lgl Flip (a logical vector of length one), Default: F
#' @param label_fill_1L_chr Label fill (a character vector of length one), Default: character(0)
#' @param line_1L_chr Line (a character vector of length one), Default: 'black'
#' @param position_xx Position (an output object of multiple potential types), Default: NULL
#' @param recode_lup_r3 Recode (a ready4 submodule extension of lookup table), Default: ready4show::ready4show_correspondences()
#' @param significance_1L_lgl Significance (a logical vector of length one), Default: F
#' @param significance_args_ls Significance arguments (a list), Default: list()
#' @param strict_1L_lgl Strict (a logical vector of length one), Default: FALSE
#' @param style_1L_chr Style (a character vector of length one), Default: get_styles()
#' @param title_1L_chr Title (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("ggsci", "manual", "unicol", "viridis")
#' @param validate_1L_lgl Validate (a logical vector of length one), Default: TRUE
#' @param x_1L_chr X (a character vector of length one), Default: character(0)
#' @param x_label_1L_chr X label (a character vector of length one), Default: character(0)
#' @param y_1L_chr Y (a character vector of length one), Default: character(0)
#' @param y_label_1L_chr Y label (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: get_journal_plot_fn("names")
#' @param ... Additional arguments
#' @return Plot (a plot)
#' @rdname plot_for_journal
#' @export 
#' @importFrom ready4show ready4show_correspondences manufacture.ready4show_correspondences
#' @importFrom dplyr select pull mutate rename group_by
#' @importFrom tidyselect any_of
#' @importFrom tidyr drop_na
#' @importFrom rlang sym exec
#' @importFrom purrr discard
#' @importFrom ggplot2 position_dodge scale_y_continuous labs aes after_stat theme element_blank
#' @importFrom tibble as_tibble
#' @importFrom scales label_percent
#' @importFrom ggpubr yscale gradient_fill
#' @importFrom ggsignif geom_signif
#' @keywords internal
plot_for_journal <- function (data_tb, as_percent_1L_lgl = FALSE, by_1L_chr = character(0), 
    colours_chr = c("#de2d26", "#fc9272"), drop_legend_1L_lgl = FALSE, 
    drop_missing_1L_lgl = FALSE, drop_ticks_1L_lgl = FALSE, fill_single_1L_lgl = FALSE, 
    flip_1L_lgl = F, label_fill_1L_chr = character(0), line_1L_chr = "black", 
    position_xx = NULL, recode_lup_r3 = ready4show::ready4show_correspondences(), 
    significance_1L_lgl = F, significance_args_ls = list(), strict_1L_lgl = FALSE, 
    style_1L_chr = get_styles(), title_1L_chr = character(0), 
    type_1L_chr = c("ggsci", "manual", "unicol", "viridis"), 
    validate_1L_lgl = TRUE, x_1L_chr = character(0), x_label_1L_chr = character(0), 
    y_1L_chr = character(0), y_label_1L_chr = character(0), what_1L_chr = get_journal_plot_fn("names"), 
    ...) 
{
    if (validate_1L_lgl) {
        style_1L_chr <- match.arg(style_1L_chr)
    }
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
    data_xx <- data_tb %>% dplyr::select(tidyselect::any_of(c(x_1L_chr, 
        y_1L_chr, by_1L_chr, extras_chr)))
    if (drop_missing_1L_lgl) {
        data_xx <- tidyr::drop_na(data_xx, tidyselect::any_of(c(x_1L_chr, 
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
        single_1L_lgl = FALSE, strict_1L_lgl = strict_1L_lgl, 
        style_1L_chr = style_1L_chr, type_1L_chr = type_1L_chr, 
        validate_1L_lgl = validate_1L_lgl)
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
                identical(y_label_1L_chr, character(0)), ifelse(as_percent_1L_lgl, 
                "", "Count"), ifelse(identical(y_label_1L_chr, 
                character(0)), "", y_label_1L_chr))))) %>% append(args_ls)
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
                args_ls <- append(args_ls, list(y = "count"))
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
        data_xx <- table(data_xx %>% dplyr::select(tidyselect::any_of(unique(c(x_1L_chr, 
            by_1L_chr)))), useNA = "ifany") %>% tibble::as_tibble() %>% 
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
    if (what_1L_chr %in% c("barplot") & as_percent_1L_lgl) {
        y_1L_chr <- ifelse(identical(y_1L_chr, character(0)), 
            "Freq", y_1L_chr)
        if (!identical(by_1L_chr, x_1L_chr) & !identical(by_1L_chr, 
            character(0))) {
            if (!flip_1L_lgl) {
                data_xx <- data_xx %>% dplyr::group_by(!!rlang::sym(by_1L_chr))
            }
            else {
                data_xx <- data_xx %>% dplyr::group_by(!!rlang::sym(x_1L_chr))
            }
        }
        data_xx <- data_xx %>% dplyr::mutate(Percent = (!!rlang::sym(y_1L_chr)/sum(!!rlang::sym(y_1L_chr))))
        args_ls$y <- "Percent"
    }
    if (what_1L_chr %in% c("donutchart", "pie") & as_percent_1L_lgl) {
        data_xx <- data_xx %>% dplyr::mutate(new_label_chr = paste0(round(!!rlang::sym(new_by_1L_chr)/sum(!!rlang::sym(new_by_1L_chr)) * 
            100, 0), "%"))
    }
    if (!"label" %in% names(custom_args_ls) & what_1L_chr %in% 
        c("donutchart", "pie") & as_percent_1L_lgl) {
        args_ls <- list(label = "new_label_chr") %>% append(args_ls)
    }
    if (what_1L_chr %in% "balloonplot" & !fill_single_1L_lgl) {
        palette_chr <- args_ls$palette
        args_ls$palette <- NULL
    }
    plot_plt <- rlang::exec(plot_fn, data_xx, !!!args_ls)
    if (as_percent_1L_lgl) {
        if (!what_1L_chr %in% c("donutchart", "pie", "histogram")) {
            plot_plt <- plot_plt + ggplot2::scale_y_continuous(labels = scales::label_percent()) + 
                ggplot2::labs(y = y_label_1L_chr)
        }
        if (what_1L_chr %in% "histogram" & ifelse(identical(y_1L_chr, 
            character(0)), T, !y_1L_chr %in% c("density", "..density.."))) {
            plot_plt <- plot_plt + ggplot2::aes(y = ggplot2::after_stat(width) * 
                (ggplot2::after_stat(density))) + ggpubr::yscale("percent", 
                .format = TRUE) + ggplot2::labs(y = y_label_1L_chr)
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
    if (significance_1L_lgl) {
        if (what_1L_chr %in% c("barplot")) {
            significance_args_ls <- append(list(as_percent_1L_lgl = as_percent_1L_lgl, 
                by_1L_chr = by_1L_chr, data_tb = data_tb, var_1L_chr = x_1L_chr), 
                significance_args_ls)
            plot_plt <- rlang::exec(add_significance, plot_plt, 
                !!!significance_args_ls)
        }
        else {
            plot_plt <- plot_plt + rlang::exec(ggsignif::geom_signif, 
                !!!significance_args_ls)
        }
    }
    return(plot_plt)
}
