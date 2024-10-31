#' Get colour codes
#' @description get_colour_codes() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get colour codes. The function returns Colour codes (a character vector).
#' @param colour_1L_int Colour (an integer vector of length one), Default: 1
#' @param manual_chr Manual (a character vector), Default: c("#de2d26", "#fc9272")
#' @param pick_1L_int Pick (an integer vector of length one), Default: integer(0)
#' @param single_1L_lgl Single (a logical vector of length one), Default: FALSE
#' @param style_1L_chr Style (a character vector of length one), Default: get_styles()
#' @param type_1L_chr Type (a character vector of length one), Default: c("ggsci", "manual", "viridis")
#' @return Colour codes (a character vector)
#' @rdname get_colour_codes
#' @export 
#' @importFrom ggpubr get_palette
#' @importFrom viridis viridis
#' @importFrom purrr discard
#' @keywords internal
get_colour_codes <- function (colour_1L_int = 1, manual_chr = c("#de2d26", "#fc9272"), 
    pick_1L_int = integer(0), single_1L_lgl = FALSE, style_1L_chr = get_styles(), 
    type_1L_chr = c("ggsci", "manual", "viridis")) 
{
    style_1L_chr <- match.arg(style_1L_chr)
    type_1L_chr <- match.arg(type_1L_chr)
    if (identical(pick_1L_int, integer(0))) {
        pick_1L_int <- colour_1L_int
    }
    if (type_1L_chr == "manual") {
        colour_codes_chr <- ggpubr::get_palette(manual_chr, k = colour_1L_int)
    }
    else {
        defaults_chr <- get_styles(type_1L_chr)
        if (!style_1L_chr %in% defaults_chr) {
            style_1L_chr <- defaults_chr[1]
        }
    }
    if (type_1L_chr == "ggsci") {
        colour_codes_chr <- ggpubr::get_palette(style_1L_chr, 
            k = colour_1L_int)
    }
    if (type_1L_chr == "viridis") 
        colour_codes_chr <- viridis::viridis(colour_1L_int, option = style_1L_chr)
    if (single_1L_lgl) {
        colour_codes_chr <- colour_codes_chr[pick_1L_int]
    }
    else {
        colour_codes_chr <- colour_codes_chr[1:pick_1L_int]
    }
    colour_codes_chr <- colour_codes_chr %>% purrr::discard(is.na)
    return(colour_codes_chr)
}
#' Get drop offs
#' @description get_drop_offs() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get drop offs. The function returns Drop offs (a character vector).
#' @param X_Ready4useDyad PARAM_DESCRIPTION, Default: Ready4useDyad()
#' @param condition_1L_chr Condition (a character vector of length one), Default: '>1'
#' @param uid_var_nm_1L_chr Unique identifier variable name (a character vector of length one), Default: 'uid_chr'
#' @return Drop offs (a character vector)
#' @rdname get_drop_offs
#' @export 
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @importFrom purrr map_int map_lgl
#' @keywords internal
get_drop_offs <- function (X_Ready4useDyad = Ready4useDyad(), condition_1L_chr = ">1", 
    uid_var_nm_1L_chr = "uid_chr") 
{
    all_ids_chr <- X_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(uid_var_nm_1L_chr))
    unique_ids_chr <- unique(all_ids_chr)
    counts_int <- unique_ids_chr %>% purrr::map_int(~sum(all_ids_chr == 
        .x))
    pass_test_chr <- unique_ids_chr[purrr::map_lgl(counts_int, 
        ~eval(parse(text = paste0(.x, condition_1L_chr))))]
    drop_offs_chr <- setdiff(all_ids_chr %>% unique(), pass_test_chr)
    return(drop_offs_chr)
}
#' Get file from dataverse
#' @description get_file_from_dv() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get file from dataverse. The function is called for its side effects and does not return a value.
#' @param ds_ui_1L_chr Dataset user interface (a character vector of length one)
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param save_fmt_1L_chr Save format (a character vector of length one)
#' @param repo_fl_fmt_1L_chr Repository file format (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param save_type_1L_chr Save type (a character vector of length one), Default: 'original'
#' @param save_dir_path_1L_chr Save directory path (a character vector of length one), Default: ''
#' @param read_fn Read (a function)
#' @param unlink_1L_lgl Unlink (a logical vector of length one), Default: T
#' @return file_xxx (An object)
#' @rdname get_file_from_dv
#' @export 
#' @importFrom ready4 make_local_path_to_dv_data write_dv_fl_to_loc
#' @importFrom rlang exec
#' @keywords internal
get_file_from_dv <- function (ds_ui_1L_chr, fl_nm_1L_chr, save_fmt_1L_chr, repo_fl_fmt_1L_chr, 
    consent_1L_chr = "", consent_indcs_int = 1L, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), 
    options_chr = c("Y", "N"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), 
    save_type_1L_chr = "original", save_dir_path_1L_chr = "", 
    read_fn, unlink_1L_lgl = T) 
{
    destination_path_chr <- ifelse(unlink_1L_lgl, tempfile(), 
        ready4::make_local_path_to_dv_data(save_dir_path_1L_chr = save_dir_path_1L_chr, 
            fl_nm_1L_chr = fl_nm_1L_chr, save_fmt_1L_chr = save_fmt_1L_chr))
    ready4::write_dv_fl_to_loc(consent_1L_chr = ifelse(unlink_1L_lgl, 
        options_chr[consent_indcs_int][1], consent_1L_chr), consent_indcs_int = consent_indcs_int, 
        ds_ui_1L_chr = ds_ui_1L_chr, fl_nm_1L_chr = fl_nm_1L_chr, 
        repo_fl_fmt_1L_chr = repo_fl_fmt_1L_chr, key_1L_chr = key_1L_chr, 
        options_chr = options_chr, server_1L_chr = server_1L_chr, 
        save_type_1L_chr = save_type_1L_chr, dest_path_1L_chr = destination_path_chr)
    file_xxx <- rlang::exec(read_fn, destination_path_chr, stringsAsFactors = F)
    if (unlink_1L_lgl) 
        unlink(destination_path_chr)
    file_xxx
    return(file_xxx)
}
#' Get file meta from dataverse list
#' @description get_fl_meta_from_dv_ls() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get file meta from dataverse list. The function returns Metadata (an output object of multiple potential types).
#' @param ds_ls Dataset (a list)
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param nms_chr Names (a character vector), Default: 'NA'
#' @param type_1L_chr Type (a character vector of length one), Default: 'description'
#' @return Metadata (an output object of multiple potential types)
#' @rdname get_fl_meta_from_dv_ls
#' @export 
#' @importFrom purrr map_chr
#' @importFrom tibble as_tibble
#' @keywords internal
get_fl_meta_from_dv_ls <- function (ds_ls, fl_nm_1L_chr, nms_chr = NA_character_, type_1L_chr = "description") 
{
    if (is.na(nms_chr[1])) {
        nms_chr <- purrr::map_chr(ds_ls$files$filename, ~ifelse(is.na(.x), 
            .y, .x))
    }
    if (fl_nm_1L_chr %in% nms_chr) {
        metadata_xx <- get_from_lup_obj(ds_ls$files[, names(ds_ls$files) %>% 
            unique()] %>% tibble::as_tibble(), match_var_nm_1L_chr = "filename", 
            match_value_xx = fl_nm_1L_chr, target_var_nm_1L_chr = type_1L_chr, 
            evaluate_1L_lgl = F)
    }
    else {
        metadata_xx <- NA_character_
    }
    return(metadata_xx)
}
#' Get file names of types
#' @description get_fl_nms_of_types() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get file names of types. The function returns Subset of file names (a character vector).
#' @param fl_nms_chr File names (a character vector)
#' @param types_chr Types (a character vector)
#' @return Subset of file names (a character vector)
#' @rdname get_fl_nms_of_types
#' @export 
#' @importFrom purrr keep map_lgl
#' @keywords internal
get_fl_nms_of_types <- function (fl_nms_chr, types_chr) 
{
    subset_of_fl_nms_chr <- purrr::keep(fl_nms_chr, ~{
        fl_nm_1L_chr <- .x
        types_chr %>% purrr::map_lgl(~endsWith(fl_nm_1L_chr, 
            .x)) %>% any()
    })
    return(subset_of_fl_nms_chr)
}
#' Get journal palette function
#' @description get_journal_palette_fn() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get journal palette function. The function returns Journal palette (a function).
#' @param type_1L_chr Type (a character vector of length one), Default: c("colour", "fill")
#' @param what_1L_chr What (a character vector of length one), Default: 'lancet'
#' @return Journal palette (a function)
#' @rdname get_journal_palette_fn
#' @export 
#' @importFrom ggsci scale_colour_aaas scale_colour_bmj scale_colour_bs5 scale_colour_cosmic scale_colour_d3 scale_colour_flatui scale_colour_frontiers scale_colour_futurama scale_colour_gsea scale_colour_igv scale_colour_jama scale_colour_jco scale_colour_lancet scale_colour_locuszoom scale_colour_material scale_colour_nejm scale_colour_npg scale_colour_observable scale_colour_rickandmorty scale_colour_simpsons scale_colour_startrek scale_colour_tron scale_colour_tw3 scale_colour_uchicago scale_colour_ucscgb scale_fill_aaas scale_fill_bmj scale_fill_bs5 scale_fill_cosmic scale_fill_d3 scale_fill_flatui scale_fill_frontiers scale_fill_futurama scale_fill_gsea scale_fill_igv scale_fill_jama scale_fill_jco scale_fill_lancet scale_fill_locuszoom scale_fill_material scale_fill_nejm scale_fill_npg scale_fill_observable scale_fill_rickandmorty scale_fill_simpsons scale_fill_startrek scale_fill_tron scale_fill_tw3 scale_fill_uchicago scale_fill_ucscgb
#' @importFrom purrr pluck
get_journal_palette_fn <- function (type_1L_chr = c("colour", "fill"), what_1L_chr = "lancet") 
{
    type_1L_chr <- match.arg(type_1L_chr)
    options_ls <- list(scale_colour_aaas = ggsci::scale_colour_aaas, 
        scale_colour_bmj = ggsci::scale_colour_bmj, scale_colour_bs5 = ggsci::scale_colour_bs5, 
        scale_colour_cosmic = ggsci::scale_colour_cosmic, scale_colour_d3 = ggsci::scale_colour_d3, 
        scale_colour_flatui = ggsci::scale_colour_flatui, scale_colour_frontiers = ggsci::scale_colour_frontiers, 
        scale_colour_futurama = ggsci::scale_colour_futurama, 
        scale_colour_gsea = ggsci::scale_colour_gsea, scale_colour_igv = ggsci::scale_colour_igv, 
        scale_colour_jama = ggsci::scale_colour_jama, scale_colour_jco = ggsci::scale_colour_jco, 
        scale_colour_lancet = ggsci::scale_colour_lancet, scale_colour_locuszoom = ggsci::scale_colour_locuszoom, 
        scale_colour_material = ggsci::scale_colour_material, 
        scale_colour_nejm = ggsci::scale_colour_nejm, scale_colour_npg = ggsci::scale_colour_npg, 
        scale_colour_observable = ggsci::scale_colour_observable, 
        scale_colour_rickandmorty = ggsci::scale_colour_rickandmorty, 
        scale_colour_simpsons = ggsci::scale_colour_simpsons, 
        scale_colour_startrek = ggsci::scale_colour_startrek, 
        scale_colour_tron = ggsci::scale_colour_tron, scale_colour_tw3 = ggsci::scale_colour_tw3, 
        scale_colour_uchicago = ggsci::scale_colour_uchicago, 
        scale_colour_ucscgb = ggsci::scale_colour_ucscgb, scale_fill_aaas = ggsci::scale_fill_aaas, 
        scale_fill_bmj = ggsci::scale_fill_bmj, scale_fill_bs5 = ggsci::scale_fill_bs5, 
        scale_fill_cosmic = ggsci::scale_fill_cosmic, scale_fill_d3 = ggsci::scale_fill_d3, 
        scale_fill_flatui = ggsci::scale_fill_flatui, scale_fill_frontiers = ggsci::scale_fill_frontiers, 
        scale_fill_futurama = ggsci::scale_fill_futurama, scale_fill_gsea = ggsci::scale_fill_gsea, 
        scale_fill_igv = ggsci::scale_fill_igv, scale_fill_jama = ggsci::scale_fill_jama, 
        scale_fill_jco = ggsci::scale_fill_jco, scale_fill_lancet = ggsci::scale_fill_lancet, 
        scale_fill_locuszoom = ggsci::scale_fill_locuszoom, scale_fill_material = ggsci::scale_fill_material, 
        scale_fill_nejm = ggsci::scale_fill_nejm, scale_fill_npg = ggsci::scale_fill_npg, 
        scale_fill_observable = ggsci::scale_fill_observable, 
        scale_fill_rickandmorty = ggsci::scale_fill_rickandmorty, 
        scale_fill_simpsons = ggsci::scale_fill_simpsons, scale_fill_startrek = ggsci::scale_fill_startrek, 
        scale_fill_tron = ggsci::scale_fill_tron, scale_fill_tw3 = ggsci::scale_fill_tw3, 
        scale_fill_uchicago = ggsci::scale_fill_uchicago, scale_fill_ucscgb = ggsci::scale_fill_ucscgb)
    journal_palette_fn <- options_ls %>% purrr::pluck(paste0("scale_", 
        type_1L_chr, "_", what_1L_chr))
    return(journal_palette_fn)
}
#' Get journal plot function
#' @description get_journal_plot_fn() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get journal plot function. The function returns Journal plot (an output object of multiple potential types).
#' @param what_1L_chr What (a character vector of length one), Default: 'barplot'
#' @param pkg_1L_chr Package (a character vector of length one), Default: 'ggpubr'
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'gg'
#' @return Journal plot (an output object of multiple potential types)
#' @rdname get_journal_plot_fn
#' @export 
#' @importFrom ggpubr ggbarplot ggballoonplot ggboxplot ggdensity ggdonutchart ggdotchart ggdotplot ggecdf ggerrorplot gghistogram ggline ggpaired ggpie ggqqplot ggscatter ggscatterhist ggstripchart ggviolin
#' @importFrom stringr str_sub
#' @importFrom purrr pluck
#' @keywords internal
get_journal_plot_fn <- function (what_1L_chr = "barplot", pkg_1L_chr = "ggpubr", prefix_1L_chr = "gg") 
{
    options_ls <- list(ggbarplot = ggpubr::ggbarplot, ggballoonplot = ggpubr::ggballoonplot, 
        ggboxplot = ggpubr::ggboxplot, ggdensity = ggpubr::ggdensity, 
        ggdonutchart = ggpubr::ggdonutchart, ggdotchart = ggpubr::ggdotchart, 
        ggdotplot = ggpubr::ggdotplot, ggecdf = ggpubr::ggecdf, 
        ggerrorplot = ggpubr::ggerrorplot, gghistogram = ggpubr::gghistogram, 
        ggline = ggpubr::ggline, ggpaired = ggpubr::ggpaired, 
        ggpie = ggpubr::ggpie, ggqqplot = ggpubr::ggqqplot, ggscatter = ggpubr::ggscatter, 
        ggscatterhist = ggpubr::ggscatterhist, ggstripchart = ggpubr::ggstripchart, 
        ggviolin = ggpubr::ggviolin)
    if (what_1L_chr == "names") {
        journal_plot_xx <- names(options_ls) %>% stringr::str_sub(start = nchar(prefix_1L_chr) + 
            1)
    }
    else {
        journal_plot_xx <- options_ls %>% purrr::pluck(paste0(prefix_1L_chr, 
            what_1L_chr))
    }
    return(journal_plot_xx)
}
#' Get local path to dataverse data
#' @description get_local_path_to_dv_data() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get local path to dataverse data. The function returns Path (a character vector).
#' @param save_dir_path_1L_chr Save directory path (a character vector of length one)
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param save_fmt_1L_chr Save format (a character vector of length one)
#' @return Path (a character vector)
#' @rdname get_local_path_to_dv_data
#' @export 
#' @importFrom lifecycle deprecate_soft
#' @keywords internal
get_local_path_to_dv_data <- function (save_dir_path_1L_chr, fl_nm_1L_chr, save_fmt_1L_chr) 
{
    lifecycle::deprecate_soft("0.0.0.9149", "get_local_path_to_dv_data()", 
        "ready4::make_local_path_to_dv_data()")
    path_chr <- paste0(ifelse(save_dir_path_1L_chr != "", paste0(save_dir_path_1L_chr, 
        "/"), ""), fl_nm_1L_chr, save_fmt_1L_chr)
    return(path_chr)
}
#' Get ready4 submodule from dataverse comma separated variables file
#' @description get_r3_from_dv_csv() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get ready4 submodule from dataverse comma separated variables file. The function returns Tibble ready4 submodule (a ready4 submodule extension of tibble).
#' @param file_name_chr File name (a character vector)
#' @param data_repo_db_ui_chr Data repository database user interface (a character vector)
#' @param data_repo_ui_chr Data repository user interface (a character vector), Default: 'NA'
#' @param r3_fn Ready4 submodule (a function), Default: ready4use_imports
#' @return Tibble ready4 submodule (a ready4 submodule extension of tibble)
#' @rdname get_r3_from_dv_csv
#' @export 
#' @importFrom tibble tibble
get_r3_from_dv_csv <- function (file_name_chr, data_repo_db_ui_chr, data_repo_ui_chr = NA_character_, 
    r3_fn = ready4use_imports) 
{
    tb_r3 <- tibble::tibble(file_type_chr = ".csv", file_name_chr = file_name_chr, 
        data_repo_chr = NA_character_, data_repo_ui_chr = data_repo_ui_chr, 
        data_repo_db_ui_chr = data_repo_db_ui_chr, data_repo_file_ext_chr = ".tab", 
        data_repo_save_type_chr = "original") %>% ready4use_dataverses() %>% 
        procure() %>% make_r3_from_csv_tb(r3_fn)
    return(tb_r3)
}
#' Get raw datasets
#' @description get_raw_dss() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get raw datasets. The function returns Dataset (a list).
#' @param paths_chr Paths (a character vector)
#' @param names_chr Names (a character vector)
#' @param read_fn Read (a function), Default: read.csv
#' @return Dataset (a list)
#' @rdname get_raw_dss
#' @export 
#' @importFrom assertthat assert_that are_equal
#' @importFrom purrr map
#' @importFrom stats setNames
#' @keywords internal
get_raw_dss <- function (paths_chr, names_chr, read_fn = read.csv) 
{
    assertthat::assert_that(assertthat::are_equal(length(paths_chr), 
        length(names_chr)), msg = "names_chr must be same length as paths_chr")
    ds_ls <- purrr::map(paths_chr, ~read_fn(.x)) %>% stats::setNames(names_chr)
    return(ds_ls)
}
#' Get reference descriptions
#' @description get_reference_descs() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get reference descriptions. The function returns Reference descriptions (a character vector).
#' @param correspondences_ls Correspondences (a list)
#' @param correspondences_r3 Correspondences (a ready4 submodule), Default: ready4show::ready4show_correspondences()
#' @return Reference descriptions (a character vector)
#' @rdname get_reference_descs
#' @export 
#' @importFrom ready4show ready4show_correspondences
#' @importFrom purrr reduce
#' @importFrom dplyr pull
#' @keywords internal
get_reference_descs <- function (correspondences_ls, correspondences_r3 = ready4show::ready4show_correspondences()) 
{
    reference_descs_chr <- purrr::reduce(correspondences_ls, 
        .init = correspondences_r3, ~rbind(.x, .y)) %>% dplyr::pull(new_nms_chr) %>% 
        unique()
    return(reference_descs_chr)
}
#' Get styles
#' @description get_styles() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get styles. The function returns Styles (a character vector).
#' @param what_1L_chr What (a character vector of length one), Default: c("all", "ggsci", "viridis")
#' @param sort_1L_lgl Sort (a logical vector of length one), Default: FALSE
#' @return Styles (a character vector)
#' @rdname get_styles
#' @export 
get_styles <- function (what_1L_chr = c("all", "ggsci", "viridis"), sort_1L_lgl = FALSE) 
{
    what_1L_chr <- match.arg(what_1L_chr)
    styles_chr <- character(0)
    if (what_1L_chr %in% c("all", "ggsci")) {
        styles_chr <- c(styles_chr, c("npg", "aaas", "lancet", 
            "jco", "nejm", "ucscgb", "uchicago", "d3", "futurama", 
            "igv", "locuszoom", "rickandmorty", "startrek", "simpsons", 
            "tron"))
    }
    if (what_1L_chr %in% c("all", "viridis")) {
        styles_chr <- c(styles_chr, c("magma", "A", "inferno", 
            "B", "plasma", "C", "viridis", "D", "cividis", "E", 
            "rocket", "F", "mako", "G", "turbo", "H"))
    }
    if (sort_1L_lgl) {
        styles_chr <- sort(styles_chr)
    }
    return(styles_chr)
}
#' Get valid path character vector
#' @description get_valid_path_chr() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get valid path character vector. The function returns Valid path (a character vector).
#' @param x An object
#' @return Valid path (a character vector)
#' @rdname get_valid_path_chr
#' @export 
#' @keywords internal
get_valid_path_chr <- function (x) 
{
    assert_file_exists(x)
    valid_path_chr <- x
    return(valid_path_chr)
}
#' Get variables with condition
#' @description get_vars_with_cdn() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get variables with condition. The function returns Variables (a character vector).
#' @param data_tb Data (a tibble)
#' @param cdn_fn Condition (a function)
#' @return Variables (a character vector)
#' @rdname get_vars_with_cdn
#' @export 
#' @importFrom purrr map_lgl
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @keywords internal
get_vars_with_cdn <- function (data_tb, cdn_fn) 
{
    vars_chr <- names(data_tb)[names(data_tb) %>% purrr::map_lgl(~cdn_fn(data_tb %>% 
        dplyr::pull(!!rlang::sym(.x))))]
    return(vars_chr)
}
