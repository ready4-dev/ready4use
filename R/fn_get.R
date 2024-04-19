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
