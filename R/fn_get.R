#' Get file from dataverse
#' @description get_file_from_dv() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get file from dataverse. Function argument ds_ui_1L_chr specifies the where to look for the required object. The function is called for its side effects and does not return a value.
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
#' @description get_fl_meta_from_dv_ls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get file meta from dataverse list. Function argument ds_ls specifies the where to look for the required object. The function returns Metadata (an output object of multiple potential types).
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
#' @description get_fl_nms_of_types() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get file names of types. Function argument fl_nms_chr specifies the where to look for the required object. The function returns Subset of file names (a character vector).
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
#' @description get_local_path_to_dv_data() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get local path to dataverse data. Function argument save_dir_path_1L_chr specifies the where to look for the required object. The function returns Path (a character vector).
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
#' Get ready4 S3 from dataverse comma separated variables file
#' @description get_r3_from_dv_csv() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get ready4 s3 from dataverse comma separated variables file. Function argument file_name_chr specifies the where to look for the required object. The function returns Tibble ready4 S3 (a ready4 S3 extension of tibble).
#' @param file_name_chr File name (a character vector)
#' @param data_repo_db_ui_chr Data repository database user interface (a character vector)
#' @param data_repo_ui_chr Data repository user interface (a character vector), Default: 'NA'
#' @param r3_fn Ready4 S3 (a function), Default: ready4use_imports
#' @return Tibble ready4 S3 (a ready4 S3 extension of tibble)
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
#' Get valid path character vector
#' @description get_valid_path_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get valid path character vector. Function argument x specifies the where to look for the required object. The function returns Valid path (a character vector).
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
