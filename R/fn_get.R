#' Get file from dataverse
#' @description get_file_from_dv() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get a file from dv. Function argument database_ui_chr specifies the where to look for the required object.NA
#' @param database_ui_chr Database ui (a character vector of length 1)
#' @param filename_chr Filename (a character vector of length 1)
#' @param save_format_chr Save format (a character vector of length 1)
#' @param repo_file_format PARAM_DESCRIPTION
#' @param dataverse_chr Dataverse (a character vector of length 1), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param save_type_chr Save type (a character vector of length 1), Default: 'original'
#' @param save_dir_path_chr Save directory path (a character vector of length 1), Default: ''
#' @param read_fn Read (a function)
#' @param unlink_lgl Unlink (a logical vector of length 1), Default: T
#' @return NULL
#' @rdname get_file_from_dv
#' @export 
#' @importFrom rlang exec
#' @keywords internal
get_file_from_dv <- function (database_ui_chr, filename_chr, save_format_chr, repo_file_format, 
    dataverse_chr = Sys.getenv("DATAVERSE_SERVER"), save_type_chr = "original", 
    save_dir_path_chr = "", read_fn, unlink_lgl = T) 
{
    destination_path_chr <- ifelse(unlink_lgl, tempfile(), get_local_path_to_dv_data(save_dir_path_chr = save_dir_path_chr, 
        filename_chr = filename_chr, save_format_chr = save_format_chr))
    write_dv_file_fl(database_ui_chr = database_ui_chr, filename_chr = filename_chr, 
        repo_file_format = repo_file_format, dataverse_chr = dataverse_chr, 
        save_type_chr = save_type_chr, destination_path_chr = destination_path_chr)
    file_xxx <- rlang::exec(read_fn, destination_path_chr, stringsAsFactors = F)
    if (unlink_lgl) 
        unlink(destination_path_chr)
    file_xxx
    return(file_xxx)
}
#' Get local path to dataverse data
#' @description get_local_path_to_dv_data() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get a local path to dv data. Function argument save_dir_path_chr specifies the where to look for the required object.The function returns path (a character vector of length 1).
#' @param save_dir_path_chr Save directory path (a character vector of length 1)
#' @param filename_chr Filename (a character vector of length 1)
#' @param save_format_chr Save format (a character vector of length 1)
#' @return Path (a character vector of length 1)
#' @rdname get_local_path_to_dv_data
#' @export 

#' @keywords internal
get_local_path_to_dv_data <- function (save_dir_path_chr, filename_chr, save_format_chr) 
{
    path_chr <- paste0(ifelse(save_dir_path_chr != "", paste0(save_dir_path_chr, 
        "/"), ""), filename_chr, save_format_chr)
    return(path_chr)
}
#' Get readyforwhatsnext S3 from dataverse csv
#' @description get_r3_from_dv_csv() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get a readyforwhatsnext S3 from dv csv. Function argument file_name_chr specifies the where to look for the required object.The function returns a tibble readyforwhatsnext s3 (a readyforwhatsnext s3 extension of tibble).
#' @param file_name_chr File name (a character vector of length 1)
#' @param data_repo_db_ui_chr Data repo database ui (a character vector of length 1)
#' @param data_repo_ui_chr Data repo ui (a character vector of length 1), Default: 'NA'
#' @param r3_fn Readyforwhatsnext S3 (a function), Default: ready4_all_import_lup
#' @return Tibble readyforwhatsnext S3 (a readyforwhatsnext S3 extension of tibble)
#' @rdname get_r3_from_dv_csv
#' @export 
#' @importFrom tibble tibble
#' @keywords internal
get_r3_from_dv_csv <- function (file_name_chr, data_repo_db_ui_chr, data_repo_ui_chr = NA_character_, 
    r3_fn = ready4_all_import_lup) 
{
    tb_r3 <- tibble::tibble(file_type = ".csv", file_name = file_name_chr, 
        data_repo = NA_character_, data_repo_ui = data_repo_ui_chr, 
        data_repo_db_ui = data_repo_db_ui_chr, data_repo_file_ext = ".tab", 
        data_repo_save_type = "original") %>% ready4_dv_import_lup() %>% 
        get_data() %>% make_r3_from_csv_tb(r3_fn)
    return(tb_r3)
}
#' Get valid path
#' @description get_valid_path_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get valid path. Function argument x specifies the where to look for the required object.The function returns valid path (a character vector of length 1).
#' @param x PARAM_DESCRIPTION
#' @return Valid path (a character vector of length 1)
#' @rdname get_valid_path_chr
#' @export 

#' @keywords internal
get_valid_path_chr <- function (x) 
{
    assert_file_exists(x)
    valid_path_chr <- x
    return(valid_path_chr)
}
