#' write dataverse dataset
#' @description write_dv_ds() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write dataverse dataset. The function returns Dataset (a list).
#' @param ds_meta_ls Dataset meta (a list)
#' @param dev_pkg_nm_1L_chr Development package name (a character vector of length one), Default: deprecated()
#' @param dss_tb Datasets (a tibble)
#' @param dv_nm_1L_chr Dataverse name (a character vector of length one)
#' @param parent_dv_dir_1L_chr Parent dataverse directory (a character vector of length one)
#' @param paths_to_dirs_chr Paths to directories (a character vector)
#' @param inc_fl_types_chr Include file types (a character vector), Default: 'NA'
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Dataset (a list)
#' @rdname write_dv_ds
#' @export 
#' @importFrom lifecycle is_present deprecate_warn
#' @importFrom ready4 write_to_dv_with_wait
write_dv_ds <- function (ds_meta_ls, dev_pkg_nm_1L_chr = deprecated(), dss_tb, 
    dv_nm_1L_chr, parent_dv_dir_1L_chr, paths_to_dirs_chr, inc_fl_types_chr = NA_character_, 
    key_1L_chr = Sys.getenv("DATAVERSE_KEY"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (lifecycle::is_present(dev_pkg_nm_1L_chr)) {
        lifecycle::deprecate_warn("0.0.0.9153", "ready4use::write_dv_ds(dev_pkg_nm_1L_chr)")
    }
    ds_url_1L_chr <- add_ds_to_dv_repo(dv_1L_chr = dv_nm_1L_chr, 
        ds_meta_ls = ds_meta_ls)
    ds_ls <- ready4::write_to_dv_with_wait(dss_tb = dss_tb, dv_nm_1L_chr = dv_nm_1L_chr, 
        ds_url_1L_chr = ds_url_1L_chr, parent_dv_dir_1L_chr = parent_dv_dir_1L_chr, 
        paths_to_dirs_chr = paths_to_dirs_chr, inc_fl_types_chr = inc_fl_types_chr, 
        key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    return(ds_ls)
}
#' write dataverse dataset files
#' @description write_dv_ds_fls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write dataverse dataset files. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param files_tb Files (a tibble)
#' @param fl_ids_int File identities (an integer vector)
#' @param ds_url_1L_chr Dataset url (a character vector of length one)
#' @param local_dv_dir_1L_chr Local dataverse directory (a character vector of length one)
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return NULL
#' @rdname write_dv_ds_fls
#' @export 
#' @importFrom lifecycle deprecate_soft
#' @importFrom purrr walk
#' @importFrom ready4 write_dv_fl_to_loc make_local_path_to_dv_data
write_dv_ds_fls <- function (files_tb, fl_ids_int, ds_url_1L_chr, local_dv_dir_1L_chr, 
    key_1L_chr = Sys.getenv("DATAVERSE_KEY"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    lifecycle::deprecate_soft("0.0.0.9149", "write_dv_ds_fls()", 
        "ready4::write_fls_from_dv()")
    purrr::walk(1:length(fl_ids_int), ~{
        if (!(ds_ls$versionState == "DRAFT" | files_tb$file_type_chr[.x] == 
            ".zip")) {
            ready4::write_dv_fl_to_loc(ds_ui_1L_chr = ds_url_1L_chr, 
                fl_nm_1L_chr = files_tb$file_chr[.x], fl_id_1L_int = fl_ids_int[.x], 
                repo_fl_fmt_1L_chr = files_tb$ds_file_ext_chr[.x], 
                key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr, 
                save_type_1L_chr = "original", dest_path_1L_chr = ready4::make_local_path_to_dv_data(save_dir_path_chr = local_dv_dir_1L_chr, 
                  filename_chr = files_tb$file_chr[.x], save_format_chr = files_tb$file_type_chr[.x]))
        }
    })
}
#' write dataverse file to local
#' @description write_dv_fl_to_loc() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write dataverse file to local. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param ds_ui_1L_chr Dataset user interface (a character vector of length one)
#' @param fl_nm_1L_chr File name (a character vector of length one), Default: 'NA'
#' @param fl_id_1L_int File identity (an integer vector of length one), Default: NA
#' @param repo_fl_fmt_1L_chr Repository file format (a character vector of length one)
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param save_type_1L_chr Save type (a character vector of length one), Default: 'original'
#' @param dest_path_1L_chr Destination path (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @return NULL
#' @rdname write_dv_fl_to_loc
#' @export 
#' @importFrom dataverse get_dataset get_file
write_dv_fl_to_loc <- function (ds_ui_1L_chr, fl_nm_1L_chr = NA_character_, fl_id_1L_int = NA_integer_, 
    repo_fl_fmt_1L_chr, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), 
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), save_type_1L_chr = "original", 
    dest_path_1L_chr, consent_1L_chr = "") 
{
    ds_ls <- dataverse::get_dataset(ds_ui_1L_chr)
    if (ds_ls$versionState != "DRAFT") {
        if (!is.na(fl_id_1L_int)) {
            ds_ui_1L_chr <- NULL
        }
        if (!consent_1L_chr %in% c("Y", "N")) {
            consent_1L_chr <- make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the file ", 
                paste0(fl_nm_1L_chr, repo_fl_fmt_1L_chr), " to ", 
                dest_path_1L_chr), options_chr = c("Y", "N"), 
                force_from_opts_1L_chr = T)
        }
        if (consent_1L_chr %in% c("Y")) {
            writeBin(dataverse::get_file(ifelse(is.na(fl_id_1L_int), 
                paste0(fl_nm_1L_chr, repo_fl_fmt_1L_chr), fl_id_1L_int), 
                dataset = ds_ui_1L_chr, format = save_type_1L_chr, 
                key = key_1L_chr, server = server_1L_chr), dest_path_1L_chr)
            message(paste0("New file created in ", dest_path_1L_chr, 
                " :\n", paste0(fl_nm_1L_chr, repo_fl_fmt_1L_chr)))
        }
    }
    else {
        warning("Cannot write local copy of files from private Dataverse repo")
    }
}
#' write files to dataverse dataset
#' @description write_fls_to_dv_ds() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write files to dataverse dataset. The function returns Dataset (a list).
#' @param dss_tb Datasets (a tibble)
#' @param dv_nm_1L_chr Dataverse name (a character vector of length one)
#' @param ds_url_1L_chr Dataset url (a character vector of length one)
#' @param wait_time_in_secs_int Wait time in secs (an integer vector), Default: 5
#' @param make_local_copy_1L_lgl Make local copy (a logical vector of length one), Default: F
#' @param parent_dv_dir_1L_chr Parent dataverse directory (a character vector of length one)
#' @param paths_to_dirs_chr Paths to directories (a character vector)
#' @param paths_are_rltv_1L_lgl Paths are relative (a logical vector of length one), Default: T
#' @param inc_fl_types_chr Include file types (a character vector), Default: 'NA'
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Dataset (a list)
#' @rdname write_fls_to_dv_ds
#' @export 
#' @importFrom lifecycle deprecate_soft
#' @importFrom stats setNames
#' @importFrom purrr map_int
#' @importFrom ready4 write_to_dv_from_tbl write_fls_from_dv
#' @importFrom dataverse get_dataset
write_fls_to_dv_ds <- function (dss_tb, dv_nm_1L_chr, ds_url_1L_chr, wait_time_in_secs_int = 5L, 
    make_local_copy_1L_lgl = F, parent_dv_dir_1L_chr, paths_to_dirs_chr, 
    paths_are_rltv_1L_lgl = T, inc_fl_types_chr = NA_character_, 
    key_1L_chr = Sys.getenv("DATAVERSE_KEY"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    lifecycle::deprecate_soft("0.0.0.9149", "write_fls_to_dv_ds()", 
        "ready4::write_to_dv_with_wait()")
    ds_chr <- dss_tb$ds_obj_nm_chr
    files_tb <- make_files_tb(paths_to_dirs_chr = paths_to_dirs_chr, 
        recode_ls = dss_tb$title_chr %>% as.list() %>% stats::setNames(ds_chr), 
        inc_fl_types_chr = inc_fl_types_chr)
    if (paths_are_rltv_1L_lgl) {
        data_dir_rt_1L_chr <- "."
    }
    else {
        data_dir_rt_1L_chr <- character(0)
    }
    fl_ids_int <- 1:nrow(files_tb) %>% purrr::map_int(~{
        Sys.sleep(wait_time_in_secs_int)
        ready4::write_to_dv_from_tbl(files_tb[.x, ], data_dir_rt_1L_chr = data_dir_rt_1L_chr, 
            ds_url_1L_chr = ds_url_1L_chr, key_1L_chr = key_1L_chr, 
            server_1L_chr = server_1L_chr)
    })
    ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
    if (make_local_copy_1L_lgl | ds_ls$versionState != "DRAFT") {
        ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
        dv_dir_1L_chr <- paste0(parent_dv_dir_1L_chr, "/", dv_nm_1L_chr)
        if (!dir.exists(dv_dir_1L_chr)) {
            dir.create(dv_dir_1L_chr)
        }
        local_dv_dir_1L_chr <- paste0(dv_dir_1L_chr, "/", ds_ls$metadataBlocks$citation$fields$value[[3]])
        if (!dir.exists(local_dv_dir_1L_chr)) {
            dir.create(local_dv_dir_1L_chr)
        }
        ready4::write_fls_from_dv(files_tb, fl_ids_int = fl_ids_int, 
            ds_url_1L_chr = ds_url_1L_chr, local_dv_dir_1L_chr = local_dv_dir_1L_chr)
    }
    return(ds_ls)
}
#' write paired dataset files to dataverse
#' @description write_paired_ds_fls_to_dv() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write paired dataset files to dataverse. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param ds_tb Dataset (a tibble)
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param desc_1L_chr Description (a character vector of length one)
#' @param ds_url_1L_chr Dataset url (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param pkg_dv_dir_1L_chr Package dataverse directory (a character vector of length one), Default: 'data-raw/dataverse'
#' @param data_dir_rt_1L_chr Data directory root (a character vector of length one), Default: '.'
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return NULL
#' @rdname write_paired_ds_fls_to_dv
#' @export 
#' @importFrom utils write.csv
#' @importFrom ready4 get_rds_from_dv make_files_tb write_to_dv_from_tbl
#' @importFrom stats setNames
write_paired_ds_fls_to_dv <- function (ds_tb, fl_nm_1L_chr, desc_1L_chr, ds_url_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", 
    pkg_dv_dir_1L_chr = "data-raw/dataverse", data_dir_rt_1L_chr = ".", 
    key_1L_chr = Sys.getenv("DATAVERSE_KEY"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (!dir.exists(pkg_dv_dir_1L_chr)) 
        dir.create(pkg_dv_dir_1L_chr)
    pkg_dv_dir_1L_chr <- paste0(pkg_dv_dir_1L_chr, "/", fl_nm_1L_chr)
    if (!dir.exists(pkg_dv_dir_1L_chr)) 
        dir.create(pkg_dv_dir_1L_chr)
    ds_tb %>% saveRDS(paste0(pkg_dv_dir_1L_chr, "/", fl_nm_1L_chr, 
        ".RDS"))
    readRDS(paste0(pkg_dv_dir_1L_chr, "/", fl_nm_1L_chr, ".RDS")) %>% 
        utils::write.csv(file = paste0(pkg_dv_dir_1L_chr, "/", 
            fl_nm_1L_chr, ".csv"), row.names = F)
    if (identical(readRDS(paste0(pkg_dv_dir_1L_chr, "/", fl_nm_1L_chr, 
        ".RDS")), ready4::get_rds_from_dv(fl_nm_1L_chr))) {
        unlink(paste0(pkg_dv_dir_1L_chr, "/", fl_nm_1L_chr, ".RDS"))
    }
    ready4::make_files_tb(paths_to_dirs_chr = pkg_dv_dir_1L_chr, 
        recode_ls = c(rep(desc_1L_chr, 2)) %>% as.list() %>% 
            stats::setNames(c(rep(fl_nm_1L_chr, 2)))) %>% ready4::write_to_dv_from_tbl(data_dir_rt_1L_chr = data_dir_rt_1L_chr, 
        ds_url_1L_chr = ds_url_1L_chr, key_1L_chr = key_1L_chr, 
        server_1L_chr = server_1L_chr)
}
#' write to copy files to dataverse directory
#' @description write_to_copy_fls_to_dv_dir() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to copy files to dataverse directory. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param files_tb Files (a tibble)
#' @param local_dv_dir_1L_chr Local dataverse directory (a character vector of length one)
#' @return NULL
#' @rdname write_to_copy_fls_to_dv_dir
#' @export 
#' @importFrom purrr pwalk
write_to_copy_fls_to_dv_dir <- function (files_tb, local_dv_dir_1L_chr) 
{
    purrr::pwalk(files_tb, ~file.copy(paste0(..1, "/", ..2, ..3), 
        local_dv_dir_1L_chr))
}
