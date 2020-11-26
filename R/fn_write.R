#' Write dataverse dataset
#' @description write_dv_ds() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write dataverse dataset. The function returns Dataset (a list).
#' @param ds_meta_ls Dataset meta (a list)
#' @param dev_pkg_nm_1L_chr Development package name (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
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
#' @importFrom ready4fun get_dev_pkg_nm
write_dv_ds <- function (ds_meta_ls, dev_pkg_nm_1L_chr = ready4fun::get_dev_pkg_nm(), 
    dss_tb, dv_nm_1L_chr, parent_dv_dir_1L_chr, paths_to_dirs_chr, 
    inc_fl_types_chr = NA_character_, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), 
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    ds_url_1L_chr <- add_ds_to_dv_repo(dv_1L_chr = dv_nm_1L_chr, 
        ds_meta_ls = ds_meta_ls)
    ds_ls <- write_fls_to_dv_ds(dss_tb = dss_tb, dv_nm_1L_chr = dv_nm_1L_chr, 
        parent_dv_dir_1L_chr = parent_dv_dir_1L_chr, paths_to_dirs_chr = paths_to_dirs_chr, 
        inc_fl_types_chr = inc_fl_types_chr, key_1L_chr = key_1L_chr, 
        server_1L_chr = server_1L_chr)
    return(ds_ls)
}
#' Write dataverse dataset files
#' @description write_dv_ds_fls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write dataverse dataset files. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param files_tb Files (a tibble)
#' @param fl_ids_int File ids (an integer vector)
#' @param local_dv_dir_1L_chr Local dataverse directory (a character vector of length one)
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return NULL
#' @rdname write_dv_ds_fls
#' @export 
#' @importFrom purrr walk
write_dv_ds_fls <- function (files_tb, fl_ids_int, local_dv_dir_1L_chr, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), 
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    purrr::walk(1:length(fl_ids_int), ~{
        if (!(ds_ls$versionState == "DRAFT" & files_tb$file_type_chr[.x] == 
            ".zip")) {
            write_dv_fl_to_loc(ds_ui_1L_chr = ds_url_1L_chr, 
                fl_nm_1L_chr = files_tb$file_chr[.x], repo_fl_fmt_1L_chr = files_tb$ds_file_ext_chr[.x], 
                key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr, 
                save_type_1L_chr = "original", dest_path_1L_chr = get_local_path_to_dv_data(save_dir_path_chr = local_dv_dir_1L_chr, 
                  filename_chr = files_tb$file_chr[.x], save_format_chr = files_tb$file_type_chr[.x]))
        }
    })
}
#' Write dataverse file to local
#' @description write_dv_fl_to_loc() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write dataverse file to local. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param ds_ui_1L_chr Dataset ui (a character vector of length one)
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param repo_fl_fmt_1L_chr Repo file fmt (a character vector of length one)
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param save_type_1L_chr Save type (a character vector of length one), Default: 'original'
#' @param dest_path_1L_chr Dest path (a character vector of length one)
#' @return NULL
#' @rdname write_dv_fl_to_loc
#' @export 
#' @importFrom dataverse get_file
write_dv_fl_to_loc <- function (ds_ui_1L_chr, fl_nm_1L_chr, repo_fl_fmt_1L_chr, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), 
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), save_type_1L_chr = "original", 
    dest_path_1L_chr) 
{
    writeBin(dataverse::get_file(paste0(fl_nm_1L_chr, repo_fl_fmt_1L_chr), 
        ds_ui_1L_chr, format = save_type_1L_chr, key = key_1L_chr, 
        server = server_1L_chr), dest_path_1L_chr)
}
#' Write files to dataverse dataset
#' @description write_fls_to_dv_ds() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write files to dataverse dataset. The function returns Dataset (a list).
#' @param dss_tb Datasets (a tibble)
#' @param dv_nm_1L_chr Dataverse name (a character vector of length one)
#' @param ds_url_1L_chr Dataset url (a character vector of length one)
#' @param wait_time_in_secs_int Wait time in secs (an integer vector), Default: 5
#' @param parent_dv_dir_1L_chr Parent dataverse directory (a character vector of length one)
#' @param paths_to_dirs_chr Paths to directories (a character vector)
#' @param inc_fl_types_chr Include file types (a character vector), Default: 'NA'
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Dataset (a list)
#' @rdname write_fls_to_dv_ds
#' @export 
#' @importFrom dataverse get_dataset
#' @importFrom stats setNames
#' @importFrom purrr map_int
write_fls_to_dv_ds <- function (dss_tb, dv_nm_1L_chr, ds_url_1L_chr, wait_time_in_secs_int = 5L, 
    parent_dv_dir_1L_chr, paths_to_dirs_chr, inc_fl_types_chr = NA_character_, 
    key_1L_chr = Sys.getenv("DATAVERSE_KEY"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
    dv_dir_1L_chr <- paste0(parent_dv_dir_1L_chr, "/", dv_nm_1L_chr)
    if (!dir.exists(dv_dir_1L_chr)) {
        dir.create(dv_dir_1L_chr)
    }
    local_dv_dir_1L_chr <- paste0(dv_dir_1L_chr, "/", ds_ls$metadataBlocks$citation$fields$value[[1]])
    if (!dir.exists(local_dv_dir_1L_chr)) {
        dir.create(local_dv_dir_1L_chr)
    }
    ds_chr <- dss_tb$ds_obj_nm_chr
    files_tb <- make_files_tb(paths_to_dirs_chr = paths_to_dirs_chr, 
        recode_ls = dss_tb$title_chr %>% as.list() %>% stats::setNames(ds_chr), 
        inc_fl_types_chr = inc_fl_types_chr)
    fl_ids_int <- 1:nrow(files_tb) %>% purrr::map_int(~{
        Sys.sleep(wait_time_in_secs_int)
        add_files_to_dv(files_tb[.x, ], ds_url_1L_chr = ds_url_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    })
    write_dv_ds_fls(files_tb, fl_ids_int = fl_ids_int, local_dv_dir_1L_chr = local_dv_dir_1L_chr)
    ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
    return(ds_ls)
}
#' Write package datasets to dataverse dataset comma separated variables files
#' @description write_pkg_dss_to_dv_ds_csvs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write package datasets to dataverse dataset comma separated variables files. The function returns Dataset (a list).
#' @param pkg_dss_tb Package datasets (a tibble)
#' @param dv_nm_1L_chr Dataverse name (a character vector of length one)
#' @param ds_url_1L_chr Dataset url (a character vector of length one)
#' @param wait_time_in_secs_int Wait time in secs (an integer vector), Default: 5
#' @param dev_pkg_nm_1L_chr Development package name (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
#' @param parent_dv_dir_1L_chr Parent dataverse directory (a character vector of length one), Default: '../../../../Data/Dataverse'
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Dataset (a list)
#' @rdname write_pkg_dss_to_dv_ds_csvs
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm
#' @importFrom purrr walk
#' @importFrom utils data
#' @importFrom dplyr mutate_if
#' @importFrom stringr str_c
write_pkg_dss_to_dv_ds_csvs <- function (pkg_dss_tb, dv_nm_1L_chr, ds_url_1L_chr, wait_time_in_secs_int = 5L, 
    dev_pkg_nm_1L_chr = ready4fun::get_dev_pkg_nm(), parent_dv_dir_1L_chr = "../../../../Data/Dataverse", 
    key_1L_chr = Sys.getenv("DATAVERSE_KEY"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    ds_chr <- pkg_dss_tb$ds_obj_nm_chr
    purrr::walk(ds_chr, ~{
        utils::data(list = .x, package = dev_pkg_nm_1L_chr, envir = environment())
        eval(parse(text = .x)) %>% dplyr::mutate_if(is.list, 
            list(~ifelse(stringr::str_c(.) == "NULL", NA_character_, 
                stringr::str_c(.)))) %>% write.csv(file = paste0("data-raw/", 
            .x, ".csv"), row.names = F)
    })
    ds_ls <- write_fls_to_dv_ds(dss_tb = pkg_dss_tb, dv_nm_1L_chr = dv_nm_1L_chr, 
        ds_url_1L_chr = ds_url_1L_chr, wait_time_in_secs_int = wait_time_in_secs_int, 
        parent_dv_dir_1L_chr = parent_dv_dir_1L_chr, paths_to_dirs_chr = c("data-raw"), 
        inc_fl_types_chr = ".csv", key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    return(ds_ls)
}
#' Write to add urls to datasets
#' @description write_to_add_urls_to_dss() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to add urls to datasets. The function returns Package datasets (a tibble).
#' @param ds_url PARAM_DESCRIPTION
#' @param pkg_dss_tb Package datasets (a tibble)
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: ready4fun::get_dev_pkg_nm()
#' @return Package datasets (a tibble)
#' @rdname write_to_add_urls_to_dss
#' @export 
#' @importFrom ready4fun get_dev_pkg_nm write_and_doc_ds
#' @importFrom dataverse dataset_files
#' @importFrom purrr map_chr map_dfr walk
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#' @importFrom dplyr inner_join select
#' @importFrom utils data
write_to_add_urls_to_dss <- function (ds_url, pkg_dss_tb, pkg_nm_1L_chr = ready4fun::get_dev_pkg_nm()) 
{
    ds_fls_ls <- dataverse::dataset_files(ds_url)
    fl_ids_chr <- purrr::map_chr(1:length(ds_fls_ls), ~ds_fls_ls[[.x]][["dataFile"]][["pidURL"]])
    fl_nms_chr <- purrr::map_chr(1:length(ds_fls_ls), ~ds_fls_ls[[.x]][["dataFile"]][["originalFileName"]] %>% 
        stringr::str_remove(".csv"))
    url_lup <- purrr::map_dfr(1:length(ds_fls_ls), ~tibble::tibble(ds_obj_nm_chr = ds_fls_ls[[.x]][["dataFile"]][["originalFileName"]] %>% 
        stringr::str_remove(".csv"), url_chr = ds_fls_ls[[.x]][["dataFile"]][["pidURL"]]))
    pkg_dss_tb <- dplyr::inner_join(pkg_dss_tb %>% dplyr::select(-url_chr), 
        url_lup)
    purrr::walk(pkg_dss_tb, ~{
        utils::data(list = ..1, package = pkg_nm_1L_chr, envir = environment())
        eval(parse(text = paste0("ds<-", ..1)))
        ds %>% ready4fun::write_and_doc_ds(db_1L_chr = ..1, title_1L_chr = ..2, 
            desc_1L_chr = ..3, url_1L_chr = ..4, pkg_dss_tb = pkg_dss_tb)
    })
    return(pkg_dss_tb)
}
#' Write to copy files to dataverse directory
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
