#' Write files to dataverse dataset
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
#' @importFrom ready4 write_to_dv_from_tbl write_new_dirs write_fls_from_dv
#' @importFrom dataverse get_dataset
#' @keywords internal
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
        ready4::write_new_dirs(dv_dir_1L_chr)
        local_dv_dir_1L_chr <- paste0(dv_dir_1L_chr, "/", ds_ls$metadataBlocks$citation$fields$value[[3]])
        ready4::write_new_dirs(local_dv_dir_1L_chr)
        ready4::write_fls_from_dv(files_tb, fl_ids_int = fl_ids_int, 
            ds_url_1L_chr = ds_url_1L_chr, local_dv_dir_1L_chr = local_dv_dir_1L_chr)
    }
    return(ds_ls)
}
#' Write to copy files to dataverse directory
#' @description write_to_copy_fls_to_dv_dir() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to copy files to dataverse directory. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param files_tb Files (a tibble)
#' @param local_dv_dir_1L_chr Local dataverse directory (a character vector of length one)
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param consent_indcs_int Consent indices (an integer vector), Default: 1
#' @param options_chr Options (a character vector), Default: c("Y", "N")
#' @return NULL
#' @rdname write_to_copy_fls_to_dv_dir
#' @export 
#' @importFrom purrr pwalk map_chr
#' @importFrom ready4 write_with_consent make_list_phrase
#' @keywords internal
write_to_copy_fls_to_dv_dir <- function (files_tb, local_dv_dir_1L_chr, consent_1L_chr = "", 
    consent_indcs_int = 1L, options_chr = c("Y", "N")) 
{
    consented_fn <- function(files_tb, local_dv_dir_1L_chr) {
        purrr::pwalk(files_tb, ~file.copy(paste0(..1, "/", ..2, 
            ..3), local_dv_dir_1L_chr))
    }
    files_chr <- purrr::map_chr(files_tb, ~paste0(..1, "/", ..2, 
        ..3))
    ready4::write_with_consent(consented_fn = consented_fn, prompt_1L_chr = paste0("Do you confirm that you want to copy the files ", 
        ready4::make_list_phrase(files_chr), " to ", local_dv_dir_1L_chr, 
        "?"), consent_1L_chr = consent_1L_chr, consent_indcs_int = consent_indcs_int, 
        consented_args_ls = list(files_tb = files_tb, local_dv_dir_1L_chr = local_dv_dir_1L_chr), 
        consented_msg_1L_chr = paste0("The files ", ready4::make_list_phrase(files_chr), 
            " have been copied to ", local_dv_dir_1L_chr, "?"), 
        declined_msg_1L_chr = "Copy request cancelled - no files or directories have been written.", 
        options_chr = options_chr)
}
