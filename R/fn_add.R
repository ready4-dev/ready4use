#' Add dataset to dataverse repo
#' @description add_ds_to_dv_repo() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add dataset to dataverse repo. Function argument dv_1L_chr specifies the object to be updated. The function returns Dataset url (a character vector of length one).
#' @param dv_1L_chr Dataverse (a character vector of length one)
#' @param ds_meta_ls Dataset meta (a list)
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Dataset url (a character vector of length one)
#' @rdname add_ds_to_dv_repo
#' @export 
#' @importFrom dataverse get_dataverse dataverse_contents get_dataset create_dataset update_dataset
#' @importFrom purrr map_chr pluck discard map_lgl
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
        dataverse:::create_dataset(dv_1L_chr, body = ds_meta_ls, 
            key = key_1L_chr, server = server_1L_chr)
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
#' Add dataverse meta to import
#' @description add_dv_meta_to_imp_lup() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add dataverse meta to import lookup table. Function argument imp_lup specifies the object to be updated. The function returns Import (a lookup table).
#' @param imp_lup Import (a lookup table)
#' @param ds_ui_1L_chr Dataset ui (a character vector of length one)
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
    imp_lup <- imp_lup %>% dplyr::mutate(data_repo_db_ui = ds_ui_1L_chr, 
        data_repo_file_ext = file_type_1L_chr, data_repo_save_type = save_type_1L_chr)
    return(imp_lup)
}
#' Add files to dataverse
#' @description add_files_to_dv() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add files to dataverse. Function argument files_tb specifies the object to be updated. The function returns File ids (an integer vector).
#' @param files_tb Files (a tibble)
#' @param data_dir_rt_1L_chr Data directory root (a character vector of length one), Default: '.'
#' @param ds_url_1L_chr Dataset url (a character vector of length one)
#' @param key_1L_chr Key (a character vector of length one)
#' @param server_1L_chr Server (a character vector of length one)
#' @return File ids (an integer vector)
#' @rdname add_files_to_dv
#' @export 
#' @importFrom purrr pmap_int
#' @importFrom dataverse get_dataset delete_file add_dataset_file
#' @importFrom ready4fun get_from_lup_obj
#' @importFrom tibble as_tibble
#' @keywords internal
add_files_to_dv <- function (files_tb, data_dir_rt_1L_chr = ".", ds_url_1L_chr, 
    key_1L_chr, server_1L_chr) 
{
    fl_ids_int <- purrr::pmap_int(files_tb, ~{
        ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
        is_draft_1L_lgl <- ds_ls$versionState == "DRAFT"
        path_1L_chr <- paste0(data_dir_rt_1L_chr, "/", ..1, "/", 
            ..2, ..3)
        file_nm_1L_chr <- paste0(..2, ..3)
        if (is_draft_1L_lgl) {
            if (file_nm_1L_chr %in% ds_ls$files$originalFileName) {
                ready4fun::get_from_lup_obj(ds_ls$files[, names(ds_ls$files) %>% 
                  unique()] %>% tibble::as_tibble(), match_var_nm_1L_chr = "originalFileName", 
                  match_value_xx = file_nm_1L_chr, target_var_nm_1L_chr = "id", 
                  evaluate_lgl = F) %>% dataverse::delete_file()
            }
            dataverse::add_dataset_file(file = path_1L_chr, dataset = ds_url_1L_chr, 
                description = ..4, key = key_1L_chr, server = server_1L_chr)
        }
    })
    return(fl_ids_int)
}
