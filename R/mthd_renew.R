#' Renew (update) a ready4 framework module (or sub-module)
#' @description renew.ready4use_dictionary() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 s3 class defining a data dictionary tibble. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 s3 class defining a data dictionary tibble.
#' @param var_nm_chr Variable name (a character vector), Default: 'NA'
#' @param var_ctg_chr Variable category categories (a character vector), Default: 'NA'
#' @param var_desc_chr Variable description (a character vector), Default: 'NA'
#' @param var_type_chr Variable type (a character vector), Default: 'NA'
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param new_cases_r3 New cases (a ready4 S3), Default: NULL
#' @param new_ready4_dict_r3 New ready4 dictionary (a ready4 S3), Default: deprecated()
#' @param slice_idxs_int Slice indices (an integer vector), Default: NA
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom lifecycle is_present deprecate_warn
#' @importFrom ready4 update_tb_r3 add_lups renew
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
renew.ready4use_dictionary <- function (x, var_nm_chr = NA_character_, var_ctg_chr = NA_character_, 
    var_desc_chr = NA_character_, var_type_chr = NA_character_, 
    filter_cdn_1L_chr = NA_character_, new_cases_r3 = NULL, new_ready4_dict_r3 = deprecated(), 
    slice_idxs_int = NA_integer_) 
{
    if (lifecycle::is_present(new_ready4_dict_r3)) {
        lifecycle::deprecate_warn("0.0.0.9211", "ready4use::renew.ready4use_dictionary(new_ready4_dict_r3)", 
            details = "Please use `ready4use::renew.ready4use_dictionary(new_cases_r3)` instead.")
    }
    x <- ready4::update_tb_r3(x, filter_cdn_1L_chr = filter_cdn_1L_chr, 
        slice_idxs_int = slice_idxs_int)
    x <- dplyr::bind_rows(x, tibble::tibble(var_nm_chr = var_nm_chr, 
        var_ctg_chr = var_ctg_chr, var_desc_chr = var_desc_chr, 
        var_type_chr = var_type_chr))
    if (!is.null(new_cases_r3)) {
        x <- ready4::add_lups(x, new_lup = new_cases_r3, key_var_nm_1L_chr = "var_nm_chr")
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4use_dictionary-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4use_dictionary", package = "ready4use"), renew.ready4use_dictionary)
#' Renew (update) a ready4 framework module (or sub-module)
#' @description renew.ready4use_imports() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for tibble object lookup table of sources of raw (un-processed) data to import.
#' @param local_file_src_chr Local file source (a character vector), Default: 'NA'
#' @param path_to_make_script_chr Path to make script (a character vector), Default: 'NA'
#' @param download_url_chr Download url (a character vector), Default: 'NA'
#' @param inc_file_main_chr Include file main (a character vector), Default: 'NA'
#' @param inc_fls_to_rename_ls Include files to rename (a list), Default: list()
#' @param new_nms_for_inc_fls_ls New names for include files (a list), Default: list()
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param local_to_url_vec_chr Local to url vector (a character vector), Default: 'NA'
#' @param slice_idxs_int Slice indices (an integer vector), Default: NA
#' @param urls_vec_chr Urls vector (a character vector), Default: 'NA'
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom ready4 update_tb_r3 renew
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom purrr reduce
renew.ready4use_imports <- function (x, local_file_src_chr = NA_character_, path_to_make_script_chr = NA_character_, 
    download_url_chr = NA_character_, inc_file_main_chr = NA_character_, 
    inc_fls_to_rename_ls = list(), new_nms_for_inc_fls_ls = list(), 
    filter_cdn_1L_chr = NA_character_, local_to_url_vec_chr = NA_character_, 
    slice_idxs_int = NA_integer_, urls_vec_chr = NA_character_) 
{
    x <- ready4::update_tb_r3(x, filter_cdn_1L_chr = filter_cdn_1L_chr, 
        slice_idxs_int = slice_idxs_int)
    x <- dplyr::bind_rows(x, tibble::tibble(local_file_src_chr = local_file_src_chr, 
        path_to_make_script_chr = path_to_make_script_chr, download_url_chr = download_url_chr, 
        inc_file_main_chr = inc_file_main_chr, inc_fls_to_rename_ls = list(), 
        new_nms_for_inc_fls_ls = list()))
    if (!is.na(local_to_url_vec_chr) & !is.na(urls_vec_chr)) 
        x <- purrr::reduce(1:length(local_to_url_vec_chr), .init = x, 
            ~update_tb_src_loc_to_url_sngl_tb(x = .x, y = .y, 
                local_to_url_vec_chr = local_to_url_vec_chr, 
                urls_vec_chr = urls_vec_chr))
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4use_imports-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4use_imports", package = "ready4use"), renew.ready4use_imports)
#' 
#' Renew (update) a ready4 framework module (or sub-module)
#' @name renew-Ready4useDyad
#' @description renew method applied to Ready4useDyad
#' @param x An object of class Ready4useDyad
#' @param remove_old_lbls_1L_lgl Remove old labels (a logical vector of length one), Default: T
#' @param tfmn_1L_chr Transformation (a character vector of length one), Default: 'capitalise'
#' @param type_1L_chr Type (a character vector of length one), Default: 'label'
#' @return x (An object of class Ready4useDyad)
#' @rdname renew-methods
#' @aliases renew,Ready4useDyad-method
#' @export 
#' @importFrom Hmisc capitalize
#' @importFrom stringr str_to_title
#' @importFrom ready4 renew
methods::setMethod("renew", "Ready4useDyad", function (x, remove_old_lbls_1L_lgl = T, tfmn_1L_chr = "capitalise", 
    type_1L_chr = "label") 
{
    if (type_1L_chr %in% c("label", "case")) {
        dictionary_tb <- x@dictionary_r3
        if (tfmn_1L_chr == "capitalise") 
            dictionary_tb$var_desc_chr <- dictionary_tb$var_desc_chr %>% 
                Hmisc::capitalize()
        if (tfmn_1L_chr == "title") 
            dictionary_tb$var_desc_chr <- dictionary_tb$var_desc_chr %>% 
                stringr::str_to_title()
    }
    if (type_1L_chr == "case") {
        x@dictionary_r3 <- dictionary_tb
    }
    if (type_1L_chr == "label") {
        tfd_ds_tb <- add_labels_from_dictionary(x@ds_tb, dictionary_tb = dictionary_tb, 
            remove_old_lbls_1L_lgl = remove_old_lbls_1L_lgl)
        x@ds_tb <- tfd_ds_tb
    }
    if (type_1L_chr == "unlabel") {
        x@ds_tb <- remove_labels_from_ds(x@ds_tb)
    }
    return(x)
})
