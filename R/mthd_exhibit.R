#' Exhibit features of model module data by printing them to the R console
#' @description exhibit.ready4use_dictionary() is an exhibit method that exhibits features of a class instance by printing to console. This method is implemented for the ready4 s3 class defining a data dictionary tibble. The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4use_dictionary`, a ready4 s3 class defining a data dictionary tibble.
#' @param caption_1L_chr Caption (a character vector of length one), Default: NULL
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: T
#' @return No return value, called for side effects.
#' @rdname exhibit-methods
#' @export 
#' @importFrom ready4 exhibit
exhibit.ready4use_dictionary <- function (x, caption_1L_chr = NULL, mkdn_tbl_ref_1L_chr = NULL, 
    output_type_1L_chr = "HTML", use_lbls_as_col_nms_1L_lgl = T) 
{
    var_desc_chr = c("Variable", "Category", "Description", "Class")
    x %>% print_from_chunk(caption_1L_chr = caption_1L_chr, mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, 
        output_type_1L_chr = output_type_1L_chr, use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl, 
        var_desc_chr = var_desc_chr)
}
#' @rdname exhibit-methods
#' @aliases exhibit,ready4use_dictionary-method
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", methods::className("ready4use_dictionary", package = "ready4use"), exhibit.ready4use_dictionary)
#' 
#' Exhibit features of model module data by printing them to the R console
#' @name exhibit-Ready4useDyad
#' @description exhibit method applied to Ready4useDyad
#' @param x An object of class Ready4useDyad
#' @param caption_1L_chr Caption (a character vector of length one), Default: 'NA'
#' @param display_1L_chr Display (a character vector of length one), Default: 'all'
#' @param label_1L_lgl Label (a logical vector of length one), Default: FALSE
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: ''
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param type_1L_chr Type (a character vector of length one), Default: 'ds'
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: TRUE
#' @param use_rdocx_1L_lgl Use rdocx (a logical vector of length one), Default: FALSE
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname exhibit-methods
#' @aliases exhibit,Ready4useDyad-method
#' @export 
#' @importFrom ready4show print_table
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", "Ready4useDyad", function (x, caption_1L_chr = NA_character_, display_1L_chr = "all", 
    label_1L_lgl = FALSE, mkdn_tbl_ref_1L_chr = "", output_type_1L_chr = "HTML", 
    type_1L_chr = "ds", use_lbls_as_col_nms_1L_lgl = TRUE, use_rdocx_1L_lgl = FALSE, 
    ...) 
{
    if (label_1L_lgl) {
        x <- renew(x, type_1L_chr = "label")
    }
    if (type_1L_chr == "ds") {
        df <- x@ds_tb
        caption_1L_chr <- ifelse(is.na(caption_1L_chr), "Dataset", 
            caption_1L_chr)
    }
    if (type_1L_chr == "dict") {
        df <- x@dictionary_r3
        df <- df %>% add_labels_from_dictionary(dictionary_tb = make_pt_ready4use_dictionary(var_nm_chr = names(df), 
            var_desc_chr = c("Variable", "Category", "Description", 
                "Class")), remove_old_lbls_1L_lgl = T)
        caption_1L_chr <- ifelse(is.na(caption_1L_chr), "Data Dictionary", 
            caption_1L_chr)
    }
    if (display_1L_chr == "head") 
        df <- df %>% head()
    if (display_1L_chr == "tail") 
        df <- df %>% tail()
    df %>% ready4show::print_table(output_type_1L_chr = output_type_1L_chr, 
        use_rdocx_1L_lgl = use_rdocx_1L_lgl, caption_1L_chr = caption_1L_chr, 
        use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl, 
        mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, ...)
})
