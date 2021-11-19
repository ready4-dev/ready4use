#' 
#' exhibit
#' @name exhibit-Ready4useDyad
#' @description exhibit method applied to Ready4useDyad

#' @return NULL
#' @rdname exhibit-methods
#' @aliases exhibit,Ready4useDyad-method
#' @export 
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", "Ready4useDyad", function (x, caption_1L_chr = NA_character_, display_1L_chr = "all", 
    mkdn_tbl_ref_1L_chr = "", output_type_1L_chr = "HTML", type_1L_chr = "ds", 
    use_lbls_as_col_nms_1L_lgl = T, use_rdocx_1L_lgl = F, ...) 
{
    if (type_1L_chr == "ds") {
        df <- x@ds_tb
        caption_1L_chr <- ifelse(is.na(caption_1L_chr), "Dataset", 
            caption_1L_chr)
    }
    if (type_1L_chr == "dict") {
        df <- x@dictionary_r3
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
