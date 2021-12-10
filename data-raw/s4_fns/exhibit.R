exhibit_Ready4useDyad <- function(x,
                                  caption_1L_chr = NA_character_,
                                  display_1L_chr = "all",
                                  mkdn_tbl_ref_1L_chr = "",
                                  output_type_1L_chr = "HTML",
                                  type_1L_chr = "ds",
                                  use_lbls_as_col_nms_1L_lgl = T,
                                  use_rdocx_1L_lgl = F,
                                  ...){
  if(type_1L_chr == "ds"){
    df <- x@ds_tb
    caption_1L_chr <- ifelse(is.na(caption_1L_chr),
                             "Dataset",
                             caption_1L_chr)
  }
  if(type_1L_chr == "dict"){
    df <- x@dictionary_r3
    df <- df %>%
      add_labels_from_dictionary(dictionary_tb = make_pt_ready4use_dictionary(var_nm_chr = names(df),
                                                                              var_desc_chr = c("Variable","Category",
                                                                                                          "Description", "Class")),
                                 remove_old_lbls_1L_lgl = T)

    caption_1L_chr <- ifelse(is.na(caption_1L_chr),
                             "Data Dictionary",
                             caption_1L_chr)
  }
  if(display_1L_chr == "head")
    df <- df %>%
      head()
  if(display_1L_chr == "tail")
    df <- df %>%
      tail()
  df %>%
    ready4show::print_table(output_type_1L_chr = output_type_1L_chr,
                            use_rdocx_1L_lgl = use_rdocx_1L_lgl,
                            caption_1L_chr = caption_1L_chr,
                            use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                            mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                            ...)
}
