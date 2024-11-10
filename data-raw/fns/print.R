print_significance <- function(data_tb,
                               by_1L_chr,
                               vars_chr,
                               caption_1L_chr = character(0),
                               output_type_1L_chr = "HTML",
                               sort_1L_lgl = T){
  if(identical(caption_1L_chr, character(0))){
    caption_1L_chr <- paste0("Differentiation by ", tolower(by_1L_chr))
  }
  data_df <- make_significance_df(data_tb,
                                  by_1L_chr = by_1L_chr,
                                  vars_chr = vars_chr)
  data_df <- data_df %>%
    dplyr::rename(Variable = variable,
                  p = p.value,
                  ` ` = stars)
  data_xx <- data_df %>%
    ready4show::print_table(output_type_1L_chr = output_type_1L_chr,
                            caption = caption_1L_chr)
  return(data_xx)
}
