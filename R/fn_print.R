#' Print significance
#' @description print_significance() is a Print function that prints output to console. Specifically, this function implements an algorithm to print significance. The function returns Data (an output object of multiple potential types).
#' @param data_tb Data (a tibble)
#' @param by_1L_chr By (a character vector of length one)
#' @param vars_chr Variables (a character vector)
#' @param caption_1L_chr Caption (a character vector of length one), Default: character(0)
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param sort_1L_lgl Sort (a logical vector of length one), Default: T
#' @return Data (an output object of multiple potential types)
#' @rdname print_significance
#' @export 
#' @importFrom dplyr rename
#' @importFrom ready4show print_table
#' @keywords internal
print_significance <- function (data_tb, by_1L_chr, vars_chr, caption_1L_chr = character(0), 
    output_type_1L_chr = "HTML", sort_1L_lgl = T) 
{
    if (identical(caption_1L_chr, character(0))) {
        caption_1L_chr <- paste0("Differentiation by ", tolower(by_1L_chr))
    }
    data_df <- make_significance_df(data_tb, by_1L_chr = by_1L_chr, 
        vars_chr = vars_chr)
    data_df <- data_df %>% dplyr::rename(Variable = variable, 
        p = p.value, ` ` = stars)
    data_xx <- data_df %>% ready4show::print_table(output_type_1L_chr = output_type_1L_chr, 
        caption = caption_1L_chr)
    return(data_xx)
}
