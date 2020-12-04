add_lups.ready4_dictionary_lup <- function(x,
                                           new_ready4_dictionary_lup){
combined_ready4_dictionary_lups <- ready4fun::add_lups(x,
                    new_lup = new_ready4_dictionary_lup,
                    key_var_nm_1L_chr = "var_nm_chr")
return(combined_ready4_dictionary_lups)
}
