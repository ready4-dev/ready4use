bind_lups.ready4_dictionary <- function(x,
                                       new_ready4_dict_r3){
combined_ready4_dictionaries <- ready4fun::add_lups(x,
                    new_lup = new_ready4_dict_r3,
                    key_var_nm_1L_chr = "var_nm_chr")
return(combined_ready4_dictionaries)
}
