make.ready4use_all_import_lup <- function(x,
                                          forced_choice_chr = NA_character_,
                                          script_args_ls = NULL){
  assert_single_row_tb(x)
  import_type_ls <- procure(x,
                            inc_script_lgl = !is.null(script_args_ls),
                            forced_choice_chr = forced_choice_chr)
  switch(names(import_type_ls),
         "script_chr" = rlang::exec(ready4use_script_data, x, !!!script_args_ls),
         "local_chr" = get_valid_path_chr(import_type_ls[[1]]),
         "repo_chr"  = make(x),
         "source_url_chr" = url(import_type_ls[[1]])
  )

}
make.ready4use_sp_import_lup <- function(x){
  ready4use_dv_import_lup(x %>%
                            dplyr::select(names(ready4use_dv_import_lup())))
}
