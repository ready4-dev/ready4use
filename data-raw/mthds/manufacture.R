manufacture.ready4use_imports <- function(x,
                                          forced_choice_chr = NA_character_,
                                          script_args_ls = NULL){
  assert_single_row_tb(x)
  import_type_ls <- ready4::procure(x,
                                    inc_script_lgl = !is.null(script_args_ls),
                                    forced_choice_chr = forced_choice_chr)
  switch(names(import_type_ls),
         "script_chr" = rlang::exec(Ready4useArguments, x, !!!script_args_ls),
         "local_chr" = get_valid_path_chr(import_type_ls[[1]]),
         "repo_chr"  = manufacture(x),
         "source_url_chr" = url(import_type_ls[[1]])
  )

}
manufacture.ready4use_dataverses <- function(x,
                                             type_1L_chr = "read_fn"){
  read_fn_ls <- NULL
  if(type_1L_chr == "read_fn")
    read_fn_ls <- purrr::map(x$file_type_chr,
             ~ switch(.x,
                      ".csv" = read.csv, ## Need to add fread
                      ".xls" = readxl::read_excel,
                      ".xlsx" = readxl::read_excel,
                      ".RDS" = readRDS()),
  )
  return(read_fn_ls)
}
# manufacture.ready4use_sp_import_lup <- function(x){
#   ready4use_dataverses(x %>%
#                             dplyr::select(names(ready4use_dataverses())))
# }
