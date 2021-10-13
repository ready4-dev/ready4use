author.ready4use_manifest <- function(x){
  # purrr::map(pkg_ds_ls_ls,
  #            ~ready4fun_dataset(.x)) # Need to edit to validate datasets_ls
  x$fns_ready4fun_manifest <- ready4::metamorphose(x) %>%
    ready4::author()
  ready4::authorData(x)
  return(x)
}
