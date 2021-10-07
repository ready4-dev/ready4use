author.ready4use_manifest <- function(x){
  # purrr::map(pkg_ds_ls_ls,
  #            ~ready4fun_dataset(.x)) # Need to edit to validate datasets_ls
  x$manifest_r3 <- metamorphose(x) %>%
    ready4fun::author(manifest_r3)
  authorData(x)
  return(x)
}
