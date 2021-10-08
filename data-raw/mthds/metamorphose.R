metamorphose.ready4use_manifest <- function(x){
  x$fns_ready4fun_manifest$subsequent_ls$pkg_ds_ls_ls <- x$pkg_ds_ls_ls
  if(!identical(x$constructor_r3, ready4class::ready4class_constructor())){
    manifest_r3 <- ready4class::ready4class_manifest(ready4class::make_pt_ready4class_manifest(manifest_r3 = x$fns_ready4fun_manifest,
                                                                                               constructor_r3 = x$constructor_r3))
  }else{
    manifest_r3 <- x$manifest_r3
  }
  return(manifest_r3)
}
