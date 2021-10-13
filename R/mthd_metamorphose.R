#' Metamorphose method applied to ready4 S3 class defining a manifest of data required to create an package..
#' @description metamorphose.ready4use_manifest() is a Metamorphose method that transforms an instance of a class into an object with different structural properties. This method is implemented for the ready4 s3 class defining a manifest of data required to create an R package. The function returns Manifest (a ready4 S3).
#' @param x An instance of ready4 s3 class defining a manifest of data required to create an R package.
#' @return Manifest (a ready4 S3)
#' @rdname metamorphose-methods
#' @export 
#' @importFrom ready4class ready4class_constructor ready4class_manifest make_pt_ready4class_manifest
#' @importFrom ready4 metamorphose
metamorphose.ready4use_manifest <- function (x) 
{
    x$x_ready4fun_manifest$subsequent_ls$pkg_ds_ls_ls <- x$pkg_ds_ls_ls
    if (!identical(x$constructor_r3, ready4class::ready4class_constructor())) {
        manifest_r3 <- ready4class::ready4class_manifest(ready4class::make_pt_ready4class_manifest(x$x_ready4fun_manifest, 
            constructor_r3 = x$constructor_r3))
    }
    else {
        manifest_r3 <- x$manifest_r3
    }
    return(manifest_r3)
}
#' @rdname metamorphose-methods
#' @aliases metamorphose,ready4use_manifest-method
#' @importFrom ready4 metamorphose
methods::setMethod("metamorphose", methods::className("ready4use_manifest", package = "ready4use"), metamorphose.ready4use_manifest)
