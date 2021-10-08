#' Author method applied to ready4 S3 class defining a manifest of data required to create an package..
#' @description author.ready4use_manifest() is an Author method that writes files to local or remote locations. This method is implemented for the ready4 s3 class defining a manifest of data required to create an R package. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 s3 class defining a manifest of data required to create an R package.
#' @return NA ()
#' @rdname author-methods
#' @export 
#' @importFrom ready4fun author
author.ready4use_manifest <- function (x) 
{
    x$fns_ready4fun_manifest <- metamorphose(x) %>% ready4fun::author()
    authorData(x)
    return(x)
}
#' @rdname author-methods
#' @aliases author,ready4use_manifest-method
#' @importFrom ready4fun author
methods::setMethod("author", methods::className("ready4use_manifest", package = "ready4use"), author.ready4use_manifest)
