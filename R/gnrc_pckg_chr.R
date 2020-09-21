#' pckg_chr
#' @description S4 Generic function to get the value of the slot pckg_chr
#' @name pckg_chr
#' @param x An object 
#' 
#' @export

methods::setGeneric("pckg_chr", function(x) standardGeneric("pckg_chr"))
#' pckg_chr
#' @name pckg_chr-ready4_local
#' @description Get the value of the slot pckg_chr for S4 objects of class ready4_local
#' @param x An object of class ready4_local
#' @rdname pckg_chr
methods::setMethod("pckg_chr", methods::className("ready4_local",".GlobalEnv"), function (x) 
{
    x@pckg_chr
})
#' pckg_chr<-
#' @description S4 Generic function to set the value of the slot pckg_chr
#' @name pckg_chr<-
#' @param x An object 
#' 
#' @export

methods::setGeneric("pckg_chr<-", function(x, value) standardGeneric("pckg_chr<-"))
#' pckg_chr<-
#' @name pckg_chr<--ready4_local
#' @description Set the value of the slot pckg_chr for S4 objects of class ready4_local
#' @param x An object of class ready4_local
#' @rdname pckg_chr-set
methods::setMethod("pckg_chr<-", methods::className("ready4_local",".GlobalEnv"), function (x, value) 
{
    x@pckg_chr <- value
    methods::validObject(x)
    x
})
