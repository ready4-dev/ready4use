#' path_to_starter_sf_chr
#' @description S4 Generic function to get the value of the slot path_to_starter_sf_chr
#' @name path_to_starter_sf_chr
#' @param x An object 
#' 
#' @export

methods::setGeneric("path_to_starter_sf_chr", function(x) standardGeneric("path_to_starter_sf_chr"))
#' path_to_starter_sf_chr
#' @name path_to_starter_sf_chr-ready4_local_proc
#' @description Get the value of the slot path_to_starter_sf_chr for S4 objects of class ready4_local_proc
#' @param x An object of class ready4_local_proc
#' @rdname path_to_starter_sf_chr
methods::setMethod("path_to_starter_sf_chr", methods::className("ready4_local_proc",".GlobalEnv"), function (x) 
{
    x@path_to_starter_sf_chr
})
#' path_to_starter_sf_chr<-
#' @description S4 Generic function to set the value of the slot path_to_starter_sf_chr
#' @name path_to_starter_sf_chr<-
#' @param x An object 
#' 
#' @export

methods::setGeneric("path_to_starter_sf_chr<-", function(x, value) standardGeneric("path_to_starter_sf_chr<-"))
#' path_to_starter_sf_chr<-
#' @name path_to_starter_sf_chr<--ready4_local_proc
#' @description Set the value of the slot path_to_starter_sf_chr for S4 objects of class ready4_local_proc
#' @param x An object of class ready4_local_proc
#' @rdname path_to_starter_sf_chr-set
methods::setMethod("path_to_starter_sf_chr<-", methods::className("ready4_local_proc",".GlobalEnv"), function (x, value) 
{
    x@path_to_starter_sf_chr <- value
    methods::validObject(x)
    x
})
