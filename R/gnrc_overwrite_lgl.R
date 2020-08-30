#' overwrite_lgl
#' @description S4 Generic function to get the value of the slot overwrite_lgl
#' @name overwrite_lgl
#' @param x An object 
#' 
#' @export

methods::setGeneric("overwrite_lgl", function(x) standardGeneric("overwrite_lgl"))
#' overwrite_lgl
#' @name overwrite_lgl-ready4_local
#' @description Get the value of the slot overwrite_lgl for S4 objects of class ready4_local
#' @param x An object of class ready4_local
#' @rdname overwrite_lgl
methods::setMethod("overwrite_lgl", methods::className("ready4_local",".GlobalEnv"), function (x) 
{
    x@overwrite_lgl
})
#' overwrite_lgl<-
#' @description S4 Generic function to set the value of the slot overwrite_lgl
#' @name overwrite_lgl<-
#' @param x An object 
#' 
#' @export

methods::setGeneric("overwrite_lgl<-", function(x, value) standardGeneric("overwrite_lgl<-"))
#' overwrite_lgl<-
#' @name overwrite_lgl<--ready4_local
#' @description Set the value of the slot overwrite_lgl for S4 objects of class ready4_local
#' @param x An object of class ready4_local
#' @rdname overwrite_lgl-set
methods::setMethod("overwrite_lgl<-", methods::className("ready4_local",".GlobalEnv"), function (x, value) 
{
    x@overwrite_lgl <- value
    methods::validObject(x)
    x
})
