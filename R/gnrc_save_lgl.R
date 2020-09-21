#' save_lgl
#' @description S4 Generic function to get the value of the slot save_lgl
#' @name save_lgl
#' @param x An object 
#' 
#' @export

methods::setGeneric("save_lgl", function(x) standardGeneric("save_lgl"))
#' save_lgl
#' @name save_lgl-ready4_local
#' @description Get the value of the slot save_lgl for S4 objects of class ready4_local
#' @param x An object of class ready4_local
#' @rdname save_lgl
methods::setMethod("save_lgl", methods::className("ready4_local",".GlobalEnv"), function (x) 
{
    x@save_lgl
})
#' save_lgl<-
#' @description S4 Generic function to set the value of the slot save_lgl
#' @name save_lgl<-
#' @param x An object 
#' 
#' @export

methods::setGeneric("save_lgl<-", function(x, value) standardGeneric("save_lgl<-"))
#' save_lgl<-
#' @name save_lgl<--ready4_local
#' @description Set the value of the slot save_lgl for S4 objects of class ready4_local
#' @param x An object of class ready4_local
#' @rdname save_lgl-set
methods::setMethod("save_lgl<-", methods::className("ready4_local",".GlobalEnv"), function (x, value) 
{
    x@save_lgl <- value
    methods::validObject(x)
    x
})
