#' save_type
#' @description S4 Generic function to get the value of the slot save_type
#' @name save_type
#' @param x An object 
#' 
#' @export

methods::setGeneric("save_type", function(x) standardGeneric("save_type"))
#' save_type
#' @name save_type-ready4_local_raw
#' @description Get the value of the slot save_type for S4 objects of class ready4_local_raw
#' @param x An object of class ready4_local_raw
#' @rdname save_type
methods::setMethod("save_type", methods::className("ready4_local_raw",".GlobalEnv"), function (x) 
{
    x@save_type
})
#' save_type<-
#' @description S4 Generic function to set the value of the slot save_type
#' @name save_type<-
#' @param x An object 
#' 
#' @export

methods::setGeneric("save_type<-", function(x, value) standardGeneric("save_type<-"))
#' save_type<-
#' @name save_type<--ready4_local_raw
#' @description Set the value of the slot save_type for S4 objects of class ready4_local_raw
#' @param x An object of class ready4_local_raw
#' @rdname save_type-set
methods::setMethod("save_type<-", methods::className("ready4_local_raw",".GlobalEnv"), function (x, value) 
{
    x@save_type <- value
    methods::validObject(x)
    x
})
#' save_type
#' @name save_type-ready4_local_proc
#' @description Get the value of the slot save_type for S4 objects of class ready4_local_proc
#' @param x An object of class ready4_local_proc
#' @rdname save_type
methods::setMethod("save_type", methods::className("ready4_local_proc",".GlobalEnv"), function (x) 
{
    x@save_type
})
#' save_type<-
#' @name save_type<--ready4_local_proc
#' @description Set the value of the slot save_type for S4 objects of class ready4_local_proc
#' @param x An object of class ready4_local_proc
#' @rdname save_type-set
methods::setMethod("save_type<-", methods::className("ready4_local_proc",".GlobalEnv"), function (x, value) 
{
    x@save_type <- value
    methods::validObject(x)
    x
})
