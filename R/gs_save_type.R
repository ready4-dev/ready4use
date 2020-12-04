#' save_type
#' @name save_type-ready4_local_raw
#' @description Get the value of the slot save_type for S4 objects of class ready4_local_raw
#' @param x An object of class ready4_local_raw
#' @rdname save_type-methods
#' @aliases save_type,ready4_local_raw-method
methods::setMethod("save_type", methods::className("ready4_local_raw"), function (x) 
{
    x@save_type
})
#' save_type<-
#' @name save_type<--ready4_local_raw
#' @description Set the value of the slot save_type for S4 objects of class ready4_local_raw
#' @param x An object of class ready4_local_raw
#' @rdname save_type_set-methods
#' @aliases save_type<-,ready4_local_raw-method
methods::setMethod("save_type<-", methods::className("ready4_local_raw"), function (x, value) 
{
    x@save_type <- value
    methods::validObject(x)
    x
})
#' save_type
#' @name save_type-ready4_local_proc
#' @description Get the value of the slot save_type for S4 objects of class ready4_local_proc
#' @param x An object of class ready4_local_proc
#' @rdname save_type-methods
#' @aliases save_type,ready4_local_proc-method
methods::setMethod("save_type", methods::className("ready4_local_proc"), function (x) 
{
    x@save_type
})
#' save_type<-
#' @name save_type<--ready4_local_proc
#' @description Set the value of the slot save_type for S4 objects of class ready4_local_proc
#' @param x An object of class ready4_local_proc
#' @rdname save_type_set-methods
#' @aliases save_type<-,ready4_local_proc-method
methods::setMethod("save_type<-", methods::className("ready4_local_proc"), function (x, value) 
{
    x@save_type <- value
    methods::validObject(x)
    x
})
