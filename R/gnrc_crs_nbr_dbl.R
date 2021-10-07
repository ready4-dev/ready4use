#' crs_nbr_dbl
#' @description S4 Generic function to get the value of the slot crs_nbr_dbl
#' @rdname crs_nbr_dbl-methods
#' @param x An object 
#' 
#' @export
methods::setGeneric("crs_nbr_dbl", function(x) standardGeneric("crs_nbr_dbl"))
#' crs_nbr_dbl
#' @name crs_nbr_dbl-Ready4useArguments
#' @description Get the value of the slot crs_nbr_dbl for S4 objects of class Ready4useArguments
#' @param x An object of class Ready4useArguments
#' @rdname crs_nbr_dbl-methods
#' @aliases crs_nbr_dbl,Ready4useArguments-method
methods::setMethod("crs_nbr_dbl", methods::className("Ready4useArguments"), function (x) 
{
    x@crs_nbr_dbl
})
#' crs_nbr_dbl<-
#' @description S4 Generic function to set the value of the slot crs_nbr_dbl
#' @rdname crs_nbr_dbl_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export
methods::setGeneric("crs_nbr_dbl<-", function(x, value) standardGeneric("crs_nbr_dbl<-"))
#' crs_nbr_dbl<-
#' @name crs_nbr_dbl<--Ready4useArguments
#' @description Set the value of the slot crs_nbr_dbl for S4 objects of class Ready4useArguments
#' @param x An object of class Ready4useArguments
#' @rdname crs_nbr_dbl_set-methods
#' @aliases crs_nbr_dbl<-,Ready4useArguments-method
methods::setMethod("crs_nbr_dbl<-", methods::className("Ready4useArguments"), function (x, value) 
{
    x@crs_nbr_dbl <- value
    methods::validObject(x)
    x
})
