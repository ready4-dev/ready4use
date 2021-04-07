#' crs_nbr_dbl
#' @name crs_nbr_dbl-ready4_script_data
#' @description Get the value of the slot crs_nbr_dbl for S4 objects of class ready4_script_data
#' @param x An object of class ready4_script_data
#' @rdname crs_nbr_dbl-methods
#' @aliases crs_nbr_dbl,ready4_script_data-method
methods::setMethod("crs_nbr_dbl", methods::className("ready4_script_data"), function (x) 
{
    x@crs_nbr_dbl
})
#' crs_nbr_dbl<-
#' @name crs_nbr_dbl<--ready4_script_data
#' @description Set the value of the slot crs_nbr_dbl for S4 objects of class ready4_script_data
#' @param x An object of class ready4_script_data
#' @rdname crs_nbr_dbl_set-methods
#' @aliases crs_nbr_dbl<-,ready4_script_data-method
methods::setMethod("crs_nbr_dbl<-", methods::className("ready4_script_data"), function (x, value) 
{
    x@crs_nbr_dbl <- value
    methods::validObject(x)
    x
})
