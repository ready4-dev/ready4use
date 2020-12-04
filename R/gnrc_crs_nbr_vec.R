#' crs_nbr_vec
#' @description S4 Generic function to get the value of the slot crs_nbr_vec
#' @rdname crs_nbr_vec-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("crs_nbr_vec", function(x) standardGeneric("crs_nbr_vec"))
#' crs_nbr_vec
#' @name crs_nbr_vec-ready4_script_data
#' @description Get the value of the slot crs_nbr_vec for S4 objects of class ready4_script_data
#' @param x An object of class ready4_script_data
#' @rdname crs_nbr_vec-methods
#' @aliases crs_nbr_vec,ready4_script_data-method
methods::setMethod("crs_nbr_vec", methods::className("ready4_script_data"), function (x) 
{
    x@crs_nbr_vec
})
#' crs_nbr_vec<-
#' @description S4 Generic function to set the value of the slot crs_nbr_vec
#' @rdname crs_nbr_vec_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("crs_nbr_vec<-", function(x, value) standardGeneric("crs_nbr_vec<-"))
#' crs_nbr_vec<-
#' @name crs_nbr_vec<--ready4_script_data
#' @description Set the value of the slot crs_nbr_vec for S4 objects of class ready4_script_data
#' @param x An object of class ready4_script_data
#' @rdname crs_nbr_vec_set-methods
#' @aliases crs_nbr_vec<-,ready4_script_data-method
methods::setMethod("crs_nbr_vec<-", methods::className("ready4_script_data"), function (x, value) 
{
    x@crs_nbr_vec <- value
    methods::validObject(x)
    x
})
