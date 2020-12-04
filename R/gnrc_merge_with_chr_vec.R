#' merge_with_chr_vec
#' @description S4 Generic function to get the value of the slot merge_with_chr_vec
#' @rdname merge_with_chr_vec-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("merge_with_chr_vec", function(x) standardGeneric("merge_with_chr_vec"))
#' merge_with_chr_vec
#' @name merge_with_chr_vec-ready4_local
#' @description Get the value of the slot merge_with_chr_vec for S4 objects of class ready4_local
#' @param x An object of class ready4_local
#' @rdname merge_with_chr_vec-methods
#' @aliases merge_with_chr_vec,ready4_local-method
methods::setMethod("merge_with_chr_vec", methods::className("ready4_local"), function (x) 
{
    x@merge_with_chr_vec
})
#' merge_with_chr_vec<-
#' @description S4 Generic function to set the value of the slot merge_with_chr_vec
#' @rdname merge_with_chr_vec_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("merge_with_chr_vec<-", function(x, value) standardGeneric("merge_with_chr_vec<-"))
#' merge_with_chr_vec<-
#' @name merge_with_chr_vec<--ready4_local
#' @description Set the value of the slot merge_with_chr_vec for S4 objects of class ready4_local
#' @param x An object of class ready4_local
#' @rdname merge_with_chr_vec_set-methods
#' @aliases merge_with_chr_vec<-,ready4_local-method
methods::setMethod("merge_with_chr_vec<-", methods::className("ready4_local"), function (x, value) 
{
    x@merge_with_chr_vec <- value
    methods::validObject(x)
    x
})
