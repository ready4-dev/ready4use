#' merge_itms_chr
#' @description S4 Generic function to get the value of the slot merge_itms_chr
#' @rdname merge_itms_chr-methods
#' @param x An object 
#' 
#' @export
methods::setGeneric("merge_itms_chr", function(x) standardGeneric("merge_itms_chr"))
#' merge_itms_chr
#' @name merge_itms_chr-Ready4useFiles
#' @description Get the value of the slot merge_itms_chr for S4 objects of class Ready4useFiles
#' @param x An object of class Ready4useFiles
#' @rdname merge_itms_chr-methods
#' @aliases merge_itms_chr,Ready4useFiles-method
methods::setMethod("merge_itms_chr", methods::className("Ready4useFiles"), function (x) 
{
    x@merge_itms_chr
})
#' merge_itms_chr<-
#' @description S4 Generic function to set the value of the slot merge_itms_chr
#' @rdname merge_itms_chr_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export
methods::setGeneric("merge_itms_chr<-", function(x, value) standardGeneric("merge_itms_chr<-"))
#' merge_itms_chr<-
#' @name merge_itms_chr<--Ready4useFiles
#' @description Set the value of the slot merge_itms_chr for S4 objects of class Ready4useFiles
#' @param x An object of class Ready4useFiles
#' @rdname merge_itms_chr_set-methods
#' @aliases merge_itms_chr<-,Ready4useFiles-method
methods::setMethod("merge_itms_chr<-", methods::className("Ready4useFiles"), function (x, value) 
{
    x@merge_itms_chr <- value
    methods::validObject(x)
    x
})
