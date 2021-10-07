#' write_type_1L_chr
#' @description S4 Generic function to get the value of the slot write_type_1L_chr
#' @rdname write_type_1L_chr-methods
#' @param x An object 
#' 
#' @export
methods::setGeneric("write_type_1L_chr", function(x) standardGeneric("write_type_1L_chr"))
#' write_type_1L_chr
#' @name write_type_1L_chr-Ready4useRaw
#' @description Get the value of the slot write_type_1L_chr for S4 objects of class Ready4useRaw
#' @param x An object of class Ready4useRaw
#' @rdname write_type_1L_chr-methods
#' @aliases write_type_1L_chr,Ready4useRaw-method
methods::setMethod("write_type_1L_chr", methods::className("Ready4useRaw"), function (x) 
{
    x@write_type_1L_chr
})
#' write_type_1L_chr<-
#' @description S4 Generic function to set the value of the slot write_type_1L_chr
#' @rdname write_type_1L_chr_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export
methods::setGeneric("write_type_1L_chr<-", function(x, value) standardGeneric("write_type_1L_chr<-"))
#' write_type_1L_chr<-
#' @name write_type_1L_chr<--Ready4useRaw
#' @description Set the value of the slot write_type_1L_chr for S4 objects of class Ready4useRaw
#' @param x An object of class Ready4useRaw
#' @rdname write_type_1L_chr_set-methods
#' @aliases write_type_1L_chr<-,Ready4useRaw-method
methods::setMethod("write_type_1L_chr<-", methods::className("Ready4useRaw"), function (x, value) 
{
    x@write_type_1L_chr <- value
    methods::validObject(x)
    x
})
#' write_type_1L_chr
#' @name write_type_1L_chr-Ready4useProcessed
#' @description Get the value of the slot write_type_1L_chr for S4 objects of class Ready4useProcessed
#' @param x An object of class Ready4useProcessed
#' @rdname write_type_1L_chr-methods
#' @aliases write_type_1L_chr,Ready4useProcessed-method
methods::setMethod("write_type_1L_chr", methods::className("Ready4useProcessed"), function (x) 
{
    x@write_type_1L_chr
})
#' write_type_1L_chr<-
#' @name write_type_1L_chr<--Ready4useProcessed
#' @description Set the value of the slot write_type_1L_chr for S4 objects of class Ready4useProcessed
#' @param x An object of class Ready4useProcessed
#' @rdname write_type_1L_chr_set-methods
#' @aliases write_type_1L_chr<-,Ready4useProcessed-method
methods::setMethod("write_type_1L_chr<-", methods::className("Ready4useProcessed"), function (x, value) 
{
    x@write_type_1L_chr <- value
    methods::validObject(x)
    x
})
