#' raw_data_dir_chr
#' @description S4 Generic function to get the value of the slot raw_data_dir_chr
#' @name raw_data_dir_chr
#' @param x An object 
#' 
#' @export

methods::setGeneric("raw_data_dir_chr", function(x) standardGeneric("raw_data_dir_chr"))
#' raw_data_dir_chr
#' @name raw_data_dir_chr-ready4_local
#' @description Get the value of the slot raw_data_dir_chr for S4 objects of class ready4_local
#' @param x An object of class ready4_local
#' @rdname raw_data_dir_chr
methods::setMethod("raw_data_dir_chr", methods::className("ready4_local",".GlobalEnv"), function (x) 
{
    x@raw_data_dir_chr
})
#' raw_data_dir_chr<-
#' @description S4 Generic function to set the value of the slot raw_data_dir_chr
#' @name raw_data_dir_chr<-
#' @param x An object 
#' 
#' @export

methods::setGeneric("raw_data_dir_chr<-", function(x, value) standardGeneric("raw_data_dir_chr<-"))
#' raw_data_dir_chr<-
#' @name raw_data_dir_chr<--ready4_local
#' @description Set the value of the slot raw_data_dir_chr for S4 objects of class ready4_local
#' @param x An object of class ready4_local
#' @rdname raw_data_dir_chr-set
methods::setMethod("raw_data_dir_chr<-", methods::className("ready4_local",".GlobalEnv"), function (x, value) 
{
    x@raw_data_dir_chr <- value
    methods::validObject(x)
    x
})
