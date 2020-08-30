#' proc_data_dir_chr
#' @description S4 Generic function to get the value of the slot proc_data_dir_chr
#' @name proc_data_dir_chr
#' @param x An object 
#' 
#' @export

methods::setGeneric("proc_data_dir_chr", function(x) standardGeneric("proc_data_dir_chr"))
#' proc_data_dir_chr
#' @name proc_data_dir_chr-ready4_local_proc
#' @description Get the value of the slot proc_data_dir_chr for S4 objects of class ready4_local_proc
#' @param x An object of class ready4_local_proc
#' @rdname proc_data_dir_chr
methods::setMethod("proc_data_dir_chr", methods::className("ready4_local_proc",".GlobalEnv"), function (x) 
{
    x@proc_data_dir_chr
})
#' proc_data_dir_chr<-
#' @description S4 Generic function to set the value of the slot proc_data_dir_chr
#' @name proc_data_dir_chr<-
#' @param x An object 
#' 
#' @export

methods::setGeneric("proc_data_dir_chr<-", function(x, value) standardGeneric("proc_data_dir_chr<-"))
#' proc_data_dir_chr<-
#' @name proc_data_dir_chr<--ready4_local_proc
#' @description Set the value of the slot proc_data_dir_chr for S4 objects of class ready4_local_proc
#' @param x An object of class ready4_local_proc
#' @rdname proc_data_dir_chr-set
methods::setMethod("proc_data_dir_chr<-", methods::className("ready4_local_proc",".GlobalEnv"), function (x, value) 
{
    x@proc_data_dir_chr <- value
    methods::validObject(x)
    x
})
