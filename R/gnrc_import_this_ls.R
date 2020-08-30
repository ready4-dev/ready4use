#' import_this_ls
#' @description S4 Generic function to get the value of the slot import_this_ls
#' @name import_this_ls
#' @param x An object 
#' 
#' @export

methods::setGeneric("import_this_ls", function(x) standardGeneric("import_this_ls"))
#' import_this_ls
#' @name import_this_ls-ready4_local_proc
#' @description Get the value of the slot import_this_ls for S4 objects of class ready4_local_proc
#' @param x An object of class ready4_local_proc
#' @rdname import_this_ls
methods::setMethod("import_this_ls", methods::className("ready4_local_proc",".GlobalEnv"), function (x) 
{
    x@import_this_ls
})
#' import_this_ls<-
#' @description S4 Generic function to set the value of the slot import_this_ls
#' @name import_this_ls<-
#' @param x An object 
#' 
#' @export

methods::setGeneric("import_this_ls<-", function(x, value) standardGeneric("import_this_ls<-"))
#' import_this_ls<-
#' @name import_this_ls<--ready4_local_proc
#' @description Set the value of the slot import_this_ls for S4 objects of class ready4_local_proc
#' @param x An object of class ready4_local_proc
#' @rdname import_this_ls-set
methods::setMethod("import_this_ls<-", methods::className("ready4_local_proc",".GlobalEnv"), function (x, value) 
{
    x@import_this_ls <- value
    methods::validObject(x)
    x
})
