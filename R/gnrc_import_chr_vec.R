#' import_chr_vec
#' @description S4 Generic function to get the value of the slot import_chr_vec
#' @name import_chr_vec
#' @param x An object 
#' 
#' @export

methods::setGeneric("import_chr_vec", function(x) standardGeneric("import_chr_vec"))
#' import_chr_vec
#' @name import_chr_vec-ready4_local_proc
#' @description Get the value of the slot import_chr_vec for S4 objects of class ready4_local_proc
#' @param x An object of class ready4_local_proc
#' @rdname import_chr_vec
methods::setMethod("import_chr_vec", methods::className("ready4_local_proc",".GlobalEnv"), function (x) 
{
    x@import_chr_vec
})
#' import_chr_vec<-
#' @description S4 Generic function to set the value of the slot import_chr_vec
#' @name import_chr_vec<-
#' @param x An object 
#' 
#' @export

methods::setGeneric("import_chr_vec<-", function(x, value) standardGeneric("import_chr_vec<-"))
#' import_chr_vec<-
#' @name import_chr_vec<--ready4_local_proc
#' @description Set the value of the slot import_chr_vec for S4 objects of class ready4_local_proc
#' @param x An object of class ready4_local_proc
#' @rdname import_chr_vec-set
methods::setMethod("import_chr_vec<-", methods::className("ready4_local_proc",".GlobalEnv"), function (x, value) 
{
    x@import_chr_vec <- value
    methods::validObject(x)
    x
})
