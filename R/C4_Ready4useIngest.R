#' Ready4useIngest
#' 
#' Ingested data and descriptive metadata.
#' 
#' @slot objects_ls Objects (a list)
#' @slot names_chr Names (a character vector)
#' @slot descriptions_chr Descriptions (a character vector)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name Ready4useIngest-class
#' @rdname Ready4useIngest-class
#' @export Ready4useIngest
#' @exportClass Ready4useIngest
Ready4useIngest <- methods::setClass("Ready4useIngest",
contains = "Ready4Module",
slots = c(objects_ls = "list",names_chr = "character",descriptions_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(objects_ls = list(list()),names_chr = NA_character_,descriptions_chr = NA_character_))


methods::setValidity(methods::className("Ready4useIngest"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
