#' Ready4usePointer
#' 
#' Metadata on local and remote data storage locations.
#' 
#' @include C4_Ready4useRepos.R
#' @slot a_Ready4usePaths  (an instance of the Ready4Module class)
#' @slot b_Ready4useRepos  (an instance of the Ready4useRepos class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name Ready4usePointer-class
#' @rdname Ready4usePointer-class
#' @export Ready4usePointer
#' @exportClass Ready4usePointer
Ready4usePointer <- methods::setClass("Ready4usePointer",
contains = "Ready4Module",
slots = c(a_Ready4usePaths = "Ready4Module",b_Ready4useRepos = "Ready4useRepos",dissemination_1L_chr = "character"),
prototype =  list(a_Ready4usePaths = ready4::Ready4Module(),b_Ready4useRepos = Ready4useRepos()))


methods::setValidity(methods::className("Ready4usePointer"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
