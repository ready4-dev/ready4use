#' Ready4useRecord
#' 
#' Ingested data, descriptive metadata and provenance details.
#' 
#' @include C4_Ready4usePointer.R C4_Ready4useIngest.R
#' @slot a_Ready4usePointer  (an instance of the Ready4usePointer class)
#' @slot b_Ready4useIngest  (an instance of the Ready4useIngest class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name Ready4useRecord-class
#' @rdname Ready4useRecord-class
#' @export Ready4useRecord
#' @exportClass Ready4useRecord
Ready4useRecord <- methods::setClass("Ready4useRecord",
contains = "Ready4Module",
slots = c(a_Ready4usePointer = "Ready4usePointer",b_Ready4useIngest = "Ready4useIngest",dissemination_1L_chr = "character"),
prototype =  list(a_Ready4usePointer = Ready4usePointer(),b_Ready4useIngest = Ready4useIngest()))


methods::setValidity(methods::className("Ready4useRecord"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
