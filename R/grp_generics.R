#' Get read function
#' @rdname get_read_fn-methods
#' @description get_read_fn() is a Get Read Function function that retrieves a read function. Specifically, this function implements an algorithm to get read function. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 
get_read_fn <- function (x, ...) 
{
    UseMethod("get_read_fn", x)
}
methods::setGeneric("get_read_fn")
