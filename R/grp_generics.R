#' Download data
#' @rdname download_data-methods
#' @description download_data() is a Download Data generic that downloads data files.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

download_data <- function (x, ...) 
{
    UseMethod("download_data", x)
}
methods::setGeneric("download_data")
#' Get data
#' @rdname get_data-methods
#' @description get_data() is a Get Data generic that retrieves data from R objects loaded in memory.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

get_data <- function (x, ...) 
{
    UseMethod("get_data", x)
}
methods::setGeneric("get_data")
#' Get import type list
#' @rdname get_import_type_ls-methods
#' @description get_import_type_ls() is a Get Import Type List generic that retrieves data about the type of import to be processed.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

get_import_type_ls <- function (x, ...) 
{
    UseMethod("get_import_type_ls", x)
}
methods::setGeneric("get_import_type_ls")
#' Get read function
#' @rdname get_read_fn-methods
#' @description get_read_fn() is a Get Read Function generic that retrieves a read function.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

get_read_fn <- function (x, ...) 
{
    UseMethod("get_read_fn", x)
}
methods::setGeneric("get_read_fn")
#' Import data
#' @rdname import_data-methods
#' @description import_data() is an Import Data generic that imports data from saved files and loads them into memory as R objects.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

import_data <- function (x, ...) 
{
    UseMethod("import_data", x)
}
methods::setGeneric("import_data")
#' Make dataverse import lookup table
#' @rdname make_dv_import_lup-methods
#' @description make_dv_import_lup() is a Make Dataverse Import Lookup Table generic that makes a Dataverse import lookup table
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

make_dv_import_lup <- function (x, ...) 
{
    UseMethod("make_dv_import_lup", x)
}
methods::setGeneric("make_dv_import_lup")
#' Make import output object of multiple potential types
#' @rdname make_import_xx-methods
#' @description make_import_xx() is a Make Import Output Object of Multiple Potential Types generic that makes an output object of multiple potential classes.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

make_import_xx <- function (x, ...) 
{
    UseMethod("make_import_xx", x)
}
methods::setGeneric("make_import_xx")
#' Save raw
#' @rdname save_raw-methods
#' @description save_raw() is a Save Raw generic that saves the native version of a file format.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

save_raw <- function (x, ...) 
{
    UseMethod("save_raw", x)
}
methods::setGeneric("save_raw")
#' Update source local to url
#' @rdname update_src_loc_to_url-methods
#' @description update_src_loc_to_url() is an Update Source Local to Url generic that updates data from a local file reference to a URL
#' @param x An object
#' @param local_to_url_vec_chr Local to url vector (a character vector)
#' @param urls_vec_chr Urls vector (a character vector)
#' @param ... Additional arguments (an additional arguments)
#' @export 

update_src_loc_to_url <- function (x, local_to_url_vec_chr, urls_vec_chr, ...) 
{
    UseMethod("update_src_loc_to_url", x)
}
methods::setGeneric("update_src_loc_to_url")
#' Update this
#' @rdname update_this-methods
#' @description update_this() is an Update this generic that updates and object
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

update_this <- function (x, ...) 
{
    UseMethod("update_this", x)
}
methods::setGeneric("update_this")
