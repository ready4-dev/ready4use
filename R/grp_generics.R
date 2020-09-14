#' Download data
#' @name download_data
#' @description download_data() is a Download Data generic that downloads data files.
#' @param x An object
#' @param ... Additional arguments
#' @export 

download_data <- function (x, ...) 
{
    UseMethod("download_data", x)
}
#' Get data
#' @name get_data
#' @description get_data() is a Get Data generic that retrieves data from R objects loaded in memory.
#' @param x An object
#' @param ... Additional arguments
#' @export 

get_data <- function (x, ...) 
{
    UseMethod("get_data", x)
}
#' Get import type list
#' @name get_import_type_ls
#' @description get_import_type_ls() is a Get Import Type List generic that retrieves data about the type of import to be processed.
#' @param x An object
#' @param ... Additional arguments
#' @export 

get_import_type_ls <- function (x, ...) 
{
    UseMethod("get_import_type_ls", x)
}
#' Get read function
#' @name get_read_fn
#' @description get_read_fn() is a Get Read Function generic that retrieves a read function.
#' @param x An object
#' @param ... Additional arguments
#' @export 

get_read_fn <- function (x, ...) 
{
    UseMethod("get_read_fn", x)
}
#' Import data
#' @name import_data
#' @description import_data() is an Import Data generic that imports data from saved files and loads them into memory as R objects.
#' @param x An object
#' @param ... Additional arguments
#' @export 

import_data <- function (x, ...) 
{
    UseMethod("import_data", x)
}
#' Make dataverse import lookup table
#' @name make_dv_import_lup
#' @description make_dv_import_lup() is a Make Dataverse Import Lookup Table generic that makes a Dataverse import lookup table
#' @param x An object
#' @param ... Additional arguments
#' @export 

make_dv_import_lup <- function (x, ...) 
{
    UseMethod("make_dv_import_lup", x)
}
#' Make import output object of multiple potential types
#' @name make_import_xx
#' @description make_import_xx() is a Make Import Output Object of Multiple Potential Types generic that makes an output object of multiple potential classes.
#' @param x An object
#' @param ... Additional arguments
#' @export 

make_import_xx <- function (x, ...) 
{
    UseMethod("make_import_xx", x)
}
#' Save raw
#' @name save_raw
#' @description save_raw() is a Save Raw generic that saves the native version of a file format.
#' @param x An object
#' @param ... Additional arguments
#' @export 

save_raw <- function (x, ...) 
{
    UseMethod("save_raw", x)
}
#' Update source local to url
#' @name update_src_loc_to_url
#' @description update_src_loc_to_url() is an Update Source Local to Url generic that updates data from a local file reference to a URL
#' @param x An object
#' @param local_to_url_vec PARAM_DESCRIPTION
#' @param urls_vec PARAM_DESCRIPTION
#' @param ... Additional arguments
#' @export 

update_src_loc_to_url <- function (x, local_to_url_vec, urls_vec, ...) 
{
    UseMethod("update_src_loc_to_url", x)
}
#' Update this
#' @name update_this
#' @description update_this() is an Update this generic that updates and object
#' @param x An object
#' @param ... Additional arguments
#' @export 

update_this <- function (x, ...) 
{
    UseMethod("update_this", x)
}
