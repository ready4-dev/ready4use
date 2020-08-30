#' Download data
#' @description download_data() is a Download Data generic that downloads data files.
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname download_data
#' @export 

#' @keywords internal
download_data <- function (x, ...) 
{
    UseMethod("download_data", x)
}
#' Get data
#' @description get_data() is a Get Data generic that retrieves data from R objects loaded in memory.
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_data
#' @export 

#' @keywords internal
get_data <- function (x, ...) 
{
    UseMethod("get_data", x)
}
#' Get import type list
#' @description get_import_type_ls() is a Get Import Type List generic that retrieves data about the type of import to be processed.NA
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_import_type_ls
#' @export 

#' @keywords internal
get_import_type_ls <- function (x, ...) 
{
    UseMethod("get_import_type_ls", x)
}
#' Get read function
#' @description get_read_fn() is a Get Read Function generic that retrieves a read function.NA
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_read_fn
#' @export 

#' @keywords internal
get_read_fn <- function (x, ...) 
{
    UseMethod("get_read_fn", x)
}
#' Import data
#' @description import_data() is an Import Data generic that imports data from saved files and loads them into memory as R objects.
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname import_data
#' @export 

#' @keywords internal
import_data <- function (x, ...) 
{
    UseMethod("import_data", x)
}
#' Make dataverse import lookup table
#' @description make_dv_import_lup() is a Make Dataverse Import Lookup Table generic that makes a Dataverse import lookup tableNA
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname make_dv_import_lup
#' @export 

#' @keywords internal
make_dv_import_lup <- function (x, ...) 
{
    UseMethod("make_dv_import_lup", x)
}
#' Make import output object of multiple potential types
#' @description make_import_xx() is a Make Import Output Object of Multiple Potential Types generic that makes an output object of multiple potential classes.NA
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname make_import_xx
#' @export 

#' @keywords internal
make_import_xx <- function (x, ...) 
{
    UseMethod("make_import_xx", x)
}
#' Save raw
#' @description save_raw() is a Save Raw generic that saves the native version of a file format.
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname save_raw
#' @export 

#' @keywords internal
save_raw <- function (x, ...) 
{
    UseMethod("save_raw", x)
}
#' Update source local to url
#' @description update_src_loc_to_url() is an Update Source Local to Url generic that updates data from a local file reference to a URL
#' @param x PARAM_DESCRIPTION
#' @param local_to_url_vec PARAM_DESCRIPTION
#' @param urls_vec PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname update_src_loc_to_url
#' @export 

#' @keywords internal
update_src_loc_to_url <- function (x, local_to_url_vec, urls_vec, ...) 
{
    UseMethod("update_src_loc_to_url", x)
}
#' Update this
#' @description update_this() is an Update this generic that updates and object
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return NULL
#' @rdname update_this
#' @export 

#' @keywords internal
update_this <- function (x, ...) 
{
    UseMethod("update_this", x)
}
