bind_lups <- function(x,
                     ...){
  UseMethod("bind_lups",x)
}
download_data <- function(x,
                          ...){
  UseMethod("download_data",x)
}
# methods::setGeneric("download_data")

get_data <- function(x,
                     ...){
  UseMethod("get_data",x)
}
# methods::setGeneric("get_data")
import_data <- function(x,
                        ...){
  UseMethod("import_data",x)
}
# methods::setGeneric("import_data")

get_import_type_ls <- function(x,
                               ...){
  UseMethod("get_import_type_ls",x)
}

get_read_fn <- function(x,
                        ...){
  UseMethod("get_read_fn",x)
}

make_dv_import_lup <- function(x,
                               ...){
  UseMethod("make_dv_import_lup",x)
}

make_import_xx <- function(x,
                           ...){
  UseMethod("make_import_xx",x)
}
# methods::setGeneric("make_local_proc_r4", function(x,
#                                                    ...) standardGeneric("make_local_proc_r4"))
save_raw <- function(x,
                     ...){
  UseMethod("save_raw",x)
}
#methods::setGeneric("save_raw")

update_src_loc_to_url <- function(x,
                                  local_to_url_vec,
                                  urls_vec,
                                  ...){
  UseMethod("update_src_loc_to_url",x)
}
update_this <- function(x,
                        ...){
  UseMethod("update_this",x)
}
#methods::setGeneric("update_this")



