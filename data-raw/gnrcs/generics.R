bind_lups <- function(x,
                     ...){
  UseMethod("bind_lups",x)
}
download_data <- function(x,
                          ...){
  UseMethod("download_data",x)
}
import_data <- function(x,
                        ...){
  UseMethod("import_data",x)
}
get_read_fn <- function(x,
                        ...){
  UseMethod("get_read_fn",x)
}
save_raw <- function(x,
                     ...){
  UseMethod("save_raw",x)
}
update_src_loc_to_url <- function(x,
                                  local_to_url_vec_chr,
                                  urls_vec_chr,
                                  ...){
  UseMethod("update_src_loc_to_url",x)
}
update_this <- function(x,
                        ...){
  UseMethod("update_this",x)
}
