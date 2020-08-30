assert_single_row_tb <- function(x){
  testit::assert("Object is not a tibble",tibble::is_tibble(x))
  testit::assert("Tibble does not have exactly one row",nrow(x)==1)
}
assert_file_exists <- function(x){
  testit::assert("Not a valid path to an existing file.", file.exists(x))
}
assert_dir_exists <- function(x){
  testit::assert("Not a valid path to an existing directory.", dir.exists(x))
}
assert_matches_chr <- function(x,
                               match_chr){
  testit::assert("Length of character vector does not equal one.", length(x)==1)
  testit::assert(paste0("Value of character vector does not match \'",match_chr,"\'"), x==match_chr)
}
