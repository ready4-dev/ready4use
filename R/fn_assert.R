#' Assert directory exists
#' @description assert_dir_exists() is an Assert function that validates that an object conforms to required condition(s). If the object does not meet all required conditions, program execution will be stopped and an error message provided. Specifically, this function implements an algorithm to assert directory exists. Function argument x specifies the object on which assert validation checks are to be performed. The function is called for its side effects and does not return a value.
#' @param x An object
#' @return NULL
#' @rdname assert_dir_exists
#' @export 
#' @importFrom testit assert
#' @keywords internal
assert_dir_exists <- function (x) 
{
    testit::assert("Not a valid path to an existing directory.", 
        dir.exists(x))
}
#' Assert file exists
#' @description assert_file_exists() is an Assert function that validates that an object conforms to required condition(s). If the object does not meet all required conditions, program execution will be stopped and an error message provided. Specifically, this function implements an algorithm to assert file exists. Function argument x specifies the object on which assert validation checks are to be performed. The function is called for its side effects and does not return a value.
#' @param x An object
#' @return NULL
#' @rdname assert_file_exists
#' @export 
#' @importFrom testit assert
#' @keywords internal
assert_file_exists <- function (x) 
{
    testit::assert("Not a valid path to an existing file.", file.exists(x))
}
#' Assert matches character vector
#' @description assert_matches_chr() is an Assert function that validates that an object conforms to required condition(s). If the object does not meet all required conditions, program execution will be stopped and an error message provided. Specifically, this function implements an algorithm to assert matches character vector. Function argument x specifies the object on which assert validation checks are to be performed. Argument match_chr provides the object containing values used for validation tests. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param match_chr Match (a character vector)
#' @return NULL
#' @rdname assert_matches_chr
#' @export 
#' @importFrom testit assert
#' @keywords internal
assert_matches_chr <- function (x, match_chr) 
{
    testit::assert("Length of character vector does not equal one.", 
        length(x) == 1)
    testit::assert(paste0("Value of character vector does not match '", 
        match_chr, "'"), x == match_chr)
}
#' Assert single row tibble
#' @description assert_single_row_tb() is an Assert function that validates that an object conforms to required condition(s). If the object does not meet all required conditions, program execution will be stopped and an error message provided. Specifically, this function implements an algorithm to assert single row tibble. Function argument x specifies the object on which assert validation checks are to be performed. The function is called for its side effects and does not return a value.
#' @param x An object
#' @return NULL
#' @rdname assert_single_row_tb
#' @export 
#' @importFrom testit assert
#' @importFrom tibble is_tibble
#' @keywords internal
assert_single_row_tb <- function (x) 
{
    testit::assert("Object is not a tibble", tibble::is_tibble(x))
    testit::assert("Tibble does not have exactly one row", nrow(x) == 
        1)
}
