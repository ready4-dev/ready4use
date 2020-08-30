#' Write dataverse file file
#' @description write_dv_file_fl() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write dv a file file.NA
#' @param database_ui_chr Database ui (a character vector of length 1)
#' @param filename_chr Filename (a character vector of length 1)
#' @param repo_file_format PARAM_DESCRIPTION
#' @param dataverse_chr Dataverse (a character vector of length 1), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param save_type_chr Save type (a character vector of length 1), Default: 'original'
#' @param destination_path_chr Destination path (a character vector of length 1)
#' @return NULL
#' @rdname write_dv_file_fl
#' @export 
#' @importFrom dataverse get_file
#' @keywords internal
write_dv_file_fl <- function (database_ui_chr, filename_chr, repo_file_format, dataverse_chr = Sys.getenv("DATAVERSE_SERVER"), 
    save_type_chr = "original", destination_path_chr) 
{
    writeBin(dataverse::get_file(paste0(filename_chr, repo_file_format), 
        database_ui_chr, format = save_type_chr, server = dataverse_chr), 
        destination_path_chr)
}
