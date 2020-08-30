write_dv_file_fl <- function(database_ui_chr,
                         filename_chr,
                         repo_file_format,
                         dataverse_chr = Sys.getenv("DATAVERSE_SERVER"),
                         save_type_chr = "original",
                         destination_path_chr){
  writeBin(dataverse::get_file(paste0(filename_chr,repo_file_format),
                               database_ui_chr,
                               format = save_type_chr,
                               server = dataverse_chr),
           destination_path_chr)
}
