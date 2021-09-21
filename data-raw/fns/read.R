read_import_from_csv <- function(file_ref_chr,
                                 is_url_1L_lgl = F){
  if(is_url_1L_lgl)
    read_in_file <- data.table::fread(file_ref_chr, stringsAsFactors = F)
  else
    read_in_file <- utils::read.csv(file = file_ref_chr,stringsAsFactors = F)
  import_r3 <- make_r3_from_csv_tb(read_in_file,
                                   ready4use_import_lup) # CHECK
  return(import_r3)
}
