read_import_from_csv <- function(file_ref_chr,
                                 is_url = F){
  if(is_url)
    read_in_file <- data.table::fread(file_ref_chr, stringsAsFactors = F)
  else
    read_in_file <- read.csv(file = file_ref_chr,stringsAsFactors = F)
  import_r3 <- make_r3_from_csv_tb(read_in_file,
                                   ready4_import_lup)
  return(import_r3)
}
