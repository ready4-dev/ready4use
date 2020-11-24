devtools::load_all()
#crs_nbr_vec <- c(4283,3577)
library(magrittr)
#data_dir_chr <- "../../../../Data"
# raw_data_dir_chr <- paste0(data_dir_chr,"/Raw_Format")
# r_data_dir_chr <- paste0(data_dir_chr,"/R_Format")
#dv <- dataverse::create_dataverse("Code")
#parent_dv_dir_1L_chr = paste0(data_dir_chr,"/Dataverse")
ds_ls <- write_pkg_dss_to_dv_ds_csvs(pkg_dss_tb,
                                     dv_nm_1L_chr = "ready4work")
##
#AFTER MAKE PUBLIC
pkg_dss_tb <- prototype_lup %>%
ready4fun::write_and_doc_ds(db_1L_chr = "prototype_lup",
                            title_1L_chr = "Class prototype lookup table",
                            desc_1L_chr = "Metadata on classes used in readyforwhatsnext suite",
                            url_1L_chr = "https://doi.org/10.7910/DVN/OZLSLR/YH1WVF",
                            pkg_dss_tb = pkg_dss_tb)
#dataverse::publish_dataset(ds_ls$datasetPersistentId)
#dataverse::delete_dataset(ds_ls$datasetPersistentId)
# purrr::pwalk(files_tb,
#      ~ file.copy(paste0(..1,"/",..2,..3),
#                 local_dv_dir_1L_chr))
#dir_chr <- paste0(dv_dir_chr,"/Spring To Life SD Australia/Data")
# files_tb <- tibble::tibble(dir_chr = paste0(dir_chr,"/",c("Geometries","Attributes")),
#                            file_chr = c("PHN_boundaries_AUS_May2017_V7_Shapefile",
#                                         "TESTFILE"),
#                            file_type_chr = c(".zip",".xlsx"),
#                            description_chr = c("Test shape file",
#                                                "Test xlsx file"),
#                            ds_file_ext_chr = c(".zip",".tab"))

## Note: Shape files or other zip files will only be downloaded for a local copy if dataset is published

## UPDATE dv import lup
