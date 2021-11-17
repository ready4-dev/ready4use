#devtools::document()
devtools::load_all()
Ready4usePaths <- methods::setClass("Ready4usePaths",
                                    contains = "Ready4Module")
#
Ready4useRepos <- methods::setClass("Ready4useRepos",
                                    contains = "Ready4Module")
#
Ready4useDataverse <- methods::setClass("Ready4useDataverse",
                                        contains = "Ready4useRepos",
                                        slots = c(dv_ds_nm_1L_chr = "character",
                                                  dv_url_pfx_1L_chr = "character",
                                                  file_nms_chr = "character",
                                                  server_1L_chr = "character"),
                                        prototype = list(dv_ds_nm_1L_chr = NA_character_,
                                                         dv_url_pfx_1L_chr = character(0),
                                                         file_nms_chr = NA_character_,
                                                         server_1L_chr = "dataverse.harvard.edu"))
#
Ready4usePointer <- methods::setClass("Ready4usePointer",
                                      contains = "Ready4Module",
                                      slots = c(a_Ready4usePaths = "Ready4usePaths",
                                                b_Ready4useRepos = "Ready4useRepos"),
                                      prototype = list(a_Ready4usePaths = Ready4usePaths(),
                                                       b_Ready4useRepos = Ready4useRepos()))
#
Ready4useRecord <- methods::setClass("Ready4useRecord",
                                      contains = "Ready4Module",
                                      slots = c(a_Ready4usePointer = "Ready4usePointer",
                                                b_Ready4Module = "Ready4Module"),
                                      prototype = list(a_Ready4usePointer = Ready4usePointer(),
                                                       b_Ready4Module = Ready4Module()))
# Specify data source
x_Ready4usePointer <- Ready4usePointer(b_Ready4useRepos = Ready4useDataverse(dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/HJXYKQ",
                                                                             file_nms_chr = "eq5d_ds_tb",
                                                                             server_1L_chr = "dataverse.harvard.edu"))

# Ingest data
ready4::get_rds_from_dv(file_nm_1L_chr = "eq5d_ds_tb",
                        dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/HJXYKQ",
                        dv_url_pfx_1L_chr = character(0),
                        key_1L_chr = NULL,
                        server_1L_chr = "dataverse.harvard.edu")
ds_tb <- youthvars::replication_popl_tb
dictionary_r3 <- youthvars::repln_ds_dict_r3
#
x_Ready4useDyad <- Ready4useDyad(ds_tb = ds_tb,
                                 dictionary_r3 = dictionary_r3)
x_Ready4useRecord <- Ready4useRecord(a_Ready4usePointer = x_Ready4usePointer,
                                    b_Ready4Module = x_Ready4useDyad) # Original & Working?
# Characterize data

# Share data
