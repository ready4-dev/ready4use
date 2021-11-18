#devtools::document()
devtools::load_all()
# Ready4useIngest <- methods::setClass("Ready4useIngest",
#                                      contains = "Ready4Module",
#                                      slots = c(objects_ls = "list",
#                                                names_chr = "character",
#                                                descriptions_chr = "character"),
#                                      prototype = list(objects_ls = NULL,
#                                                       names_chr = NA_character_,
#                                                       descriptions_chr = NA_character_))
# Ready4usePaths <- methods::setClass("Ready4usePaths",
#                                     contains = "Ready4Module")
# Ready4useRepos <- methods::setClass("Ready4useRepos",
#                                     contains = "Ready4Module",
#                                     slots = c(dataverse_nm_1L_chr = "character",
#                                               dv_ds_metadata_ls = "list",
#                                               dv_ds_nm_1L_chr = "character",
#                                               dv_url_pfx_1L_chr = "character",
#                                               rds_objs_nms_chr = "character",
#                                               server_1L_chr = "character"),
#                                     prototype = list(dataverse_nm_1L_chr = NA_character_,
#                                                      dv_ds_metadata_ls = NULL,
#                                                      dv_ds_nm_1L_chr = NA_character_,
#                                                      dv_url_pfx_1L_chr = character(0),
#                                                      rds_objs_nms_chr = NA_character_,
#                                                      server_1L_chr = "dataverse.harvard.edu"))
# Ready4usePointer <- methods::setClass("Ready4usePointer",
#                                       contains = "Ready4Module",
#                                       slots = c(a_Ready4usePaths = "Ready4Module",
#                                                 b_Ready4useRepos = "Ready4useRepos"),
#                                       prototype = list(a_Ready4usePaths = Ready4Module(),
#                                                        b_Ready4useRepos = Ready4useRepos()))
# Ready4useRecord <- methods::setClass("Ready4useRecord",
#                                       contains = "Ready4Module",
#                                       slots = c(a_Ready4usePointer = "Ready4usePointer",
#                                                 b_Ready4useIngest = "Ready4useIngest"),
#                                       prototype = list(a_Ready4usePointer = Ready4usePointer(),
#                                                        b_Ready4useIngest = Ready4useIngest()))
# methods::setMethod("share",
#                    methods::className("Ready4useRecord"#, package = "ready4use"
#                    ),
#                    share_Ready4useRecord)
# methods::setMethod("ingest",
#                    methods::className("Ready4useRepos"#, package = "ready4use"
#                    ),
#                    ingest_Ready4useRepos)
# Specify data source
# ymh_clinical_tb <- youthvars::replication_popl_tb
# ymh_clinical_dict_r3 <- youthvars::repln_ds_dict_r3
#
x <- Ready4useRepos(dataverse_nm_1L_chr = "fakes",
                    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/HJXYKQ",
                    server_1L_chr = "dataverse.harvard.edu")
# Ingest data
x <- ingest(x)
# Inspect data
y <- Ready4useDyad(ds_tb = x_Ready4useRecord@b_Ready4useIngest@objects_ls$ymh_clinical_tb,
                   dictionary_r3 = x_Ready4useRecord@b_Ready4useIngest@objects_ls$ymh_clinical_dict_r3)
exhibit(y,
        display_1L_chr = "head")
exhibit(y,
        type_1L_chr = "dict")
# Share data
y <- Ready4useIngest(objects_ls = list(y),
                                     names_chr = c("ymh_clinical_dyad_r4"),
                                     descriptions_chr = c("An example of a Ready4useDyad - a dataset and data dictionary pair. Note this example uses fake data"))
z <- Ready4usePointer(b_Ready4useRepos = Ready4useRepos(dataverse_nm_1L_chr = "fakes",
                                                        dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/W95KED",
                                                        server_1L_chr = "dataverse.harvard.edu"))
y <- Ready4useRecord(a_Ready4usePointer = z,
                     b_Ready4useIngest = y)
ready4::share(y,
              publish_dv_1L_lgl = T)
