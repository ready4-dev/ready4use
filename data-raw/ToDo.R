objects_ls$eq5d_ds_dict
eq5d_ds_dict <- ready4use::ready4use_dictionary(objects_ls$eq5d_ds_dict %>% remove_labels_from_ds())
x <- Ready4useRepos(dv_nm_1L_chr = "fakes",
                    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/HJXYKQ",
                    dv_server_1L_chr = "dataverse.harvard.edu")
y <- Ready4usePointer(b_Ready4useRepos = x)
y <- Ready4useRecord(a_Ready4usePointer = y,
                     b_Ready4useIngest = Ready4useIngest(objects_ls = list(eq5d_ds_dict = eq5d_ds_dict),
                                                         descriptions_chr = c("Data dictionary for a synthetic (fake) dataset (clinical youth mental health, EQ-5D).")))
share(y)
eq5d_ds_dict <- eq5d_ds_dict %>% add_labels_from_dictionary(dictionary_tb = make_pt_ready4use_dictionary(var_nm_chr = names(eq5d_ds_dict),
                                                                                         var_desc_chr = c("Variable","Category",
                                                                                                          "Description", "Class")))
# procureSlot <- function (x, ...)
# {
#   UseMethod("procureSlot", x)
# }
# methods::setGeneric("procureSlot")
# methods::setMethod("procureSlot", "Ready4Module", function(x,
#                                                            slot_nm_1L_chr){
#   y_xx <- eval(parse(text = paste0("x@",slot_nm_1L_chr)))
#   return(y_xx)
# })
# #devtools::document()
# devtools::load_all()
# # Specify Data sources
# x <- Ready4useRepos(dataverse_nm_1L_chr = "fakes",
#                     dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/HJXYKQ",
#                     server_1L_chr = "dataverse.harvard.edu")
# # Ingest data
# x <- ingest(x)
# class(x)
# names(x@b_Ready4useIngest@objects_ls)
# # Inspect data
# y <- Ready4useDyad(ds_tb = x@b_Ready4useIngest@objects_ls$ymh_clinical_tb,
#                    dictionary_r3 = x@b_Ready4useIngest@objects_ls$ymh_clinical_dict_r3)
# exhibit(y,
#         display_1L_chr = "head")
# exhibit(y,
#         type_1L_chr = "dict")
# # Share data
# z <- Ready4usePointer(b_Ready4useRepos = Ready4useRepos(dataverse_nm_1L_chr = "fakes",
#                                                         dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/W95KED",
#                                                         server_1L_chr = "dataverse.harvard.edu"))
# y <- Ready4useRecord(a_Ready4usePointer = z,
#                      b_Ready4useIngest = Ready4useIngest(objects_ls = list(y),
#                                                          names_chr = c("ymh_clinical_dyad_r4"),
#                                                          descriptions_chr = c("An example of a Ready4useDyad - a dataset and data dictionary pair. Note this example uses fake data")))
# share(y,
#       publish_dv_1L_lgl = T)
