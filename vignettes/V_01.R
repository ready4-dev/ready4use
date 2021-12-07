## ----echo = F-----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----results='hide', message=FALSE--------------------------------------------
library(ready4use)

## -----------------------------------------------------------------------------
x <- Ready4useRepos(dv_nm_1L_chr = "fakes",
                    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/HJXYKQ",
                    dv_server_1L_chr = "dataverse.harvard.edu",
                    gh_repo_1L_chr = "ready4-dev/ready4",
                    gh_tag_1L_chr = "Documentation_0.0")

## -----------------------------------------------------------------------------
## Not run
# x <- ingest(x,
#             fls_to_ingest_chr = c("ymh_clinical_tb","ymh_clinical_dict_r3"))

## -----------------------------------------------------------------------------
objects_ls <- ingest(x,
                     fls_to_ingest_chr = c("ymh_clinical_tb","ymh_clinical_dict_r3"),
                     metadata_1L_lgl = F)

## -----------------------------------------------------------------------------
## Not run
# ymh_clinical_tb <- ingest(x,
#                           fls_to_ingest_chr = c("ymh_clinical_tb"),
#                           metadata_1L_lgl = F)

