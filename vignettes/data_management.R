## ----echo = F-----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----results='hide', message=FALSE--------------------------------------------
library(ready4use)

## -----------------------------------------------------------------------------
x <- Ready4useRepos(dv_nm_1L_chr = "fakes",
                    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/HJXYKQ",
                    dv_server_1L_chr = "dataverse.harvard.edu")

## -----------------------------------------------------------------------------
x <- ingest(x)
class(x)

## -----------------------------------------------------------------------------
y <- Ready4useDyad(ds_tb = x@b_Ready4useIngest@objects_ls$ymh_clinical_tb,
                   dictionary_r3 = x@b_Ready4useIngest@objects_ls$ymh_clinical_dict_r3)

## -----------------------------------------------------------------------------
 exhibit(y,
         display_1L_chr = "head")

