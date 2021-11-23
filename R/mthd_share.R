#' 
#' share
#' @name share-Ready4useRecord
#' @description share method applied to Ready4useRecord
#' @param x An object of class Ready4useRecord
#' @param gh_prerelease_1L_lgl Github prerelease (a logical vector of length one), Default: T
#' @param gh_repo_desc_1L_chr Github repository description (a character vector of length one), Default: 'Supplementary Files'
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param publish_dv_1L_lgl Publish dataverse (a logical vector of length one), Default: F
#' @param type_1L_chr Type (a character vector of length one), Default: 'prefer_dv'
#' @return x (An object of class Ready4useRecord)
#' @rdname share-methods
#' @aliases share,Ready4useRecord-method
#' @export 
#' @importFrom purrr discard
#' @importFrom ready4 write_env_objs_to_dv share
methods::setMethod("share", "Ready4useRecord", function (x, gh_prerelease_1L_lgl = T, gh_repo_desc_1L_chr = "Supplementary Files", 
    key_1L_chr = Sys.getenv("DATAVERSE_KEY"), publish_dv_1L_lgl = F, 
    type_1L_chr = "prefer_dv") 
{
    repos_chr <- c(character(0), ifelse(!is.na(x@a_Ready4usePointer@b_Ready4useRepos@gh_tag_1L_chr) & 
        (type_1L_chr %in% c("all", "prefer_gh")), "gh", NA_character_), 
        ifelse(!is.na(x@a_Ready4usePointer@b_Ready4useRepos@dv_ds_nm_1L_chr) & 
            (type_1L_chr %in% c("all", "prefer_dv")), "dv", NA_character_)) %>% 
        purrr::discard(is.na)
    if (!identical(repos_chr, character(0))) {
        if ("dv" %in% repos_chr) {
            server_1L_chr <- ifelse("server_1L_chr" %in% slotNames(x@a_Ready4usePointer@b_Ready4useRepos), 
                x@a_Ready4usePointer@b_Ready4useRepos@dv_server_1L_chr, 
                Sys.getenv("DATAVERSE_SERVER"))
            ds_ls <- ready4::write_env_objs_to_dv(env_objects_ls = x@b_Ready4useIngest@objects_ls, 
                descriptions_chr = x@b_Ready4useIngest@descriptions_chr, 
                ds_url_1L_chr = x@a_Ready4usePointer@b_Ready4useRepos@dv_ds_nm_1L_chr, 
                key_1L_chr = key_1L_chr, publish_dv_1L_lgl = publish_dv_1L_lgl, 
                server_1L_chr = server_1L_chr)
            x@dv_ds_metadata_ls <- list(ds_ls = ds_ls)
        }
        if ("gh" %in% repos_chr) {
            piggyback_tag_1L_chr <- ifelse(!is.na(x@a_Ready4usePointer@b_Ready4useRepos@gh_tag_1L_chr), 
                x@a_Ready4usePointer@b_Ready4useRepos@gh_tag_1L_chr, 
                "Documentation_0.0")
            ready4::write_env_objs_to_dv(env_objects_ls = x@b_Ready4useIngest@objects_ls, 
                descriptions_chr = NULL, ds_url_1L_chr = character(0), 
                piggyback_desc_1L_chr = gh_repo_desc_1L_chr, 
                piggyback_tag_1L_chr = piggyback_tag_1L_chr, 
                piggyback_to_1L_chr = x@a_Ready4usePointer@b_Ready4useRepos@gh_repo_1L_chr, 
                prerelease_1L_lgl = gh_prerelease_1L_lgl)
        }
    }
    return(x)
})
