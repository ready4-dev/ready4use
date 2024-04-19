make_correspondences <- function(dyad_ls, #manufacture method
                                 append_1L_lgl = T,
                                 correspondences_ls = NULL,
                                 names_1L_lgl = F,
                                 reference_1L_int = 2L){
  items_int <- length(dyad_ls)
  if(items_int > 1){
    if(is.null(correspondences_ls)){
      correspondences_ls <- setdiff(1:items_int, reference_1L_int) %>%
        purrr::map(~{
          dyad_ls[[.x]]@dictionary_r3$var_nm_chr %>%
            purrr::map2_dfr(dyad_ls[[.x]]@dictionary_r3$var_desc_chr,
                            ~ {
                              x <- ready4show_correspondences()
                              test_1L_chr <- ready4::get_from_lup_obj(dyad_ls[[reference_1L_int]]@dictionary_r3,
                                                                      match_value_xx = .x,
                                                                      match_var_nm_1L_chr = "var_nm_chr",
                                                                      target_var_nm_1L_chr = "var_desc_chr")
                              if(!identical(.y, test_1L_chr)){
                                x<- ready4show::renew.ready4show_correspondences(x,
                                                                                 new_nms_chr = test_1L_chr,
                                                                                 old_nms_chr = .y)

                              }
                              x
                            })
        }) %>% stats::setNames(names(dyad_ls)[setdiff(1:items_int, reference_1L_int)])
      if(names_1L_lgl){
        correspondences_ls <- make_correspondences(dyad_ls, append_1L_lgl = append_1L_lgl, correspondences_ls = correspondences_ls, reference_1L_int = reference_1L_int)
      }
    }else{
      correspondences_ls <- update_correspondences(correspondences_ls, dyad_ls = dyad_ls, reference_1L_int = reference_1L_int)
    }
    if(append_1L_lgl && length(correspondences_ls) < length(dyad_ls))
      correspondences_ls <- correspondences_ls %>% append(list(ready4show::ready4show_correspondences()) %>% stats::setNames(names(dyad_ls)[reference_1L_int]), after = reference_1L_int -1)
  }else{
    correspondences_ls <- NULL
  }
  return(correspondences_ls)
}
make_keep_lgl <- function(ds_tb,
                          filter_fn = is.na,
                          summary_fn = any,
                          var_nms_chr){
  keep_lgl <- var_nms_chr %>% purrr::map_dfc(~tibble::tibble(!!rlang::sym(.x) := ds_tb %>% dplyr::pull(.x) %>% filter_fn()))  %>%
    t() %>% tibble::as_tibble() %>% dplyr::summarise_all(summary_fn) %>% as.vector() %>%
    purrr::flatten_lgl()
  return(keep_lgl)
}
make_period_correspondences <- function(descriptions_chr,
                                        integers_1L_lgl = TRUE,
                                        name_1L_chr = "period",
                                        plural_chr = "s",
                                        range_int = 1L:12L,
                                        reference_1L_int = integer(0),
                                        #replace_blanks_1L_lgl = FALSE,
                                        spaced_1L_lgl = TRUE,
                                        type_1L_chr = "descriptions",
                                        units_chr = c("minute","hour","week","month","year")){
  patterns_ls <- get_patterns(descriptions_chr, flatten_1L_lgl = F,
                              integers_1L_lgl = integers_1L_lgl,
                              plural_chr = plural_chr,
                              range_int = range_int,
                              reference_1L_int = reference_1L_int,
                              replace_blanks_1L_lgl = T,
                              spaced_1L_lgl = spaced_1L_lgl, units_chr = units_chr)
  new_xx <- descriptions_chr %>% purrr::map2(patterns_ls,
                                             ~{
                                               description_1L_chr <- .x
                                               patterns_chr <- .y
                                               concepts_chr <- patterns_chr %>% strsplit(" ") %>%
                                                 purrr::map_chr(~{
                                                   parts_chr <- if(suppressWarnings(!is.na(as.numeric(.x[1]))))
                                                   {
                                                     c(english::words(as.numeric(.x[1])),.x[-1])
                                                   }else{
                                                     .x}
                                                   paste0(parts_chr, collapse = " ")
                                                 }
                                                 )
                                               unique_chr <- unique(concepts_chr)
                                               if(length(unique_chr)>1){
                                                 options_chr <- paste0(name_1L_chr,"_",1:length(unique_chr))
                                                 replacements_chr <- patterns_chr %>% strsplit(" ") %>% purrr::map_chr(~{
                                                   parts_chr <- if(suppressWarnings(!is.na(as.numeric(.x[1]))))
                                                   {
                                                     c(english::words(as.numeric(.x[1])),.x[-1])
                                                   }else{
                                                     .x}
                                                   options_chr[which(paste0(parts_chr, collapse = " ")== unique_chr)]
                                                 })
                                               }else{
                                                 replacements_chr <- rep(name_1L_chr, length(patterns_chr))
                                               }
                                               if(type_1L_chr == "periods"){
                                                 ready4show::ready4show_correspondences() %>%
                                                   ready4show::renew.ready4show_correspondences(old_nms_chr = unique_chr,
                                                                                                new_nms_chr = replacements_chr)
                                               }else{
                                                 purrr::reduce(1:length(patterns_chr),.init = description_1L_chr, ~ stringr::str_replace_all(.x,
                                                                                                                                             patterns_chr[.y],
                                                                                                                                             replacements_chr[.y]))
                                               }
                                             })
  if(type_1L_chr == "descriptions"){
    correspondences_xx <- ready4show::ready4show_correspondences() %>%
      ready4show::renew.ready4show_correspondences(old_nms_chr = descriptions_chr,
                                                   new_nms_chr = new_xx %>% purrr::flatten_chr()) %>%
      dplyr::filter(!is.na(new_nms_chr))
  }
  if(type_1L_chr == "periods"){
    correspondences_xx <- new_xx
  }
  return(correspondences_xx)
}
make_temporal_lup <- function(dyad_ls,
                              recode_ls,
                              spaced_1L_lgl = TRUE){
  temporal_lup <-  purrr::map_dfr(1:length(dyad_ls),
                                  ~
                                    {

                                      if(!identical(.x, ready4show_correspondences())){
                                        Y <- dyad_ls[[.x]]
                                        x <- recode_ls[[.x]]
                                        grouping_1L_chr <- names(dyad_ls)[.x]
                                        new_lup <- x$old_nms_chr %>% make_period_correspondences(spaced_1L_lgl = spaced_1L_lgl, type_1L_chr = "periods") %>%
                                          purrr::map2_dfr(x$old_nms_chr,
                                                          ~{
                                                            lup_tb <- tibble::tibble(grouping_chr = grouping_1L_chr,
                                                                                     variable_chr = ready4::get_from_lup_obj(Y@dictionary_r3, match_var_nm_1L_chr = "var_desc_chr", match_value_xx = .y, target_var_nm_1L_chr = "var_nm_chr"),
                                                                                     periods_chr = .x$old_nms_chr,
                                                                                     standardised_chr = .x$new_nms_chr)
                                                            lup_tb <- lup_tb %>% dplyr::mutate(value_int = periods_chr %>% stringr::word(),
                                                                                               unit_chr = periods_chr %>% stringr::word(start = 2))
                                                            lup_tb
                                                          })
                                      }else{
                                        NULL
                                      }})
  return(temporal_lup)
}
make_r3_from_csv_tb <- function(csv_tb,
                                r3_fn){ ## NEED TO EDIT inc_fls_to_rename_ls list col logic
  list_cols <- rlang::exec(r3_fn) %>%
    dplyr::select_if(is.list) %>% names()
  char_cols <- rlang::exec(r3_fn) %>%
    dplyr::select_if(is.character) %>% names()
  tb <- csv_tb %>%
    tibble::as_tibble() %>%
    dplyr::mutate_at(.vars = list_cols, ~ purrr::map(.,~.x)) %>%
    dplyr::mutate_at(.vars = list_cols, .funs = transform_csv_col_to_ls_col) %>%
    dplyr::mutate_at(.vars = list_cols, .funs = ~ purrr::map(.,~if(all(is.na(.x))){NULL}else{.x})) %>%
    dplyr::mutate_at(.vars = char_cols, .funs = as.character) %>%
    dplyr::select(names(rlang::exec(r3_fn)))
  tb_r3 <- rlang::exec(r3_fn, tb)
  return(tb_r3)
}
# make_files_tb <- function(paths_to_dirs_chr,
#                           recode_ls,
#                           inc_fl_types_chr = NA_character_){
#   lifecycle::deprecate_soft("0.0.0.9149", "make_files_tb()", "ready4::make_files_tb()")
#   files_tb <- purrr::map_dfr(paths_to_dirs_chr,
#                              ~{
#                                files_chr_vec <- list.files(.x)
#                                if(!identical(files_chr_vec,character(0))){
#                                  tb <- tibble::tibble(dir_chr = rep(.x,length(files_chr_vec)),
#                                                       file_chr = files_chr_vec %>%
#                                                         purrr::map_chr(~stringr::str_sub(.x,
#                                                                                          end = as.vector(stringi::stri_locate_last_regex(.x, "\\.")[,1])-1)),
#                                                       file_type_chr = files_chr_vec %>%
#                                                         purrr::map_chr(~stringr::str_sub(.x,
#                                                                                          start = as.vector(stringi::stri_locate_last_regex(.x, "\\.")[,1]))))
#
#                                  tb
#                                }
#                              })
#   if(!is.na(inc_fl_types_chr))
#     files_tb <- files_tb %>%
#       dplyr::filter(file_type_chr %in% inc_fl_types_chr)
#   files_tb <- files_tb %>%
#     dplyr::filter(file_chr %in% names(recode_ls))
#   description_chr <- purrr::map_chr(files_tb$file_chr,
#                                     ~ {
#                                       arg_ls <- append(list(EXPR=.x),recode_ls)
#                                       rlang::exec(.fn = switch, !!!arg_ls)
#                                     })
#   files_tb <- files_tb %>%
#     dplyr::mutate(description_chr = description_chr,
#                   ds_file_ext_chr = purrr::map_chr(file_type_chr,
#                                                    ~ ifelse(.x %in% c(".csv", ".xls",".xlsx"),
#                                                             ".tab",
#                                                             ".zip")))
#   assertthat::are_equal(nrow(files_tb),
#                         paste0(files_tb$file_chr,
#                                files_tb$file_type_chr) %>%
#                           unique() %>%
#                           length())
#   return(files_tb)
# }
