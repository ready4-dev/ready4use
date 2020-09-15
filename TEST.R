add_ds_to_dv_repo <- function(dv_chr,
                              ds_meta_ls,
                              key_chr = Sys.getenv("DATAVERSE_KEY"),
                              server_chr = Sys.getenv("DATAVERSE_SERVER")){
  dv <- dataverse::get_dataverse(dv_chr)
  dv_ls <- dataverse::dataverse_contents(dv)
  per_chr_vec <- purrr::map_chr(dv_ls,
                                ~{
                                  per_chr <-.x %>%
                                    purrr::pluck("persistentUrl")
                                  ifelse(is.null(per_chr),
                                         NA_character_,
                                         per_chr)
                                } ) %>%
    purrr::discard(is.na) %>%
    unname()
  add_ds_lgl <- T
  update_ds_lgl <- F
  if(!identical(per_chr_vec,character(0))){
    db_nm_chr_vec <- purrr::map_chr(per_chr_vec,
                                    ~{
                                      ds_ls <- dataverse::get_dataset(.x)
                                      ds_ls$metadataBlocks$citation$fields$value[[1]]
                                    })
    add_ds_lgl <- !(ds_meta_ls$title %in% db_nm_chr_vec)
  }
  if(add_ds_lgl){
    dataverse:::initiate_sword_dataset(dv_chr,
                                       body = ds_meta_ls,
                                       key = key_chr,
                                       server = server_chr)
    dv_ls <- dataverse::dataverse_contents(dv)
  }else{
    ds_ls <- dataverse::get_dataset(per_chr_vec[ds_meta_ls$title == db_nm_chr_vec])
    update_ds_lgl <- purrr::map_lgl(names(ds_meta_ls),
                                    ~{
                                      type_name_chr <-  {
                                        tmp_chr <- switch(.x,
                                                          creator = "author",
                                                          description = "dsDescription",
                                                          subject = "keyword")
                                        ifelse(is.null(tmp_chr),
                                               ifelse(.x %in% ds_ls$metadataBlocks$citation$fields$typeName,
                                                      .x,
                                                      NA_character_),
                                               tmp_chr)
                                      }
                                      new_val_chr <- ds_meta_ls %>%
                                        purrr::pluck(.x)
                                      idx_dbl <- which(type_name_chr==ds_ls$metadataBlocks$citation$fields$typeName)
                                      purrr::map_lgl(1:length(ds_ls$metadataBlocks$citation$fields$value[idx_dbl]),
                                                     ~{
                                                       if(class(ds_ls$metadataBlocks$citation$fields$value[idx_dbl][[.x]]) == "character"){
                                                         (new_val_chr != ds_ls$metadataBlocks$citation$fields$value[idx_dbl])
                                                       }else{
                                                         if(class(ds_ls$metadataBlocks$citation$fields$value[idx_dbl][[.x]]) == "data.frame")
                                                           (new_val_chr !=  ds_ls$metadataBlocks$citation$fields$value[idx_dbl][[.x]][[1]]$value)
                                                       }
                                                     }) %>% any()

                                    }) %>% any()
    if(update_ds_lgl & F) # TEMPORARILY TURNED OFF AS UPDATE NOT WORKING
      dataverse::update_dataset(dataset = ds_ls,
                                body = ds_meta_ls,
                                key = key_chr,
                                server = server_chr
      )
    dv_ls <- dataverse::dataverse_contents(dv)
  }
  dv_ls[[1]]$persistentUrl
}
add_dv_meta_to_imp_lup <- function(x,
                                   db_ui_chr,
                                   file_type_chr,
                                   save_type_chr){
  ready4use::assert_single_row_tb(x)
  x %>%
    dplyr::mutate(data_repo_db_ui = db_ui_chr,
                  data_repo_file_ext = file_type_chr,
                  data_repo_save_type = save_type_chr)
}
add_files_to_dv <- function(files_tb, ## NEED TO ADD IGNORE/DELETE & REPLACE LOGIC IF FILES ALREADY IN ONLINE REPO
                            data_dir_rt_1L_chr = ".",
                            ds_url_chr,
                            key_chr,
                            server_chr){
  fl_ids_int <- purrr::pmap_int(files_tb,
                  ~ {
                    ds_ls <- dataverse::get_dataset(ds_url_chr)
                    is_draft_1L_lgl <- ds_ls$versionState == "DRAFT"
                    path_1L_chr <- paste0(data_dir_rt_1L_chr,"/",..1,"/",..2,..3)
                    file_nm_1L_chr <- paste0(..2,..3)
                    if(is_draft_1L_lgl){
                      if(file_nm_1L_chr %in% ds_ls$files$originalFileName){
                        ready4fun::get_from_lup_obj(ds_ls$files[,names(ds_ls$files) %>% unique()] %>% tibble::as_tibble(),
                                                    match_var_nm_1L_chr = "originalFileName",
                                                    match_value_xx = file_nm_1L_chr,
                                                    target_var_nm_1L_chr = "id",
                                                    evaluate_lgl = F) %>%
                          dataverse::delete_file()
                      }
                      dataverse::add_dataset_file(file = path_1L_chr,
                                                  dataset = ds_url_chr,
                                                  description = ..4,
                                                  key = key_chr,
                                                  server = server_chr)
                    }
                  }
  )
  return(fl_ids_int)
}
copy_files_to_dv_dir <- function(files_tb,
                                 local_dv_dir_1L_chr){
  purrr::pwalk(files_tb,
               ~ file.copy(paste0(..1,"/",..2,..3),
                           local_dv_dir_1L_chr))
}
make_files_tb <- function(paths_to_dirs_chr,
                          recode_ls,
                          inc_fl_types_chr = NA_character_){
  files_tb <- purrr::map_dfr(paths_to_dirs_chr,
                             ~{
                               files_chr_vec <- list.files(.x)
                               if(!identical(files_chr_vec,character(0))){
                                 tb <- tibble::tibble(dir_chr = rep(.x,length(files_chr_vec)),
                                                      file_chr = files_chr_vec %>%
                                                        purrr::map_chr(~stringr::str_sub(.x,
                                                                                         end = as.vector(stringi::stri_locate_last_regex(.x, "\\.")[,1])-1)),
                                                      file_type_chr = files_chr_vec %>%
                                                        purrr::map_chr(~stringr::str_sub(.x,
                                                                                         start = as.vector(stringi::stri_locate_last_regex(.x, "\\.")[,1]))))

                                 tb
                               }
                             })
  if(!is.na(inc_fl_types_chr))
    files_tb <- files_tb %>%
      dplyr::filter(file_type_chr %in% inc_fl_types_chr)
  description_chr <- purrr::map_chr(files_tb$file_chr,
                                    ~ {
                                      arg_ls <- append(list(EXPR=.x),recode_ls)
                                      rlang::exec(.fn = switch, !!!arg_ls)
                                    })
  files_tb <- files_tb %>%
    dplyr::mutate(description_chr = description_chr,
                  ds_file_ext_chr = purrr::map_chr(file_type_chr,
                                               ~ ifelse(.x %in% c(".csv", ".xls",".xlsx"),
                                                        ".tab",
                                                        ".zip")))
  assertthat::are_equal(nrow(files_tb),
                        paste0(files_tb$file_chr,
                               files_tb$file_type_chr) %>%
                          unique() %>%
                          length())
  return(files_tb)
}
write_dv_ds_fls <- function(files_tb,
                            fl_ids_int,
                            local_dv_dir_1L_chr){
  purrr::walk(1:length(fl_ids_int),
              ~{
                if(!(ds_ls$versionState=="DRAFT" & files_tb$file_type_chr[.x]==".zip")){
                  write_dv_file_fl(database_ui_chr = ds_url_chr,
                                   filename_chr = files_tb$file_chr[.x],
                                   repo_file_format = files_tb$ds_file_ext_chr[.x],
                                   dataverse_chr = Sys.getenv("DATAVERSE_SERVER"),
                                   save_type_chr = "original",
                                   destination_path_chr = get_local_path_to_dv_data(save_dir_path_chr = local_dv_dir_1L_chr,
                                                                                    filename_chr = files_tb$file_chr[.x],
                                                                                    save_format_chr = files_tb$file_type_chr[.x]))
                }
              })

}
write_dv_ds <- function(ds_meta_ls,
                        dev_pkg_nm_1L_chr = ready4fun::get_dev_pkg_nm(),
                        dss_tb,
                            dv_nm_1L_chr,
                            parent_dv_dir_1L_chr,
                            paths_to_dirs_chr,
                            inc_fl_types_chr = NA_character_,
                            key_chr = Sys.getenv("DATAVERSE_KEY"),
                            server_chr = Sys.getenv("DATAVERSE_SERVER")){
  ds_url_chr <- add_ds_to_dv_repo(dv_chr = dv_nm_1L_chr,
                                  ds_meta_ls = ds_meta_ls)
  ds_ls <- dataverse::get_dataset(ds_url_chr)
  dv_dir_1L_chr <- paste0(parent_dv_dir_1L_chr,"/",dv_nm_1L_chr)
  if(!dir.exists(dv_dir_1L_chr)){
    dir.create(dv_dir_1L_chr)
  }
  local_dv_dir_1L_chr <- paste0(dv_dir_1L_chr,"/",ds_meta_ls$title)
  if(!dir.exists(local_dv_dir_1L_chr)){
    dir.create(local_dv_dir_1L_chr)
  }
  ds_chr <- dss_tb$ds_obj_nm_chr
  files_tb <- make_files_tb(paths_to_dirs_chr = paths_to_dirs_chr,
                            recode_ls = dss_tb$title_chr %>% as.list() %>% stats::setNames(ds_chr),
                            inc_fl_types_chr = inc_fl_types_chr)
  fl_ids_int <- add_files_to_dv(files_tb,
                                ds_url_chr = ds_url_chr,
                                key_chr = key_chr,
                                server_chr = server_chr)
  write_dv_ds_fls(files_tb,
                  fl_ids_int = fl_ids_int,
                  local_dv_dir_1L_chr = local_dv_dir_1L_chr)
  ds_ls <- dataverse::get_dataset(ds_url_chr)
  return(ds_ls)
}
write_pkg_dss_to_dv_ds_csvs <- function(pkg_dss_tb,
                                        dv_nm_1L_chr,
                                        dev_pkg_nm_1L_chr = ready4fun::get_dev_pkg_nm(),
                                        parent_dv_dir_1L_chr = "../../../../Data/Dataverse",
                                        key_chr = Sys.getenv("DATAVERSE_KEY"),
                                        server_chr = Sys.getenv("DATAVERSE_SERVER"),
                                        subject_1L_chr = "Mental health, health economics, data synthesis, simulation."){
  ds_meta_ls <- list(title = paste0(dev_pkg_nm_1L_chr," Code Library Metadata"),
                     creator = suppressWarnings(parse(text=(packageDescription(dev_pkg_nm_1L_chr,
                                                                               fields = "Authors@R"))) %>%
                                                  eval() %>%
                                                  purrr::keep(~stringr::str_detect(.x,"aut, cre")) %>%
                                                  stringr::str_sub(end = stringi::stri_locate_first_fixed(.,"<")[1]-2)),
                     description = paste0("Metadata relating to the abbreviations, classes, datasets, functions, generics and methods used in the ",dev_pkg_nm_1L_chr," code library."),
                     subject = subject_1L_chr)
  ds_chr <- pkg_dss_tb$ds_obj_nm_chr
  purrr::walk(ds_chr,~ {
    data(list=.x, package = dev_pkg_nm_1L_chr, envir = environment())
    eval(parse(text = .x)) %>%
      dplyr::mutate_if(is.list,
                       list(~ifelse(stringr::str_c(.)=="NULL",NA_character_ , stringr::str_c (.)))) %>%
      write.csv(file = paste0("data-raw/",.x,".csv"),
                row.names = F)
  })
  ds_ls <- write_dv_ds(ds_meta_ls = ds_meta_ls,
                       dev_pkg_nm_1L_chr = dev_pkg_nm_1L_chr,
                       dv_nm_1L_chr = dv_nm_1L_chr,
                       parent_dv_dir_1L_chr = parent_dv_dir_1L_chr,
                       inc_fl_types_chr = ".csv",
                       paths_to_dirs_chr = c("data-raw"),
                       key_chr = key_chr,
                       server_chr = server_chr,
                       dss_tb = pkg_dss_tb)
  return(ds_ls)
}
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
