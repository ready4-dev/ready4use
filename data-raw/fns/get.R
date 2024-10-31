get_colour_codes <- function(colour_1L_int = 1,
                             manual_chr = c("#de2d26","#fc9272"),
                             pick_1L_int = integer(0),
                             single_1L_lgl = FALSE,
                             style_1L_chr = get_styles(),
                             type_1L_chr = c("ggsci", "manual", "viridis")){
  style_1L_chr <- match.arg(style_1L_chr)
  type_1L_chr <- match.arg(type_1L_chr)
  if(identical(pick_1L_int, integer(0))){
    pick_1L_int <- colour_1L_int
  }
  if(type_1L_chr == "manual"){
    colour_codes_chr <- ggpubr::get_palette(manual_chr, k=colour_1L_int)
  }else{
    defaults_chr <- get_styles(type_1L_chr)
    if(!style_1L_chr %in% defaults_chr){
      style_1L_chr <- defaults_chr[1]
    }
  }
  if(type_1L_chr == "ggsci"){
    colour_codes_chr <- ggpubr::get_palette(style_1L_chr, k=colour_1L_int)
  }
  if(type_1L_chr == "viridis")
    colour_codes_chr <- viridis::viridis(colour_1L_int, option = style_1L_chr)
  if(single_1L_lgl){
    colour_codes_chr <- colour_codes_chr[pick_1L_int]
  }else{
    colour_codes_chr <- colour_codes_chr[1:pick_1L_int]
  }
  colour_codes_chr <- colour_codes_chr %>% purrr::discard(is.na)
  return(colour_codes_chr)
}
get_drop_offs <- function(X_Ready4useDyad = Ready4useDyad(),
                          condition_1L_chr = ">1",
                          uid_var_nm_1L_chr = "uid_chr"){
  all_ids_chr <- X_Ready4useDyad@ds_tb %>% dplyr::pull(!!rlang::sym(uid_var_nm_1L_chr))
  unique_ids_chr <- unique(all_ids_chr)
  counts_int <- unique_ids_chr %>% purrr::map_int(~sum(all_ids_chr == .x))
  pass_test_chr <- unique_ids_chr[purrr::map_lgl(counts_int, ~ eval(parse(text=paste0(.x, condition_1L_chr))))]
  drop_offs_chr <- setdiff(all_ids_chr %>% unique(), pass_test_chr)
  return(drop_offs_chr)
}
get_file_from_dv <- function(ds_ui_1L_chr,
                             fl_nm_1L_chr,
                             save_fmt_1L_chr,
                             repo_fl_fmt_1L_chr,
                             consent_1L_chr = "",
                             consent_indcs_int = 1L,
                             key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                             options_chr = c("Y", "N"),
                             server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), # was dataverse_chr
                             save_type_1L_chr = "original",
                             save_dir_path_1L_chr = "",
                             read_fn,
                             unlink_1L_lgl = T){
  destination_path_chr <- ifelse(unlink_1L_lgl,
                                 tempfile(),
                                 ready4::make_local_path_to_dv_data(save_dir_path_1L_chr = save_dir_path_1L_chr,
                                                                    fl_nm_1L_chr = fl_nm_1L_chr,
                                                                    save_fmt_1L_chr = save_fmt_1L_chr))
  ready4::write_dv_fl_to_loc(consent_1L_chr = ifelse(unlink_1L_lgl,options_chr[consent_indcs_int][1],consent_1L_chr),
                             consent_indcs_int = consent_indcs_int,
                             ds_ui_1L_chr = ds_ui_1L_chr,
                             fl_nm_1L_chr = fl_nm_1L_chr,
                             repo_fl_fmt_1L_chr = repo_fl_fmt_1L_chr,
                             key_1L_chr = key_1L_chr,
                             options_chr = options_chr,
                             server_1L_chr = server_1L_chr,
                             save_type_1L_chr = save_type_1L_chr,
                             dest_path_1L_chr = destination_path_chr)
  file_xxx <- rlang::exec(read_fn,destination_path_chr,stringsAsFactors = F)
  if(unlink_1L_lgl)
    unlink(destination_path_chr)
  file_xxx
  return(file_xxx)
}
get_fl_nms_of_types <- function(fl_nms_chr,
                                types_chr){
  subset_of_fl_nms_chr <- purrr::keep(fl_nms_chr,
                                      ~{
                                        fl_nm_1L_chr <- .x
                                        types_chr %>%
                                          purrr::map_lgl(~endsWith(fl_nm_1L_chr,
                                                                   .x)) %>%
                                          any()
                                      })
  return(subset_of_fl_nms_chr)
}
get_fl_meta_from_dv_ls <- function (ds_ls,
                                    fl_nm_1L_chr,
                                    nms_chr = NA_character_,
                                    type_1L_chr = "description")
{
  if (is.na(nms_chr[1])) {
    nms_chr <- purrr::map_chr(ds_ls$files$filename, ~ifelse(is.na(.x), .y, .x))
  }
  if (fl_nm_1L_chr %in% nms_chr) {
    metadata_xx <- get_from_lup_obj(ds_ls$files[, names(ds_ls$files) %>%
                                                  unique()] %>% tibble::as_tibble(),
                                    match_var_nm_1L_chr = "filename",
                                    match_value_xx = fl_nm_1L_chr,
                                    target_var_nm_1L_chr = type_1L_chr, evaluate_1L_lgl = F)
  }
  else {
    metadata_xx <- NA_character_
  }
  return(metadata_xx)
}
get_journal_palette_fn <- function (type_1L_chr = c("colour", "fill"), what_1L_chr = "lancet")
{
  type_1L_chr <- match.arg(type_1L_chr)
  options_ls <- list(scale_colour_aaas = ggsci::scale_colour_aaas,
                     scale_colour_bmj = ggsci::scale_colour_bmj, scale_colour_bs5 = ggsci::scale_colour_bs5,
                     scale_colour_cosmic = ggsci::scale_colour_cosmic, scale_colour_d3 = ggsci::scale_colour_d3,
                     scale_colour_flatui = ggsci::scale_colour_flatui, scale_colour_frontiers = ggsci::scale_colour_frontiers,
                     scale_colour_futurama = ggsci::scale_colour_futurama,
                     scale_colour_gsea = ggsci::scale_colour_gsea, scale_colour_igv = ggsci::scale_colour_igv,
                     scale_colour_jama = ggsci::scale_colour_jama, scale_colour_jco = ggsci::scale_colour_jco,
                     scale_colour_lancet = ggsci::scale_colour_lancet, scale_colour_locuszoom = ggsci::scale_colour_locuszoom,
                     scale_colour_material = ggsci::scale_colour_material,
                     scale_colour_nejm = ggsci::scale_colour_nejm, scale_colour_npg = ggsci::scale_colour_npg,
                     scale_colour_observable = ggsci::scale_colour_observable,
                     scale_colour_rickandmorty = ggsci::scale_colour_rickandmorty,
                     scale_colour_simpsons = ggsci::scale_colour_simpsons,
                     scale_colour_startrek = ggsci::scale_colour_startrek,
                     scale_colour_tron = ggsci::scale_colour_tron, scale_colour_tw3 = ggsci::scale_colour_tw3,
                     scale_colour_uchicago = ggsci::scale_colour_uchicago,
                     scale_colour_ucscgb = ggsci::scale_colour_ucscgb, scale_fill_aaas = ggsci::scale_fill_aaas,
                     scale_fill_bmj = ggsci::scale_fill_bmj, scale_fill_bs5 = ggsci::scale_fill_bs5,
                     scale_fill_cosmic = ggsci::scale_fill_cosmic, scale_fill_d3 = ggsci::scale_fill_d3,
                     scale_fill_flatui = ggsci::scale_fill_flatui, scale_fill_frontiers = ggsci::scale_fill_frontiers,
                     scale_fill_futurama = ggsci::scale_fill_futurama, scale_fill_gsea = ggsci::scale_fill_gsea,
                     scale_fill_igv = ggsci::scale_fill_igv, scale_fill_jama = ggsci::scale_fill_jama,
                     scale_fill_jco = ggsci::scale_fill_jco, scale_fill_lancet = ggsci::scale_fill_lancet,
                     scale_fill_locuszoom = ggsci::scale_fill_locuszoom, scale_fill_material = ggsci::scale_fill_material,
                     scale_fill_nejm = ggsci::scale_fill_nejm, scale_fill_npg = ggsci::scale_fill_npg,
                     scale_fill_observable = ggsci::scale_fill_observable,
                     scale_fill_rickandmorty = ggsci::scale_fill_rickandmorty,
                     scale_fill_simpsons = ggsci::scale_fill_simpsons, scale_fill_startrek = ggsci::scale_fill_startrek,
                     scale_fill_tron = ggsci::scale_fill_tron, scale_fill_tw3 = ggsci::scale_fill_tw3,
                     scale_fill_uchicago = ggsci::scale_fill_uchicago, scale_fill_ucscgb = ggsci::scale_fill_ucscgb)
  journal_palette_fn <- options_ls %>% purrr::pluck(paste0("scale_",
                                                           type_1L_chr, "_", what_1L_chr))
  return(journal_palette_fn)
}
get_journal_plot_fn <- function (what_1L_chr = "barplot",
                                 pkg_1L_chr = "ggpubr",
                                 prefix_1L_chr = "gg") {
  options_ls <- list(ggbarplot = ggpubr::ggbarplot,
                     ggballoonplot = ggpubr::ggballoonplot,
                     ggboxplot = ggpubr::ggboxplot,
                     ggdensity = ggpubr::ggdensity,
                     ggdonutchart = ggpubr::ggdonutchart,
                     ggdotchart = ggpubr::ggdotchart,
                     ggdotplot = ggpubr::ggdotplot,
                     ggecdf = ggpubr::ggecdf,
                     ggerrorplot = ggpubr::ggerrorplot,
                     gghistogram = ggpubr::gghistogram,
                     ggline = ggpubr::ggline,
                     # ggmaplot = ggpubr::ggmaplot,
                     ggpaired = ggpubr::ggpaired,
                     ggpie = ggpubr::ggpie,
                     ggqqplot = ggpubr::ggqqplot,
                     ggscatter = ggpubr::ggscatter,
                     ggscatterhist = ggpubr::ggscatterhist,
                     ggstripchart = ggpubr::ggstripchart,
                     ggviolin = ggpubr::ggviolin)
  if(what_1L_chr == "names"){
    journal_plot_xx <- names(options_ls) %>% stringr::str_sub(start = nchar(prefix_1L_chr)+1)
  }else{
    journal_plot_xx <- options_ls %>% purrr::pluck(paste0(prefix_1L_chr, what_1L_chr))
  }
  return(journal_plot_xx)
}
get_local_path_to_dv_data <- function(save_dir_path_1L_chr,
                                      fl_nm_1L_chr,
                                      save_fmt_1L_chr){
  lifecycle::deprecate_soft("0.0.0.9149", "get_local_path_to_dv_data()", "ready4::make_local_path_to_dv_data()")
  path_chr <- paste0(ifelse(save_dir_path_1L_chr!="",paste0(save_dir_path_1L_chr,"/"),""),
         fl_nm_1L_chr,
         save_fmt_1L_chr)
  return(path_chr)
}
get_r3_from_dv_csv <- function(file_name_chr,
                               data_repo_db_ui_chr,
                               data_repo_ui_chr = NA_character_,
                               r3_fn = ready4use_imports){
  tb_r3 <- tibble::tibble(file_type_chr = ".csv",
                 file_name_chr = file_name_chr,
                 data_repo_chr = NA_character_,
                 data_repo_ui_chr = data_repo_ui_chr,
                 data_repo_db_ui_chr =  data_repo_db_ui_chr,
                 data_repo_file_ext_chr = ".tab",
                 data_repo_save_type_chr = "original") %>%
    ready4use_dataverses() %>%
    procure() %>%
    make_r3_from_csv_tb(r3_fn)
  return(tb_r3)
}
get_raw_dss <- function(paths_chr,
                        names_chr,
                        read_fn = read.csv){
  assertthat::assert_that(assertthat::are_equal(length(paths_chr), length(names_chr)), msg = "names_chr must be same length as paths_chr")
  ds_ls <- purrr::map(paths_chr,
                      ~ read_fn(.x)#read.csv(eval(parse(text = .x)))
  ) %>% stats::setNames(names_chr)
  return(ds_ls)
}
get_reference_descs <- function(correspondences_ls,
                                correspondences_r3 = ready4show::ready4show_correspondences()){
  reference_descs_chr <- purrr::reduce(correspondences_ls,
                                       .init = correspondences_r3,
                                       ~ rbind(.x,.y)) %>% dplyr::pull(new_nms_chr) %>% unique()
  return(reference_descs_chr)
}
get_styles <- function(what_1L_chr = c("all", "ggsci",  "viridis"),
                       sort_1L_lgl = FALSE){
  what_1L_chr <- match.arg(what_1L_chr)
  styles_chr <- character(0)
  if(what_1L_chr %in% c("all", "ggsci")){
    styles_chr <- c(styles_chr, c("npg", "aaas", "lancet", "jco", "nejm", "ucscgb", "uchicago", "d3", "futurama", "igv", "locuszoom", "rickandmorty", "startrek", "simpsons", "tron"))
  }
  if(what_1L_chr %in% c("all", "viridis")){
    styles_chr <- c(styles_chr, c("magma","A","inferno", "B", "plasma", "C", "viridis", "D",  "cividis", "E", "rocket", "F", "mako", "G", "turbo",  "H"))
  }
  if(sort_1L_lgl){
    styles_chr <- sort(styles_chr)
  }
  return(styles_chr)
}
get_valid_path_chr <- function(x){
  assert_file_exists(x)
  valid_path_chr <- x
  return(valid_path_chr)
}
get_vars_with_cdn <- function(data_tb,
                               cdn_fn){
  vars_chr <- names(data_tb)[names(data_tb) %>% purrr::map_lgl(~cdn_fn(data_tb %>% dplyr::pull(!!rlang::sym(.x))))]
  return(vars_chr)
}
