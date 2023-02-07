# regular fns
manufacture_Ready4Module <- function(x,
                                     classes_lup = NULL,
                                     what_1L_chr = "slots_chr"){
  return_xx <- NULL
  if(what_1L_chr == slots_chr){
    if(is.null(classes_lup))
      classes_lup <- make_classes_lup(libraries_chr = NA_character_)
    modules_lup <- make_classes_lup(exclude_1L_chr = "S3", template_lup = classes_lup)
    submodules_lup <- make_classes_lup(exclude_1L_chr = "S4", template_lup = classes_lup)
    elements_lup <- make_classes_lup(exclude_1L_chr = "ready4", template_lup = classes_lup)
    slots_chr <- slotNames(x)
    slots_ls <- slots_chr %>%
      purrr::map(~procureSlot(x,
                              slot_nm_1L_chr = .x)) %>%
      stats::setNames(slots_chr)
    classes_ls <- slots_ls %>% purrr::map(~class(.x)) %>%
      stats::setNames(slots_chr)

    }
}
make_module_contents_ls <- function(x, s = NULL, classes_lup = NULL, what_1L_chr = "names") {
  if(is.null(classes_lup))
    classes_lup <- make_classes_lup(libraries_chr = NA_character_)
  modules_lup <- make_classes_lup(exclude_1L_chr = "S3", template_lup = classes_lup)
  submodules_lup <- make_classes_lup(exclude_1L_chr = "S4", template_lup = classes_lup)
  elements_lup <- make_classes_lup(exclude_1L_chr = "ready4", template_lup = classes_lup)

  if (!inherits(x,"Ready4Module")) {
    return(s)
  }
  slots_ls <- make_slots_ls(x)
  classes_ls <- slots_ls %>% purrr::map(~class(.x) %>% unlist()  %>% as.character()) %>%
    stats::setNames(names(slots_ls))
  summary_ls <- purrr::map2(slots_ls,
                            names(slots_ls),
                            ~ make_module_contents_ls(.x,
                                s = .y,
                                classes_lup = classes_lup))
  modules_lgl <- classes_ls %>% purrr::map_lgl(~!identical(intersect(.x,modules_lup$type_chr),
                                                           character(0)))
  submodules_lgl <- classes_ls %>% purrr::map_lgl(~!identical(intersect(.x,submodules_lup$type_chr),
                                                              character(0)))
  elements_lgl <- !(modules_lgl + submodules_lgl)
  summary_ls <- append(summary_ls[modules_lgl],
                       list(submodules_chr = summary_ls[submodules_lgl] %>% purrr::flatten_chr(),
                            elements_chr = summary_ls[elements_lgl] %>% purrr::flatten_chr()))
  slots_ls <- append(slots_ls[modules_lgl],
                     list(submodules_chr = summary_ls$submodules_chr,
                          elements_chr = summary_ls$elements_chr))
  summary_ls <- purrr::pmap(list(summary_ls,
                                 c(rep(T,length(summary_ls)-2),F,F),#modules_lgl,
                                 slots_ls %>% purrr::map_lgl(~{
                                   ifelse(inherits(.x,"Ready4Module"),
                                          any(make_slots_ls(.x) %>% purrr::map_lgl(~inherits(.x,"Ready4Module")))
                                          ,F)}),
                                   slots_ls),
                            ~if(..2){
                              if(!..3){
                                sub_classes_ls <- sub_slots_ls %>% purrr::map(~class(.x) %>% unlist()  %>% as.character()) %>%
                                  stats::setNames(names(sub_slots_ls))
                                submodules_chr <- sub_classes_ls %>% purrr::map(~intersect(.x,submodules_lup$type_chr)) %>% purrr::discard(~identical(.x,character(0))) %>% purrr::flatten_chr()
                                list(submodules_chr = submodules_chr,
                                     elements_chr = setdiff(purrr::flatten_chr(..1),submodules_chr))
                              }else{
                                make_module_contents_ls(..4,
                                  classes_lup = classes_lup)
                              }

                              }else{
                                ..1
                                })
  if(what_1L_chr == "names")
    return_ls <- summary_ls
  if(what_1L_chr == "contents")
    return_ls <- slots_ls
  return(return_ls)

}
make_slots_ls <- function(x){
  nms_chr <- slotNames(x)
  slots_ls <- nms_chr %>%
    purrr::map(~procureSlot(x,
                            slot_nm_1L_chr = .x)) %>%
    stats::setNames(nms_chr)
  return(slots_ls)
}

make_list_tree_nms <- function(list_ls, module_pfx_1L_chr = "",other_pfx_1L_chr = ""){
  if(is.list(list_ls)){
    names_ls <- purrr::map2(list_ls,
                            names(list_ls),
                            ~ c(paste0(ifelse(.y %in% c("submodules_chr","elements_chr"),
                                              other_pfx_1L_chr,
                                              module_pfx_1L_chr),
                                       ifelse(.y %in% c("submodules_chr","elements_chr"),
                                              stringr::str_sub(.y,end=-5),
                                              .y),
                                       sep="" ),
                                make_list_tree_nms(.x,
                                                   module_pfx_1L_chr = paste0(module_pfx_1L_chr,.y, "@"),
                                                   other_pfx_1L_chr = paste0(module_pfx_1L_chr,.y, " - "))))
    return(names_ls)
  }
}
# make_module_contents_ls(x, classes_lup = classes_lup) -> test_ls
# make_module_contents_ls(x, classes_lup = classes_lup, what_1L_chr = "contents") -> test_contents_ls
# make_list_tree_nms(test_ls)-> test_chr
make_classes_lup <- function(exclude_1L_chr = "",
                             gh_repo_1L_chr = "ready4-dev/ready4",
                             gh_tag_1L_chr = "Documentation_0.0",
                             libraries_chr = "",
                             template_lup = NULL){
  if(all(!is.na(libraries_chr)) & libraries_chr ==""){
    libraries_chr <- c(ready4::make_modules_pkgs_chr(),
                       "ready4show","ready4use") %>% sort()
  }
  if(!is.null(template_lup)){
    classes_lup  <- template_lup
  }else{
    dmt_urls_chr <- piggyback::pb_download_url(repo = gh_repo_1L_chr,
                                               tag = gh_tag_1L_chr, .token = "")
    classes_lup  <- readRDS(url(dmt_urls_chr[dmt_urls_chr %>%
                                               endsWith("prototype_lup.RDS")]))
    class(classes_lup) <- setdiff(class(classes_lup),"ready4class_pt_lup")
  }
  if(all(!is.na(libraries_chr))){
    if(exclude_1L_chr == "ready4"){
      classes_lup <- classes_lup %>%
        dplyr::filter(!pt_ns_chr %in% c(libraries_chr,
                                        "ready4",
                                        "ready4fun",
                                        "ready4class",
                                        "ready4pack"))
    }else{
      classes_lup <- classes_lup %>%
        dplyr::filter(pt_ns_chr %in% libraries_chr)
    }
  }
  if(exclude_1L_chr == "S3")
  classes_lup <- classes_lup %>%
    dplyr::filter(!old_class_lgl)
  if(exclude_1L_chr == "S4")
    classes_lup <- classes_lup %>%
    dplyr::filter(old_class_lgl)
  return(classes_lup)
}
# shiny module fns
import_csv_UI <- function(id_1L_chr, label_1L_chr = "CSV file") {
  ns_fn <- shiny::NS(id_1L_chr)
  shiny::tagList(
    shiny::fileInput(ns_fn("file"), label_1L_chr),
    shiny::checkboxInput(ns_fn("heading"), "Has heading"),
    shiny::selectInput(ns_fn("quote"),
                       "Quote",
                       c(
                         "None" = "",
                         "Double quote" = "\"",
                         "Single quote" = "'"
    ))
  )
}
import_csv_Server <- function(id_1L_chr, as_fctrs_1L_lgl) {
  shiny::moduleServer(
    id_1L_chr,
    function(input, output, session) {
      userFile <- shiny::reactive({
        shiny::validate(shiny::need(input$file,
                                    message = FALSE))
        input$file
      })
      dataframe <- shiny::reactive({
        utils::read.csv(userFile()$datapath,
                        header = input$heading,
                        quote = input$quote,
                        stringsAsFactors = as_fctrs_1L_lgl)
      })
      shiny::observe({
        msg_1L_chr <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg_1L_chr, "\n")
      })
      return(dataframe)
    }
  )
}
import_modules_UI <- function(id_1L_chr, #label_1L_chr = "CSV file"
                              modules_chr = character(0),
                              classes_lup = NULL){
  if(is.null(classes_lup)){
    classes_lup <- make_classes_lup(exclude_1L_chr = "S3")
  }
  if(!identical(modules_chr, character(0)))
    classes_lup <- classes_lup %>%
      dplyr::filter(type_chr %in% modules_chr)

  ns_fn <- shiny::NS(id_1L_chr)
  shiny::tagList(
    shiny::selectInput(ns_fn("module"),
                       "Module",
                       classes_lup$type_chr)
  )
}
import_modules_Server <- function(id_1L_chr,
                                  classes_lup){
  shiny::moduleServer(
    id_1L_chr,
    function(input, output, session) {
      module_fn <- shiny::reactive({
        shiny::validate(shiny::need(input$module,
                                    message = FALSE))
        input$module
      })
      module_nm_fn <- shiny::reactive({
        module_fn()
      })
      shiny::observe({
        msg_1L_chr <- sprintf("Module %s was selected", module_fn())
        cat(msg_1L_chr, "\n")
      })
      X_fn <- shiny::reactive({
        fn_txt_1L_chr <- ready4::get_from_lup_obj(classes_lup,
                                 match_value_xx = module_fn(),
                                 match_var_nm_1L_chr = "type_chr",
                                 target_var_nm_1L_chr = "val_chr")
        parse(text=fn_txt_1L_chr) %>% eval()
      })
      X_ls_fn <- shiny::reactive({
        # fn_txt_1L_chr <- ready4::get_from_lup_obj(classes_lup,
        #                                           match_value_xx = module_fn(),
        #                                           match_var_nm_1L_chr = "type_chr",
        #                                           target_var_nm_1L_chr = "val_chr")
        X_ls <- list()
        X_ls$contents_ls = make_module_contents_ls(X_fn(), classes_lup = classes_lup, what_1L_chr = "contents")
        X_ls$names_ls = make_module_contents_ls(X_fn(), classes_lup = classes_lup)
        X_ls$tree_names_ls = make_list_tree_nms(X_ls$names_ls)
        X_ls
      })


      module_ls <- list(module_nm_fn = module_nm_fn,
                        X_fn = X_fn,
                        X_ls_fn = X_ls_fn)
      return(module_ls)
    }
  )
}
plot_cars_UI <- function(id_1L_chr,
                         label_1L_chr = "Variable:",
                         choices_chr = c("Cylinders" = "cyl",
                                         "Transmission" = "am",
                                         "Gears" = "gear")){
  ns_fn <- shiny::NS(id_1L_chr)
  shiny::tagList(
    shiny::selectInput(ns_fn("variable"), label_1L_chr,choices_chr),
    shiny::checkboxInput(ns_fn("outliers"), "Show outliers", TRUE),
  )
}
plot_cars_Server <- function(id_1L_chr, mpgData,fml_1L_chr = "") {
  shiny::moduleServer(
    id_1L_chr,
    if(fml_1L_chr == ""){
      function(input, output, session) {
        variable_fn <- shiny::reactive({
          shiny::validate(shiny::need(input$variable, message = FALSE))
          input$variable
        })
        formula_fn <- shiny::reactive({
          paste("mpg ~", variable_fn())
        })
        return(formula_fn)
      }
    }else{
      function(input, output, session) {
        outliers_fn <- shiny::reactive({
          shiny::validate(shiny::need(input$outliers, message = FALSE))
          input$outliers
        })
        graphics::boxplot(as.formula(fml_1L_chr),
                           data = mpgData,
                           outline = outliers_fn(),
                           col = "#007bc2", pch = 19)
      }
    }
  )
}

