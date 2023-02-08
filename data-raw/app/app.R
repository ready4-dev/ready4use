library(shiny)
library(ready4)
source("functions.R")

mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))
classes_lup <-  make_classes_lup(exclude_1L_chr = "S3",
                                 libraries_chr = "ready4show")
# abbreviations_lup <- ready4fun::get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
#                                             piggyback_to_1L_chr = "ready4-dev/ready4")
# object_type_lup <- ready4fun::get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
#                                           piggyback_to_1L_chr = "ready4-dev/ready4")
ui <- fluidPage(
  headerPanel("ready4 User Interface"),
  sidebarLayout(
    sidebarPanel(
      import_modules_UI("modelmods",
                        #modules_chr = "Ready4showSynopsis",
                        classes_lup = classes_lup),
      #h3(textOutput("modulename")),
      #
      # p("The checkbox group controls the select input"),
      # checkboxGroupInput("inCheckboxGroup", "Input checkbox",
      #                    c("Item A", "Item B", "Item C")),
      # selectInput("inSelect", "Select input",
      #             c("Item A", "Item B", "Item C")),
      plot_cars_UI("mpgplot"),
      import_csv_UI("datafile", "User data (.csv format)")
    ),
    mainPanel(
      h3(textOutput("caption")),
      plotOutput("mpgPlot"),
      dataTableOutput("table")
    )
  )
)
server <- function(input, output, session) {
  modules_ls <- reactiveValues(X=NULL,
                               contents_ls = NULL,
                               names_ls = NULL,
                               tree_names_ls = NULL
                               )
  module_meta_ls <- import_modules_Server("modelmods",
                                          classes_lup = classes_lup)
  output$testX <- renderText({
    module_meta_ls$X_ls_fn()$tree_names_ls %>% length() %>% as.character()
  })
   # output$modulename <- renderText({
  #   module_meta_ls$module_nm_fn()
  # })
  observeEvent(input$modelmods, {
  modules_ls$X = module_meta_ls$X_fn()
  # modules_ls$contents_ls = make_module_contents_ls(x, classes_lup = classes_lup, what_1L_chr = "contents")
  # modules_ls$names_ls = make_module_contents_ls(modules_ls$X, classes_lup = classes_lup)
  # modules_ls$tree_names_ls = make_list_tree_nms(modules_ls$names_ls)
  })

  # observe({
  #   updateSelectInput(session, "inSelect",
  #                     label = paste("Select input label", length(x)),
  #                     choices = unlist(modules_ls$tree_names_ls)
  #                     # ,
  #                     # selected = tail(x, 1)
  #   )
  # })

  caption_fn <- plot_cars_Server("mpgplot", mpgData)
  output$caption <- renderText({
    caption_fn()
  })
  output$mpgPlot <- renderPlot({
    plot_cars_Server("mpgplot", mpgData, fml_1L_chr = caption_fn())
  })
  datafile <- import_csv_Server("datafile", as_fctrs_1L_lgl = FALSE)
  output$table <- renderDataTable({
    datafile()
  })
}
shinyApp(ui, server)
