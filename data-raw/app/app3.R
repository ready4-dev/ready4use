library(shiny)
library(stringi)
stack.shiny <- function(){
  dat_set <- mtcars

  all_cars <- row.names(dat_set)

  car_brands <- unique(stri_extract_first_words(all_cars))



  server <- function(input, output, session) {

    output$car_brands <- renderUI({
      selectInput(inputId = 'select1', label = 'choose brand',
                  choices = car_brands)
    })

    output$cars <- renderUI({
      selectInput(inputId = 'select2', label = 'choose car',
                  choices = all_cars)
    })

    observeEvent(input$select1, {
      x <- input$select1
      find_pat <- sprintf('^%s', x)
      these_cars <- all_cars[grepl(find_pat, all_cars, perl = TRUE)]
      # Can also set the label and select items
      updateSelectInput(session, "select2",
                        choices = these_cars,
                        selected = NULL)
    })
  }

  ui <- fluidPage(
    uiOutput('car_brands'),
    uiOutput('cars')
  )

  shinyApp(ui, server)

}
stack.shiny()
