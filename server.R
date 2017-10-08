library(shiny)
source("./utils.R")

shinyServer(function(input, output) {

  output$modelControls <- renderUI({
    models <- getModels(input$brand)
    selectInput("model", "Models (multiple):", models, multiple = TRUE)
  })
  
  auto_data <- reactive({
    filtered_auto(input)
  })

  output$obsTotal <- renderText(
    paste(as.character(dim(auto_data())[1]), "results of total 293 thousands"))
  
  pricePredict <- reactive({
    fast_lm(auto_data())
  })
  
#  output$low <- renderText(input$price[1])
#  output$high <- renderText(input$price[2])
  
#  output$after <- renderText(input$year[1])
#  output$before <- renderText(input$year[2])
  
#  output$states <- renderText(paste(input$state))
#  output$brands <- renderText(paste(input$brand))
#  output$models <- renderText(paste(input$model))
  
#  output$over <- renderText(input$km[1])
#  output$under <- renderText(input$km[2])
  
  
#  output$types <- renderText(input$type)
  
  
})