library(shiny)
source("./utils.R")

shinyServer(function(input, output) {
  output$low <- renderText(input$price[1])
  output$high <- renderText(input$price[2])
  
  output$after <- renderText(input$year[1])
  output$before <- renderText(input$year[2])
  
  output$states <- renderText(paste(input$state))
  output$brands <- renderText(paste(input$brand))
  output$models <- renderText(paste(input$model))
  
  output$over <- renderText(input$km[1])
  output$under <- renderText(input$km[2])
  
  output$modelControls <- renderUI({
    models <- getModels(input$brand)
    selectInput("model", "Models (may be multiple):", models, multiple = TRUE)
  })  
  
  output$types <- renderText(input$type)
  
  
})