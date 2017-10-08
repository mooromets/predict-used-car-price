library(shiny)
library(ggplot2)
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
  
  output$plotlm <- renderPlot({

    newdata <- auto_data() %>%
      group_by(yearOfRegistration) %>%
      summarise(
        powerPS = mean(powerPS),
        kilometer = mean(kilometer))

    modelLines <- predict(pricePredict(), newdata = newdata)

    qplot(x = newdata$yearOfRegistration, 
          y = modelLines) + geom_line()

  })  
})