library(shiny)
library(ggplot2)
library(leaflet)
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
  
  output$onMap <- renderLeaflet({
    postalData <- auto_data() %>% 
      group_by(postalCode) %>%
      summarise(count = n(),
                lat = first(Latitude),
                lng = first(Longitude))
    
    postalData[,c("lat", "lng")] %>% 
      leaflet() %>%
      addTiles() %>% 
      addCircleMarkers(popup = paste(as.character(postalData$count), "item(s) at ", postalData$postalCode),
                       weight = 1, 
                       radius = 10 + log(postalData$count) * 3,
                       clusterOptions = markerClusterOptions())    
  })

})