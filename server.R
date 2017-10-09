library(shiny)
library(ggplot2)
library(leaflet)
source("./utils.R")

Server = function(input, output) {

  output$modelControls <- renderUI({
    models <- getModels(input$brand)
    selectInput("model", "Models (multiple):", models, multiple = TRUE)
  })
  
  auto_data <- reactive({
    filtered_auto(input)
  })

  output$obsTotal <- renderText(
    paste(as.character(dim(auto_data())[1]), "items of total 293 thousands in database"))
  output$q1st <- renderText(paste("1st quartile:",
                                  as.character(quantile(auto_data()$price, probs = .25)),
                                  "Euro"))
  output$median <- renderText(paste("Median:",
                                  as.character(quantile(auto_data()$price, probs = .5)),
                                  "Euro"))
  output$q3rd <- renderText(paste("3rd quartile:",
                                  as.character(quantile(auto_data()$price, probs = .75)),
                                  "Euro"))
    
  pricePredict <- reactive({
    fast_lm(auto_data())
  })
  
  output$plotlm <- renderPlot({
    tmp <- auto_data()
    Ys <- predict(pricePredict())
    ggplot() +
      geom_boxplot(aes(x = tmp$yearOfRegistration, y = Ys, group = tmp$yearOfRegistration)) +
      labs(x = "Year of the first registration", y = "Price (Euro)")
  })
  
  output$plotyear <- renderPlot({
    ggplot(auto_data(), aes(yearOfRegistration)) + 
      geom_histogram(binwidth = 1) +
      labs(x = "Year of the first registration")
  })

  output$plotkm <- renderPlot({
    ggplot(auto_data(), aes(kilometer)) + 
      geom_histogram(binwidth = 10000) +
      labs(x = "Total kilometers")
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

}