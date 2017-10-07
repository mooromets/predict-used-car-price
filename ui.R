library(shiny)
source("./utils.R")

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      h3("Filters:"),
      sliderInput("price", "Price (Euro):",
                  min = 100, max = 150000,
                  value = c(3000, 100000),
                  step = 100),
      sliderInput("year", "First registration (year):",
                  min = 1975, max = 2016,
                  value = c(1995, 2012),
                  sep = NULL),
      sliderInput("km", "km:",
                  min = 0, max = max(clean_auto$kilometer),
                  value = c(0, 100000),
                  step = 10000),
      selectInput("brand", "Brand (may be multiple):", 
                  choices = unique(as.character(clean_auto$brand)),
                  multiple = TRUE),
      uiOutput("modelControls"),
      selectInput("state", "State (may be multiple):", 
                  choices = unique(as.character(clean_auto$State)),
                  multiple = TRUE),
      selectInput("type", "Vehicle type (may be multiple):", 
                  choices = unique(as.character(clean_auto$vehicleType)),
                  multiple = TRUE),
      sliderInput("hp", "HorsePower:",
                  min = min(clean_auto$powerPS), max = max(clean_auto$powerPS),
                  value = c(min(clean_auto$powerPS), max(clean_auto$powerPS))),
      radioButtons("gearbox", "Gearbox:", 
                  choices = getRadioChoices("gearbox")),
      selectInput("fuel", "Fuel (may be multiple):", 
                  choices = unique(as.character(clean_auto$fuelType)),
                  multiple = TRUE),
      radioButtons("damage", "Not repaired damage:", 
                  choices = getRadioChoices("notRepairedDamage"))
      ),
    
    mainPanel(
      fluidRow(textOutput("obsTotal")),
      plotOutput("plotlm")
#      fluidRow(textOutput("low"),textOutput("high")), 
#      fluidRow(textOutput("after"),textOutput("before")), 
#      fluidRow(textOutput("states")),
#      fluidRow(textOutput("brands")),
#      fluidRow(textOutput("models")),
#      fluidRow(textOutput("over", inline = TRUE), textOutput("under", inline = TRUE)),
#      fluidRow(textOutput("types"))
    )
  )
))