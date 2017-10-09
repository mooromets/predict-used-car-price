library(shiny)
library(leaflet)
source("./data-clean.R")
source("./utils.R")

Ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("Filters:"),
      sliderInput("price", "Price (Euro):",
                  min = 100, max = 150000,
                  value = c(2000, 50000),
                  step = 100),
      sliderInput("year", "First registration (year):",
                  min = 1975, max = 2015,
                  value = c(2005, 2015),
                  sep = NULL),
      sliderInput("km", "km:",
                  min = 0, max = max(clean_auto$kilometer),
                  value = c(0, 100000),
                  step = 10000),
      fluidRow(
        column(6,
               selectInput("brand", "Brand (multiple):", 
                           choices = unique(as.character(clean_auto$brand)),
                           multiple = TRUE)),
        column(6,
               uiOutput("modelControls"))
      ),
      sliderInput("hp", "HorsePower:",
                  min = min(clean_auto$powerPS), max = max(clean_auto$powerPS),
                  value = c(min(clean_auto$powerPS), max(clean_auto$powerPS))),
      fluidRow(
        column(6,
               radioButtons("damage", "Not repaired damage:", 
                     choices = getRadioChoices("notRepairedDamage"))),
        column(6,
               radioButtons("gearbox", "Gearbox:", 
                     choices = getRadioChoices("gearbox")))
      ),
      fluidRow(
        column(6,
               selectInput("type", "Vehicle type:", 
                           choices = unique(as.character(clean_auto$vehicleType)),
                           multiple = TRUE)),
        column(6,
               selectInput("fuel", "Fuel:", 
                           choices = unique(as.character(clean_auto$fuelType)),
                           multiple = TRUE))
      ),
      selectInput("state", "State (multiple allowed):", 
                  choices = unique(as.character(clean_auto$State)),
                  multiple = TRUE)
      ),
    
    mainPanel(tabsetPanel(type = "tabs",
                          tabPanel("Price and offer",
                                   br(),
                                   h6(textOutput("obsTotal")),
                                   fluidRow(
                                     column(4, h5(textOutput("q1st"))),
                                     column(4, h5(textOutput("median"))),
                                     column(4, h5(textOutput("q3rd")))
                                   ),
                                   plotOutput("plotlm"),
                                   plotOutput("plotyear", height = 200),
                                   plotOutput("plotkm", height = 200),
                                   a("https://github.com/mooromets/predict-used-car-price")),
                          tabPanel("Offers on map",
                                   br(),
                                   leafletOutput("onMap", height = 700),
                                   a("https://github.com/mooromets/predict-used-car-price"))
                          )
              )
  )
)