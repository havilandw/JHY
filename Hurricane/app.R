library(tidyverse)
library(shiny)
library(ggplot2)
library(drat)
library(dplyr)
addRepo("geanders")

library(hurricaneexposuredata)

# use 2 datasets in the package
# attach the hurr_tracks data
data("hurr_tracks")
data_ht <- as.data.frame(hurr_tracks)
# add a column "year"
data_ht$year <- substr(data_ht$date, 1, 4)

# attach the rain data
data("rain")
data_rain <- as.data.frame(rain)

## build a data frame that contains one line
## of data per storm consisting of:
## Storm_name, beginning data and time, ending time,
## max(rain), max(wind)
##  
## plot x=time, y=rain, the storm marker is scaled in size to reflects the max wind.
##
## iteration can be done with a for-loop, an apply functional, or purrr().
##


## list of storms
storms <- hurr_tracks$storm_id %>% unique()

## for each storm find max(rain) and max(wind)
##  and earliest time and latest time.
##


ui <- fluidPage(
  title = "Examples of Data Tables",
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId="m",label="Number of samples:", 2, min=1, max=100,width=NULL)
    ),
    
    tabsetPanel(
      conditionalPanel(
        'input.dataset === "data_ht"'),
      
      conditionalPanel(
        'input.dataset === "data_rain"',
      )
    ),
    mainPanel(
      plotOutput("hist"),
      tabsetPanel(
        id = 'dataset',
        tabPanel("Storm tracks for Atlantic basin storms data table",
                 
                 # Create a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("storm",
                                      "Storm ID:",
                                      c("All",
                                        unique(as.character(data_ht$storm_id))))
                   ),
                   column(4,
                          selectInput("year",
                                      "Year:",
                                      c("All",
                                        unique(data_ht$year)))
                   )
                 ),
                 # Create a new row for the table.
                 DT::dataTableOutput("table1")),
        
        tabPanel("Rainfall for US counties during Atlantic basin tropical storms data table",
                 
                 # Create a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("storm",
                                      "Storm ID:",
                                      c("All",
                                        unique(as.character(data_rain$storm_id))))
                   ),
                   column(4,
                          selectInput("fips",
                                      "Fips:",
                                      c("All",
                                        unique(data_rain$fips)))
                   )
                 ),
                 # Create a new row for the table.
                 DT::dataTableOutput("table2")))
      
    )
  )
)


server <- function(input, output, session) {
  
  # Filter data based on selections
  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks =20)
      })
  }
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- data_ht
    if (input$storm != "All") {
      data <- data[data$storm_id == input$storm,]
    }
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    
    data
  }))
  
  # sorted columns are colored now because CSS are attached to them
  # Filter data based on selections
  output$table2 <- DT::renderDataTable(DT::datatable({
    data2 <- data_rain
    if (input$storm != "All") {
      data2 <- data2[data_rain$storm_id == input$storm,]
    }
    if (input$fips != "All") {
      data2 <- data2[data_rain$fips == input$fips,]
    }
    
    data2
  }))
  
  


# Run the application 
shinyApp(ui = ui, server = server)
