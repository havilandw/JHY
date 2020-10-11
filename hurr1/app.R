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
data_rain <- data.frame(rain[2], rain[6])
data_hurr <- data.frame(hurr_tracks[1], hurr_tracks[3], hurr_tracks[6])
## of data per storm consisting of:
## Storm_name, beginning data and time, ending time,
library(lubridate)
date <- ymd_hm(data_hurr$date)
data_hurr <- data.frame(hurr_tracks[1], date, hurr_tracks[6])

## max(rain), max(wind)

rain_max <- data_rain %>%
  group_by(storm_id) %>%
  summarise(
    precip_max = max(precip_max, na.rm = T)
  ) %>%
  arrange(storm_id)

wind_max <- data_hurr %>%
  group_by(storm_id) %>%
  summarise(
    wind = max(wind, na.rm = T)
  ) %>%
  arrange(storm_id)

data_ht <- data.frame(wind_max)
data_ht2<- data.frame(rain_max)

##  
## plot x=time, y=rain, the storm marker is scaled in size to reflects the max wind.
png(width=100, height =100)
plot <- ggplot(data=rain_max, aes(x=storm_id, y=precip_max), color ="blue")+geom_point()
dev.off()
png(width=100, height =100)
plot2 <- ggplot(data=wind_max, aes(x=storm_id, y=wind), color ="blue")+geom_point()
dev.off()


## list of storms
storms <- hurr_tracks$storm_id %>% unique()




ui <- fluidPage(
  title = "Examples of Data Tables",
  sidebarLayout(
    tabsetPanel(
      conditionalPanel(
        'input.dataset === "data_ht"'),
      
      conditionalPanel(
        'input.dataset === "data_ht2"',
      )
    ),
    mainPanel(
      

      tabsetPanel(
        id = 'dataset',
        tabPanel("Storm tracks for Atlantic basin storms data table",
                 plotOutput(outputId="plot2", width="100%"),
                 
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
                 plotOutput(outputId="plot", width="100%"),
                
                 
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


server <- function(input, output) {
  
  # Filter data based on selections
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
  output$plot <- renderPlot({
    p <- ggplot(data=rain_max, aes(x=storm_id, y=precip_max), color ="blue", height=100, width=100)+geom_point()
    print(p)
  })
  output$plot2 <- renderPlot({
    p2 <- ggplot(data=wind_max, aes(x=storm_id, y=wind), color ="blue", height=100, width=100)+geom_point()
    print(p2)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



