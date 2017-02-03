##shiny app for spawning calendar

library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)

ui <- fluidPage(
  dateRangeInput(inputId = "daterange", label = "Enter a date range", 
                 format = "yyyy-mm-dd", start = "2016-01-01", end = "2016-12-31"),
  #selectInput(inputId= "timeunit", label = "Select time unit",
              #choices = c("Calendar days" = "Date", "lunar cycle days" = "Day.of.Cycle")),
  checkboxGroupInput(inputId = "target", label = "Choose any/all incubators:", 
                     choices = c("Cabinent", "New", "Newest","Old", "Ron")),
  
  plotOutput(outputId = "spawnplot1"),
  plotOutput(outputId = "spawnplot2")
)

data_long <- readRDS("Data/2016spawncounts.rds")

server <- function(input, output){
  output$spawnplot1 <-renderPlot({
        data_long %>% 
          filter(Date > input$daterange[1] & Date < input$daterange[2]) %>%
            filter(Incubator %in% input$target) %>%
              ggplot( aes(x=Day.of.Cycle, y=Count, fill=Spawn_type)) +
              geom_bar(stat = "identity") +
              scale_x_continuous(breaks = c(1:30)) + 
              #scale_y_continuous(breaks = seq(0, 15, by =1)) +  #kept adjusting range for different incubators
              ggtitle("Spawning Frequency") +
              theme_bw()
  })
  
  output$spawnplot2 <- renderPlot({
    data_long %>% 
      filter(Date > input$daterange[1] & Date < input$daterange[2]) %>% 
      filter(Incubator %in% input$target) %>% 
        ggplot( aes(x=Date, y=Count, fill=Spawn_type)) +
        geom_bar(stat = "identity") +
        ggtitle("Spawning Frequency") +
        theme_bw()
  })
}


shinyApp(ui = ui, server = server)