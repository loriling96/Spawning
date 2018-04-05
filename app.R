##shiny app for spawning data calendar plots

library(shiny)
library(tidyverse)
library(magrittr)
library(htmltools)

# Global variables
SpawnDF <- read.csv(file="Data/SpawningData20180404.csv", header = TRUE)

#check that columns with dates are not character but date class
SpawnDF$Spawn.Date=as.Date(SpawnDF$Spawn.Date)
SpawnDF$Start.Date=as.Date(SpawnDF$Start.Date)
SpawnDF$End.Date=as.Date(SpawnDF$End.Date, "%Y-%m-%d")
SpawnDF <- SpawnDF %>% mutate(age_Days = Spawn.Date - Start.Date) %>% 
  mutate(comp = age_Days >= Day.of.Cycle) %>% 
  mutate(Lunar.age = 28 * (as.numeric(age_Days) %/% 28) + Day.of.Cycle -1)


#check that only four levels of spawn type
levels(SpawnDF$Spawn.Type)

ui <- fluidPage(
  tags$h2("Frequency of Aiptasia spawning in Pringle Lab"),
  column(3,
         checkboxGroupInput(inputId = "target", label = "Choose any/all incubators:", 
                            choices = c("Blue (Cabinet)", "Red (New)", "Green (Newest)","Orange (Old)", "Yellow (Ron)"),
                            selected = c("Blue (Cabinet)", "Red (New)", "Green (Newest)","Orange (Old)", "Yellow (Ron)")),
         
         dateRangeInput(inputId = "daterange", label = "Enter a date range", 
                 format = "yyyy-mm-dd", start = "2016-01-01"),
         
         selectInput(inputId ="xvar", label = "Select X-axis variable", choices = c("Spawn.Date", "Lunar.age", "Strain"), selected = "Spawn.Date"),
         
         br(),
         
         submitButton("Submit")
         ),
  column(9,
         plotOutput(outputId = "spawnplot"),
         print("last updated 2018-04-04")
  )
  
)



server <- function(input, output){
  output$spawnplot <-renderPlot({ 
    if (input$xvar == "Lunar.age") {
      SpawnDF %>%
        filter(Spawn.Date >= input$daterange[1] & Spawn.Date <= input$daterange[2]) %>% 
        filter(Incubator %in% input$target) %>% 
        filter(comp == TRUE & Lunar.age > 0) %>%
        group_by(Lunar.age) %>% 
        count(Spawn.Type) %>%
        ggplot( aes(x=Lunar.age, y=n, fill=Spawn.Type)) +
        geom_bar(stat = "identity") +
        ggtitle("Spawn Type Frequency") + 
        ylab("total spawn observations") + xlab(paste(input$xvar)) +
        theme_bw() + theme(text = element_text(size = 16))
    }  
    if (input$xvar == "Strain") {
      SpawnDF %>%
        filter(Spawn.Date >= input$daterange[1] & Spawn.Date <= input$daterange[2]) %>% 
        filter(Incubator %in% input$target) %>% 
        group_by(Female) %>% 
        count(Spawn.Type) %>%
        ggplot( aes(x=Female, y=n, fill=Spawn.Type)) +
        geom_bar(stat = "identity") +
        ggtitle("Spawn Type Frequency") + 
        ylab("total spawn observations") + xlab(paste(input$xvar)) +
        theme_bw() + theme(text = element_text(size = 16))
      }
    else {
      SpawnDF %>% 
        filter(Spawn.Date >= input$daterange[1] & Spawn.Date <= input$daterange[2]) %>% 
        filter(Incubator %in% input$target) %>%
        group_by(Spawn.Date, Total...of.tanks) %>% 
        count(Spawn.Type) %>%
        mutate(norm.Count = n/ Total...of.tanks )  %>%
        ggplot( aes(x=Spawn.Date, y= norm.Count, fill=Spawn.Type) ) +
        geom_bar(stat = "identity") +
        ggtitle("Spawning Frequency by calendar dates") + ylab("normalized counts")+
        theme_bw()+ theme(text = element_text(size = 16))
    }
    
    
    })
}
  



shinyApp(ui = ui, server = server)