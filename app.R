##shiny app for spawning data calendar plots

library(shiny)
library(tidyverse)
library(googledrive)
library(googlesheets4)

## Authenticate to Google Drive interactive mode ONLY once #
#options(gargle_oauth_cache = ".secrets/")
#drive_auth()
#list.files(".secrets/")



## Authenticate to Google Drive Non-interactive mode #
drive_auth(cache = ".secrets/", email = TRUE)

sheets_auth(token = drive_token(),
            scopes = "https//www.googleapis.com/auth/spreadsheets.readonly")

# read google sheet by ID
sheets_get("1OgmHW3f9O3HHqp-5PK8jEj9vDwVHg1oTMw81Qg2GQkE")
SpawnDF <- read_sheet("1OgmHW3f9O3HHqp-5PK8jEj9vDwVHg1oTMw81Qg2GQkE", sheet = 1, col_types = "c", na = "")

# Convert dates from character to date class using lubridate package
SpawnDF$DATE <- lubridate::mdy(SpawnDF$DATE)
SpawnDF$`Start Date`=lubridate::mdy(SpawnDF$`Start Date`)
SpawnDF$`End Date`=lubridate::mdy(SpawnDF$`End Date`)

# Convert Total # of tanks to numeric vector
SpawnDF$`Total # of tanks` <- as.numeric(SpawnDF$`Total # of tanks` )

#Caluculate age of Tank
SpawnDF <- SpawnDF %>% mutate(age_Days = DATE -`Start Date`)

# Fix levels of strains
SpawnDF$Female <- as.factor(gsub("H2 ", "H2", SpawnDF$Female))
SpawnDF$Female <- as.factor(gsub("unknown.*", "Unknown", SpawnDF$Female, ignore.case = TRUE))
SpawnDF$Male <- as.factor(gsub("unknown.*", "Unknown", SpawnDF$Male, ignore.case = TRUE))
Male.sel <- c("CC7", "NA", NA)

#Simplify levels of incubator names
SpawnDF$Incubator <- as.factor(gsub("Orange.*", "Orange", SpawnDF$Incubator, ignore.case = TRUE))
SpawnDF$Incubator <- as.factor(gsub("Blue.*", "Blue", SpawnDF$Incubator, ignore.case = TRUE))
SpawnDF$Incubator <- as.factor(gsub("Green.*", "Green", SpawnDF$Incubator, ignore.case = TRUE))
SpawnDF$Incubator <- as.factor(gsub("Red.*", "Red", SpawnDF$Incubator, ignore.case = TRUE))
SpawnDF$Incubator <- as.factor(gsub("Yellow.*", "Yellow", SpawnDF$Incubator, ignore.case = TRUE))

#convert `Time Found` into hms time class
SpawnDF$`Time Found` <- gsub("NA", NA, SpawnDF$`Time Found`)
SpawnDF$`Time Found` <- lubridate::hm(SpawnDF$`Time Found`) 



ui <- fluidPage(
  tags$h2("Frequency of Aiptasia spawning in Pringle Lab"),
  column(3,
         wellPanel(
           checkboxGroupInput(inputId = "target", label = "Choose any/all incubators:", 
                            choices = c("Blue", "Red", "Green","Orange", "Yellow"),
                            selected = c("Blue", "Red", "Green","Orange", "Yellow")),
         
            dateRangeInput(inputId = "daterange", label = "Enter a date range", 
                 format = "yyyy-mm-dd", start = "2016-01-01"),
         
            selectInput(inputId ="xvar", label = "Select X-axis variable from drop down menu", choices = c("DATE", "Age of Tank", "Strain", "Time Found", "Tanks"), selected = "DATE")
         ),
         br(),
         
         submitButton("Submit")
         ),
  column(9,
         plotOutput(outputId = "spawnplot"),
         br(),
         print("This web app is now directly linked to our lab server so all data is updated in real time. 2019-12-09")
      
  )
  
)



server <- function(input, output){
  output$spawnplot <-renderPlot({ 
    if (input$xvar == "Tanks") {
      SpawnDF %>%
        filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>% 
        filter(Incubator %in% input$target) %>%
        select(`Tank Name`, Incubator, `Spawn Type`) %>% 
        group_by(`Tank Name`) %>% 
        add_count() %>% 
        unique() %>% 
        ggplot(aes(x=n)) +
        geom_bar(aes(fill=`Spawn Type`)) +
        xlab("\n Tanks with n-th multiple of spawns")+
        theme_bw() + theme(text = element_text(size = 16))
    }
    else if (input$xvar == "Age of Tank") {
      SpawnDF %>%
        filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>% 
        filter(Incubator %in% input$target) %>% 
        filter(age_Days > 0) %>%
        filter(`Start Date` > 2019-06-27) %>% 
        group_by(age_Days) %>% 
        count(`Spawn Type`) %>%
        ggplot( aes(x=age_Days, y=n, fill=`Spawn Type`)) +
        geom_bar(stat = "identity") +
        ylab("total spawn observations") + 
        xlab("\n Age of Tank (Days)") +
        theme_bw() + theme(text = element_text(size = 16)) +
        scale_x_continuous(breaks = seq(0,800, 28))
    }  
    else if (input$xvar == "Strain") {
      SpawnDF %>%
        filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>%
        filter(Incubator %in% input$target) %>%
        filter(Male %in% Male.sel) %>%
        mutate(Pair = str_c(Female, "_", Male)) %>% 
        add_count(Pair, name = "n")  %>% 
        group_by(Female, Male) %>% 
        add_count(`Spawn Type`, name = "s") %>% 
        mutate(norm.Count = s/n) %>% 
        ggplot( aes(x=Pair, y=norm.Count, fill=`Spawn Type`)) +
        geom_bar(stat = "identity") +
        ylab("normalized counts") + 
        xlab("\n Strain Pairs") +
        theme_bw() + theme(text = element_text(size = 16)) +
        theme(axis.text.x=element_text(angle=90,hjust=1))
      }
    else if (input$xvar == "Time Found") {
      SpawnDF %>%
        filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>%
        filter(Incubator %in% input$target) %>%
        ggplot( aes(y=Incubator, x=`Time Found`)) +
        geom_jitter(aes(color = `Spawn Type`)) +
        xlab("\n Time Found") + 
        ylab("Incubator") +
        scale_x_time(labels = function(x) str_sub(x,1,5))+
        theme_bw() + theme(text = element_text(size = 16)) 
        #theme(axis.text.x=element_text(angle=90,hjust=1))
    }
    else {
      SpawnDF %>% 
        filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>%
        filter(Incubator %in% input$target) %>%
        group_by(DATE, Incubator, `Total # of tanks`) %>% 
        count(`Spawn Type`) %>%
        mutate(norm.Count = n/ `Total # of tanks` )  %>%
        ggplot( aes(x=DATE, y= norm.Count, fill=`Spawn Type`) ) +
        geom_bar(stat = "identity") +
        ylab("normalized counts") +
        xlab("\n Date") +
        theme_bw()+ theme(text = element_text(size = 16)) +
        facet_grid(rows = vars(Incubator))
    }
    
    
    })
}
  



shinyApp(ui = ui, server = server)
