##shiny app for spawning data calendar plots

library(shiny)
library(tidyverse)
library(htmltools)
library(hms)

# Global variables
SpawnDF <- read.csv(file="Data/SpawningData20191112.csv", header = TRUE)

# new paste function to work on a vector
paste3 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}

#check that columns with dates are not character but date class
SpawnDF$DATE=as.Date(SpawnDF$DATE, "%m-%d-%Y") #updated column header
SpawnDF$Start.Date=as.Date(SpawnDF$Start.Date, "%m-%d-%Y")
SpawnDF$End.Date=as.Date(SpawnDF$End.Date, "%m-%d-%Y")

#Caluculate lunar age
#SpawnDF <- SpawnDF %>% mutate(age_Days = DATE -Start.Date) %>% 
#  mutate(comp = age_Days >= Day.of.Cycle) %>% 
#  mutate(Lunar.age = 28 * (as.numeric(age_Days) %/% 28) + Day.of.Cycle -1)

#Caluculate age of Tank
SpawnDF <- SpawnDF %>% mutate(age_Days = DATE -Start.Date)

# Fix levels of strains
SpawnDF$Female <- as.factor(gsub("H2 ", "H2", SpawnDF$Female))
SpawnDF$Female <- as.factor(gsub("unknown.*", "Unknown", SpawnDF$Female, ignore.case = TRUE))
SpawnDF$Male <- as.factor(gsub("unknown.*", "Unknown", SpawnDF$Male, ignore.case = TRUE))
Male.sel <- c("CC7", NA)

#Simplify levels of incubator names
SpawnDF$Incubator <- as.factor(gsub("Orange.*", "Orange", SpawnDF$Incubator, ignore.case = TRUE))
SpawnDF$Incubator <- as.factor(gsub("Blue.*", "Blue", SpawnDF$Incubator, ignore.case = TRUE))
SpawnDF$Incubator <- as.factor(gsub("Green.*", "Green", SpawnDF$Incubator, ignore.case = TRUE))
SpawnDF$Incubator <- as.factor(gsub("Red.*", "Red", SpawnDF$Incubator, ignore.case = TRUE))
SpawnDF$Incubator <- as.factor(gsub("Yellow.*", "Yellow", SpawnDF$Incubator, ignore.case = TRUE))

#convert Time.found into hms time class
which(SpawnDF$Time.Found == "unknown")
SpawnDF$Time.Found <- gsub("unknown", NA, SpawnDF$Time.Found) # fix that one "unknown" to NA
which(SpawnDF$Time.Found == "unknown")
SpawnDF$Time.Found <- as.character(SpawnDF$Time.Found)
SpawnDF$Time.Found <- paste3(SpawnDF$Time.Found, "00", sep = ":") #needed for hms function, which expects hour:minute:second in argument
SpawnDF$Time.Found <- str_replace_all(SpawnDF$Time.Found, "^00", NA_character_)


ui <- fluidPage(
  tags$h2("Frequency of Aiptasia spawning in Pringle Lab"),
  column(3,
         checkboxGroupInput(inputId = "target", label = "Choose any/all incubators:", 
                            choices = c("Blue", "Red", "Green","Orange", "Yellow"),
                            selected = c("Blue", "Red", "Green","Orange", "Yellow")),
         
         dateRangeInput(inputId = "daterange", label = "Enter a date range", 
                 format = "yyyy-mm-dd", start = "2016-01-01"),
         
         selectInput(inputId ="xvar", label = "Select X-axis variable from drop down menu", choices = c("DATE", "Age of Tank", "Strain", "Time.Found", "Tanks"), selected = "DATE"),
         
         br(),
         
         submitButton("Submit")
         ),
  column(9,
         plotOutput(outputId = "spawnplot"),
         br(),
         print("last updated 2019-11-12")
      
  )
  
)



server <- function(input, output){
  output$spawnplot <-renderPlot({ 
    if (input$xvar == "Tanks") {
      SpawnDF %>%
        filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>% 
        filter(Incubator %in% input$target) %>%
        select(Tank.Name, Incubator, Spawn.Type) %>% 
        group_by(Tank.Name) %>% 
        add_count() %>% 
        unique() %>% 
        ggplot(aes(x=n)) +
        geom_bar(aes(fill=Spawn.Type)) +
        xlab("Tanks with nth multiple of spawns")+
        theme_bw() + theme(text = element_text(size = 16))
    }
    else if (input$xvar == "Age of Tank") {
      SpawnDF %>%
        filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>% 
        filter(Incubator %in% input$target) %>% 
        filter(age_Days > 0) %>%
        filter(Start.Date > 2019-06-27) %>% 
        group_by(age_Days) %>% 
        count(Spawn.Type) %>%
        ggplot( aes(x=age_Days, y=n, fill=Spawn.Type)) +
        geom_bar(stat = "identity") +
        ylab("total spawn observations") + xlab(paste(input$xvar)) +
        theme_bw() + theme(text = element_text(size = 16)) +
        scale_x_continuous(breaks = seq(0,112, 28))
    }  
    else if (input$xvar == "Strain") {
      SpawnDF %>%
        filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>% 
        filter(Incubator %in% input$target) %>% 
        filter(Male %in% Male.sel) %>%
        mutate(Pair = paste0(Female, "_", Male)) %>% 
        add_count(Pair) %>% 
        group_by(Female, Male) %>% 
        add_count(Spawn.Type) %>% 
        mutate(norm.Count = nn/n) %>% 
        ggplot( aes(x=Pair, y=norm.Count, fill=Spawn.Type)) +
        geom_bar(stat = "identity") +
        ylab("normalized counts") + xlab(input$xvar) +
        theme_bw() + theme(text = element_text(size = 16)) +
        theme(axis.text.x=element_text(angle=90,hjust=1))
      }
    else if (input$xvar == "Time.Found") {
      SpawnDF %>%
        filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>% 
        filter(Incubator %in% input$target) %>% 
        filter(!str_detect(Time.Found, "^4")) %>%
        ggplot( aes(x=Incubator, y=as.hms(Time.Found))) +
        geom_jitter(aes(color = Spawn.Type)) +
        ylab("Time Found") + 
        xlab("Incubator") +
        scale_y_time(labels = function(y) str_sub(y,1,5))+
        theme_bw() + theme(text = element_text(size = 16)) 
        #theme(axis.text.x=element_text(angle=90,hjust=1))
    }
    else {
      SpawnDF %>% 
        filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>% 
        filter(Incubator %in% input$target) %>%
        group_by(DATE, Total...of.tanks) %>% 
        count(Spawn.Type) %>%
        mutate(norm.Count = n/ Total...of.tanks )  %>%
        ggplot( aes(x=DATE, y= norm.Count, fill=Spawn.Type) ) +
        geom_bar(stat = "identity") +
        ylab("normalized counts") + xlab(paste(input$xvar)) +
        theme_bw()+ theme(text = element_text(size = 16))
    }
    
    
    })
}
  



shinyApp(ui = ui, server = server)