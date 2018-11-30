fluidPage(
  titlePanel("Frequency of Aiptasia spawning in Pringle Lab"),
  column(3,
         checkboxGroupInput(inputId = "target", label = "Choose any/all incubators:", 
                            choices = c("Blue (Cabinet)", "Red (New)", "Green (Newest)","Orange (Old)", "Yellow (Ron)"),
                            selected = c("Blue (Cabinet)", "Red (New)", "Green (Newest)","Orange (Old)", "Yellow (Ron)")),
         dateRangeInput(inputId = "daterange", label = "Enter a date range", 
                        format = "yyyy-mm-dd", start = "2016-01-01"),
         
         wellPanel(
           selectInput("xvar", "X-axis variable", axis_vars, selected = "Meter"),
           selectInput("yvar", "Y-axis variable", axis_vars, selected = "Reviews")
         )
         
  ),
  column(9,
         plotOutput(outputId = "spawnplot1"),
         plotOutput(outputId = "spawnplot2")),
  
  print("last updated 2018-02-28")
  
)