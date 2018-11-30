#Check to see if there is really an incompatibility of CC7- H2 pairing or CC7-PLf pairings.
library(tidyverse)

'%!in%' <- function(x,y)!('%in%'(x,y))

Spawn <- read.csv(file = "Data/SpawningData20171130.csv", header = TRUE)
Spawn$Spawn.Date=as.Date(Spawn$Spawn.Date)
Spawn$Start.Date = as.Date(Spawn$Start.Date, format = "%m/%d/%y")
Spawn$End.Date = as.Date(Spawn$End.Date, format = "%m/%d/%y")

#remove leading/trailing whitespace
SType <- as.factor(trimws(Spawn$Spawn.Type, "right"))

#replace column data
Spawn$Spawn.Type = SType

Spawn <- Spawn %>% mutate(age = Spawn.Date - Start.Date)

#plot frequency and type of spawning vs age of tank
Spawn %>% 
  filter(Spawn.Date >= "2016-01-01") %>% 
  filter(age > 0) %>%  
  #filter(Incubator %in% input$target) %>%
  group_by(age, Total...of.tanks) %>% 
  count(Spawn.Type) %>%
  #mutate(norm.Count = n/ Total...of.tanks )  %>%
  ggplot( aes(x=age, y= n, fill=Spawn.Type) ) +
  geom_bar(stat = "identity") +
  ggtitle("Spawning Frequency by age of tank") + ylab("raw counts")+
  theme_bw()+ theme(text = element_text(size = 16))
