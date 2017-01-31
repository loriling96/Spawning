## make ggplots

#input filepath to csv containing the cleaned up data
countDF=
  
library(ggplot2)
library(dplyr)
library(magrittr)
library(reshape2)

data_long <- melt(countDF,
                  # ID variables - all the variables to keep but not split apart on
                  id.vars=c("Date", "Day.of.Cycle"),
                  # The source columns
                  measure.vars=c("Eggs", "Sperm", "DisEgg", "Embryos", "Larvae" ),
                  # Name of the destination column that will identify the original column that the measurement came from
                  variable.name="Spawn_type",
                  value.name="Count")

data_long$Date = as.Date(data_long$Date)

data_long %>% 
  filter(Date > '2016-12-01' & Date < '2016-12-31') %>% 
  ggplot( aes(x=Day.of.Cycle, y=Count, fill=Spawn_type)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(1:30)) + 
  scale_y_continuous(breaks = seq(0, 15, by =1)) +  #kept adjusting range for different incubators
  ggtitle("Spawning Frequency") +
  theme_bw()
  