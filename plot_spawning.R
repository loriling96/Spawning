## make ggplots

library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)


# MERGE ALL INCUBATOR FORMATTED DATA INTO SUPER DF ------------------------

Ron_count=read.csv(file = "../Spawning/formattedData/Ron_spawncounts.csv", header = TRUE)
New_count=read.csv(file="../Spawning/formattedData/NewIncubator_spawncounts.csv", header = TRUE)
Newest_count=read.csv(file = "../Spawning/formattedData/NewestIncubator_spawncounts.csv", header= TRUE)
Old_count=read.csv(file = "../Spawning/formattedData/OldIncubator_spawncounts.csv", header= TRUE)
Cabinet_count=read.csv(file = "../Spawning/formattedData/Cabinet_spawncounts.csv", header= TRUE)

ALL_count=dplyr::bind_rows(Cabinet_count, New_count, Newest_count, Old_count, Ron_count)

data_long = ALL_count %>% 
  gather(key= "Spawn_type", value = "count", 3:7)

data_long$Date = as.Date(data_long$Date)


# plots in lunar cycle days -----------------------------------------------

data_long %>% 
  filter(Date > '2016-12-01' & Date < '2016-12-31') %>% 
  ggplot( aes(x=Day.of.Cycle, y=Count, fill=Spawn_type)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(1:30)) + 
  scale_y_continuous(breaks = seq(0, 15, by =1)) +  #kept adjusting range for different incubators
  ggtitle("Spawning Frequency") +
  theme_bw()



# plots in calendar days --------------------------------------------------
data_long %>% 
  filter(Date > '2016-12-01' & Date < '2016-12-31') %>%
  ggplot(data=data_long, aes(x=Date, y=Count, fill=Spawn_type)) + 
  geom_bar(stat = "identity") +
  scale_x_date(date_breaks="month", date_labels=("%b"))+
  ggtitle("Spawning Frequency") +
  theme_bw()


  