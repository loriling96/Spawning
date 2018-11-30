## make ggplots

library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)

# Using updated Spawning Data retrieved 2017-03-06 ---------------------------------------------
SpawnDF=read.csv(file="./Data/SpawningData20170331.csv", header = TRUE)

levels(SpawnDF$Spawn.Type)
#remove leading/trailing whitespace
SType=trimws(SpawnDF$Spawn.Type, "right")
#reassign levels
levels(SType)= c("eggs", "sperm", "embryos", "larvae")
#replace column data
SpawnDF$Spawn.Type = SType


#make sure Spawn.Dates are date class
SpawnDF$Spawn.Date=as.Date(SpawnDF$Spawn.Date, format = "%m/%d/%y")

#count spawning events
All_count<- SpawnDF %>% 
  group_by(Spawn.Date, Day.of.Cycle, Incubator, Total...of.tanks) %>% 
  count(Spawn.Type) %>% 
  as.data.frame()

colnames(All_count)[colnames(All_count)=="n"] <- "Count"

#calculate incubator efficiency Count/Tanks
#All_count<- All_count %>% mutate(norm.Count = Count/ Total...of.tanks)
##Just noticed that Total...of.tanks columns is the cumulative number of tanks, not number of active tanks

saveRDS(All_count, file = "./Data/SpawnCounts20170306.rds")

daterange=as.Date(c("2015-01-01", "2017-03-31"))

SpawnDF %>% 
  filter(Spawn.Date >= daterange[1] & Spawn.Date <= daterange[2]) %>% 
  filter(Incubator %in% target) %>%
  group_by(Spawn.Date, Total...of.tanks) %>% 
  count(Spawn.Type) %>%
  mutate(norm.Count = n/ Total...of.tanks )  %>%
  ggplot( aes(x=Spawn.Date, y= norm.Count, fill=Spawn.Type) ) +
  geom_bar(stat = "identity") +
  ggtitle("Spawning Frequency by calendar dates") + ylab("normalized counts")+
  theme_bw()+ theme(text = element_text(size = 16))


# MERGE ALL INCUBATOR FORMATTED DATA INTO SUPER DF ------------------------

# Cabinet_count=read.csv(file = "./Data/Cabinet_spawncounts.csv")
# New_count=read.csv(file="./Data/NewIncubator_spawncounts.csv")
# Newest_count=read.csv(file = "./Data/NewestIncubator_spawncounts.csv")
# Old_count=read.csv(file = "./Data/OldIncubator_spawncounts.csv")
# Ron_count=read.csv(file = "./Data/Ron_spawncounts.csv")
# 
# #I don't understand why the read.csv function reads date in YYYY-MM-DD format for Ron and Cabinet but not the other three
# #format into Date class
# Old_count$Date=as.Date(Old_count$Date, format = "%m/%d/%y")
# New_count$Date=as.Date(New_count$Date, format = "%m/%d/%y")
# Newest_count$Date=as.Date(Newest_count$Date, format = "%m/%d/%y")
# Ron_count$Date=as.Date(Ron_count$Date)
# Cabinet_count$Date=as.Date(Cabinet_count$Date)
# 
# ALL_count=dplyr::bind_rows(Cabinet_count, New_count, Newest_count, Old_count, Ron_count)
# ALL_count$Incubator=as.factor(ALL_count$Incubator) 
# 
# data_long = ALL_count %>% 
#   gather(key= "Spawn_type", value = "Count", 3:7)
# data_long$Spawn_type=as.factor(data_long$Spawn_type) 
# levels(data_long$Spawn_type) = c("DisEgg", "Eggs", "Sperm", "Embryos", "Larvae")


# # plots in lunar cycle days -----------------------------------------------
# 
# data_long %>% 
#   filter(Date > '2016-01-01' & Date < '2016-12-31' & Incubator == "Old") %>% 
#   ggplot( aes(x=Date, y=Count, fill=Spawn_type)) +
#   geom_bar(stat = "identity") +
#   #scale_x_continuous(breaks = c(1:30)) + 
#   #scale_y_continuous(breaks = seq(0, 15, by =1)) +  #kept adjusting range for different incubators
#   ggtitle("Spawning Frequency") +
#   theme_bw()
# 
# 
# 
# # plots in calendar days --------------------------------------------------
# data_long %>% 
#   filter(Date > '2016-12-01' & Date < '2016-12-31') %>%
#   ggplot(data=data_long, aes(x=Date, y=Count, fill=Spawn_type)) + 
#   geom_bar(stat = "identity") +
#   scale_x_date(date_breaks="month", date_labels=("%b"))+
#   ggtitle("Spawning Frequency") +
#   theme_bw()


  