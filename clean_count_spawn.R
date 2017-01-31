#aggregate and clean up spawning data for multiple incubators

library(argparse)
description = "this is my script to aggregate and clean up spawning data for multiple incubators"
parser= ArgumentParser(description=description)
parser$add_argument("-i", "--infile",required=TRUE, help="full path to csv file of dataset")
#parser$add_argument("-n", "--Name.of.Incubtor", required=TRUE, help="name of incubator used to label output files")
parser$add_argument("-rp", "--resultspath", help= "specify path to folder which will contain results")
args= parser$parse_args()

infile = args$infile

temp=unlist(strsplit(infile, split ="_"))[2]
IncubatorName = unlist(strsplit(temp, split = "\\."))[1]

resultspath = as.character(args$resultspath)

#load libraries
library(ggplot2)
library(dplyr)
library(magrittr)
# Clean up raw data -------------------------------------------------------

spawnDF=read.csv(file=infile, header = TRUE, strip.white = TRUE, na.strings = "")

#count spawning events
Eggs<- apply(spawnDF, 1, function(x) which(x == "eggs") %>% length() )
Sperm<- apply(spawnDF, 1, function(x) which(x == "sperm") %>% length() )
Embryos<-apply(spawnDF, 1, function(x) which(x == "embryos") %>% length() )
DisEgg<-apply(spawnDF, 1, function(x) which(x == "disintegrated eggs") %>% length() )
Larvae <-apply(spawnDF, 1, function(x) which(x == "larvae") %>% length() )

#make new aggregated DF
countDF=select(spawnDF, Date, Day.of.Cycle)
countDF$Date<-as.Date(countDF$Date, format="%m/%d/%y")
countDF=mutate(countDF, DisEgg, Eggs, Sperm, Embryos, Larvae)
countDF=mutate(countDF, Gamete_sum= DisEgg + Eggs + Sperm + Embryos*2 + Larvae*2)
countDF=mutate(countDF, Incubator = IncubatorName)

#save DF
output=paste(IncubatorName, "spawncounts.csv", sep = '_')
write.csv(countDF, file=paste(resultspath, output, sep = '/'), row.names = FALSE)


##following code depreciated and ploting chunks moved to plot_spawning.R for interactive R sessions
## Analyze spawning frequency ----------------------------------------------

# library(reshape2)
# 
# #frequency by Day of Cycle
# data_long <- melt(countDF,
#       # ID variables - all the variables to keep but not split apart on
#       id.vars=c("Date", "Day.of.Cycle"),
#       # The source columns
#       measure.vars=c("Eggs", "Sperm", "DisEgg", "Embryos", "Larvae" ),
#       # Name of the destination column that will identify the original column that the measurement came from
#       variable.name="Spawn_type",
#       value.name="Count")
# 
# output2=paste(IncubatorName, "bycycle.pdf", sep = "_")
# pdf(file=paste(resultspath, output2, sep="/"))
# 
# ggplot(data=data_long, aes(x=Day.of.Cycle, y=Count, fill=Spawn_type)) +
#   geom_bar(stat = "identity") +
#   scale_x_continuous(breaks = c(1:30)) + 
#   scale_y_continuous(breaks = seq(0, 15, by =1)) +  #kept adjusting range for different incubators
#   ggtitle(paste(IncubatorName, "Spawning Frequency", sep = " ")) +
#   theme_bw()
# 
# dev.off()
#   
# 
# #frequency by calendar date
# 
# output3=paste(IncubatorName, "byDate.pdf", sep = "_")
# pdf(file=paste(resultspath, output3, sep = '/'), width = 10)
# 
# ggplot(data=data_long, aes(x=Date, y=Count, fill=Spawn_type)) + 
#   geom_bar(stat = "identity") +
#   scale_x_date(date_breaks="month", date_labels=("%b"))+
#   ggtitle(paste(IncubatorName, "Spawning Frequency", sep = " ")) +
#   theme_bw()
# 
# dev.off()

##failed attempt to have two x-axis labels  
#two x-axis labels strategy: combine the two with a line break in a single set of labels
#  l <- with(cycle_long, paste(Date, Day.of.Cycle, sep = "\n")) 



