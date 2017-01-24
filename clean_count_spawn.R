#aggregate and clean up spawning data for multiple incubators
library(ggplot2)
library(dplyr)
library(magrittr)

library(argparse)
description = "this is my script to aggregate and clean up spawning data for multiple incubators"
parser= ArgumentParser(description=description)
parser$add_argument("-i", "--infile",required=TRUE, help="full path to csv file of dataset")
parser$add_argument("-n", "--Name.of.Incubtor", required=TRUE, help="name of incubator used to label output files")
parser$add_argument("-rp", "--resultspath", help= "specify path to folder which will contain results")
args= parser$parse_args()

infile=args$infile
IncubatorName=as.character(args$Name.of.Incubator)
# resultspath = as.character(args$resultspath)

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

# Analyze spawning frequency ----------------------------------------------

library(reshape2)

#frequency by Day of Cycle
cycle_long <- melt(countDF,
      # ID variables - all the variables to keep but not split apart on
      id.vars=c("Date", "Day.of.Cycle"),
      # The source columns
      measure.vars=c("Eggs", "Sperm", "DisEgg", "Embryos", "Larvae" ),
      # Name of the destination column that will identify the original column that the measurement came from
      variable.name="Spawn_type",
      value.name="Count")

pdf(paste(IncubatorName, "bycycle.pdf", sep = "_"))

ggplot(data=cycle_long, aes(x=Day.of.Cycle, y=Count, fill=Spawn_type)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(1:30)) +
  ggtitle(paste(IncubatorName, "Spawning Frequency", sep = " "))

dev.off()
  

#frequency by calendar date

pdf(paste(IncubatorName, "byDate.pdf", sep = "_"))

ggplot(data=cycle_long, aes(x=Date, y=Count, fill=Spawn_type)) + 
  geom_bar(stat = "identity") +
  scale_x_date(date_breaks="month", date_labels=("%b"))+
  ggtitle(paste(IncubatorName, "Spawning Frequency", sep = " "))

dev.off()
##failed attempt to have two x-axis labels  
#two x-axis labels strategy: combine the two with a line break in a single set of labels
  l <- with(cycle_long, paste(Date, Day.of.Cycle, sep = "\n")) 



