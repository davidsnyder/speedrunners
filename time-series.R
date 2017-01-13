#!/usr/bin/env Rscript

graph_title = "Super Metroid Any% World Records"
args = commandArgs(trailingOnly=TRUE)

# input output
if (length(args) < 2) {
  stop("Provide input.tsv output.pdf", call.=FALSE)
}

library(tidyverse)
library(lubridate)
options(digits.secs=3)

pdf(args[2])

table <- read.table(args[1], sep = "\t", header = TRUE)
ftable <- table %>% select(format_time,date,player) %>% mutate(fdate = mdy(date)) #objectify date
ftable$format_time <- as.POSIXct(ftable$format_time, format = "%H:%M:%OS") #must convert to datetime, even though we only use hms
graph <- ggplot(data = ftable) +
      	 geom_line(mapping = aes(x = fdate, y = format_time)) +
	 geom_point(mapping = aes(x = fdate, y = format_time)) +
	 theme(axis.title.y=element_text(margin=margin(0,15,0,0))) +
	 theme(axis.title.x=element_text(margin=margin(15,0,0,0))) +
	 theme(plot.margin=unit(c(1,1,1,1),"cm")) +
	 ggtitle(graph_title) +
	 xlab("Record Year") +
	 ylab("Record Time") +
#	 geom_text(data=subset(ftable, player=="Zoast"), aes(fdate,format_time,label=player), hjust=1,vjust=1) +
	 scale_y_datetime(date_labels = "%Mm") 
#	 scale_y_datetime(date_labels = "%M\'%S\"") 

print(graph)

dev.off()