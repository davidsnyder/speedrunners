#!/usr/bin/env Rscript

graph_title = "Super Metroid Any% World Record Progression"
args = commandArgs(trailingOnly=TRUE)

# input output
if (length(args) < 2) {
  stop("Provide input.tsv output.pdf", call.=FALSE)
}

library("ggrepel")
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
	 ggtitle(graph_title) +
	 theme(plot.title = element_text(hjust = 0.5)) +
	 xlab("Record Year") +
	 ylab("Record Time") +
	 theme(axis.text=element_text(size=14)) + #tick label size
	 theme(plot.title=element_text(size=18)) + #plot title size
	 theme(axis.title=element_text(size=16)) + #axis label size
	 theme(axis.title.y=element_text(margin=margin(0,15,0,0))) + #increase margin for y-axis label
	 theme(axis.title.x=element_text(margin=margin(15,0,0,0))) + #increase margin for x-axis label
	 theme(plot.margin=unit(c(1,1,1,1),"cm")) + #increase margin size around whole graph
#	 geom_text_repel(data=subset(ftable, player=="Zoast"), aes(fdate,format_time,label=player), size=3) + #label all points matching player
	 geom_text_repel(data=subset(ftable, date=="9/2/2016"), aes(fdate,format_time,label=player),size=4,nudge_y=100,point.padding=unit(0.25,'lines'),box.padding=unit(2,'lines')) + #label all points matching date
#	 geom_vline(xintercept=as.numeric(mdy("10/19/2014")), linetype="dashed") + #how to make a dashed vline
#	 geom_hline(yintercept=as.numeric(as.POSIXct("2017-01-14 00:44:29")),linetype="dashed") + #how to make a dashed hline
	 scale_y_datetime(date_labels = "%Mm") 
#	 scale_y_datetime(date_labels = "%M\'%S\"") 

print(graph)

dev.off()