#!/usr/bin/env Rscript

#graph_title = "GTA: Vice City Any% World Record Progression"
#citation_url = "Source: www.speedrun.com"
graph_title = ""
citation_url = ""
args = commandArgs(trailingOnly=TRUE)

# input output
if (length(args) < 2) {
  stop("Provide input.tsv output.pdf", call.=FALSE)
}

library("ggrepel")
library(tidyverse)
library(lubridate)
options(digits.secs=3)

pdf(args[2],width=8)

table <- read.table(args[1], sep = "\t", header = TRUE)
ftable <- table %>% select(format_time,date,player,category) %>% mutate(fdate = mdy(date)) #objectify date
ftable$format_time <- as.POSIXct(ftable$format_time, format = "%H:%M:%OS") #must convert to datetime, even though we only use hms

graph <- ggplot(data=ftable,aes(x=fdate, y=format_time, group=category)) +
     geom_line(aes(linetype=category),size=.75) +
	 geom_point(size=3) +
	 scale_linetype_manual("Category", values=c("solid", "dotted")) +
	 theme(legend.title=element_text(size=14)) +
	 ggtitle(graph_title) +
	 theme(plot.title = element_text(hjust = 0.5)) +
	 labs(caption=citation_url) +	 
	 xlab("") +
	 ylab("Record Time") +
	 theme(legend.text=element_text(size=14)) +
	 theme(legend.key.size = unit(1.5, 'lines')) +
	 theme(axis.text=element_text(size=14)) + #tick label size
	 theme(plot.title=element_text(size=18)) + #plot title size
	 theme(axis.title=element_text(size=16)) + #axis label size
	 theme(axis.title.y=element_text(margin=margin(0,15,0,0))) + #increase margin for y-axis label
	# theme(axis.title.x=element_text(margin=margin(15,0,0,0))) + #increase margin for x-axis label
	 theme(plot.margin=unit(c(0,.5,0,.5),"cm")) + #increase margin size around whole graph (t,r,b,l)
#	 geom_text_repel(data=subset(ftable, player=="Zoast"), aes(fdate,format_time,label=player), size=3) + #label all points matching player
	 geom_text_repel(data=subset(ftable, date=="8/9/2014"), aes(fdate,format_time,label="Replays discovery"),size=6,nudge_x=75,nudge_y=125,point.padding=unit(0.75,'lines'),box.padding=unit(2,'lines')) + #label all points matching date
	 geom_text_repel(data=subset(ftable, date=="8/1/2016"), aes(fdate,format_time,label="SSU discovery"),size=6,nudge_x=-500,nudge_y=-200,point.padding=unit(0.75,'lines'),box.padding=unit(2,'lines')) + #label all points matching date
#	 geom_vline(xintercept=as.numeric(mdy("08/05/2014")), linetype="dashed") + #discovery of replays
#	 geom_vline(xintercept=as.numeric(mdy("08/01/2016")), linetype="dashed") + #discovery of SSU
#	 scale_y_datetime(date_labels = "%Mm") #y axis labels for runs < 1hr
	 scale_y_datetime(date_labels = "%Hh%Mm") #y axis labels for runs with hours
#	 scale_y_datetime(date_labels = "%M\'%S\"") 

print(graph)

dev.off()