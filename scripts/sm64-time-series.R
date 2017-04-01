#!/usr/bin/env Rscript

#use with file "data/sm64-120-filtered.tsv"

#graph_title = "Super Mario 64 '120 Star' World Record Progression"
#citation_url = "Sources: www.speedrun.com, https://goo.gl/ak1OaA"
graph_title = ""
citation_url = ""

#https://docs.google.com/spreadsheets/u/1/d/1HwLF6aB40Q3NgGh36G6whFtMVUF0rz0uEs9ya-kDlOA/pubhtml#
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

tas <- data.frame(format_time=as.POSIXct("1:20:41.52",format="%H:%M:%OS"),fdate=mdy("12/13/2012"),player="MKDasher,Nahoc,sonicpacker")

table <- read.table(args[1], sep = "\t", header = TRUE)
ftable <- table %>% select(format_time,date,player) %>% mutate(fdate = mdy(date)) #objectify date
ftable$format_time <- as.POSIXct(ftable$format_time, format = "%H:%M:%OS") #must convert to datetime, even though we only use hms
graph <- ggplot(data = ftable) +
     geom_line(mapping = aes(x = fdate, y = format_time)) +
	 geom_point(mapping = aes(x = fdate, y = format_time,shape=player),size=3.5) +
#	 geom_point(data=tas,aes(x=fdate,y=format_time,shape=player),size=3) +
	 scale_shape_manual(values=c(10,0,17,15,1,16,2)) +
	 labs(shape="",caption=citation_url) +
	 ggtitle(graph_title) +
	 theme(plot.title = element_text(hjust = 0.5)) +
	 xlab("") +
	 ylab("Record Time") +
	 theme(legend.text=element_text(size=14)) +
	 theme(legend.key.size = unit(1.5, 'lines')) +
	 theme(legend.position= "bottom") +
	 theme(axis.text=element_text(size=14)) + #tick label size
	 theme(plot.title=element_text(size=18)) + #plot title size
	 theme(axis.title=element_text(size=16)) + #axis label size
	 theme(axis.title.y=element_text(margin=margin(0,15,0,0))) + #increase margin for y-axis label
	 theme(plot.margin=unit(c(0,.5,0,.5),"cm")) + #increase margin size around whole graph (t,r,b,l)
#	 geom_text_repel(data=subset(ftable, player=="Zoast"), aes(fdate,format_time,label=player), size=3) + #label all points matching player
#	 geom_text_repel(data=tas, aes(fdate,format_time,label="TAS"), size=4,box.padding=unit(2,'lines'),point.padding=unit(0.75,'lines'),nudge_x=-10) +
#	 geom_text_repel(data=subset(ftable, feature!=""), aes(fdate,format_time,label=feature),size=4,point.padding=unit(0.75,'lines'),box.padding=unit(2,'lines')) + #label all points with "feature" column
#	 geom_vline(xintercept=as.numeric(mdy("10/19/2014")), linetype="dashed") + #how to make a dashed vline
#	 geom_hline(yintercept=as.numeric(as.POSIXct("2017-01-20 00:4:29")),linetype="dashed") + #how to make a dashed hline
	 scale_y_datetime(date_labels = "%Hh%Mm") 
#	 scale_y_datetime(date_labels = "%M\'%S\"") 

print(graph)

dev.off()