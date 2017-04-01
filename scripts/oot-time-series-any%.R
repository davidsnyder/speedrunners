#!/usr/bin/env Rscript

#Use data file "oot-any%.tsv" with this R script

#graph_title = "The LoZ: Ocarina of Time Any% World Record Progression"
#citation_url = "Sources: www.speedrun.com,www.zeldaspeedruns.com, https://goo.gl/jvNMJW"
graph_title = ""
citation_url = ""

# https://docs.google.com/spreadsheets/d/1DgefbvS3X4geDxfwdE-K6CnGvNcDwZSmiZz_u2KGDcI/edit#gid=0
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
ftable <- table %>% select(format_time,player,date,version,note,other_note) %>% mutate(fdate = mdy(date)) #objectify date
ftable$format_time <- as.POSIXct(ftable$format_time, format = "%H:%M:%OS") #must convert to datetime, even though we only use hms

zfg <- subset(ftable, player=="ZFG")
first <- subset(ftable,date=="12/25/2016")
ftable_points <- subset(ftable, player!="ZFG")

tas <- data.frame(format_time=as.POSIXct("0:16:57.69",format="%H:%M:%OS"),fdate=mdy("12/31/2013"),player="Bloobiebla & MrGrunz",feature="TAS")

graph <- ggplot() +

     geom_line(data=ftable,mapping = aes(x = fdate, y = format_time),linetype=1) +
	 geom_point(data=ftable_points,mapping = aes(x = fdate, y = format_time),size=1.5) +
	 #zfg
	 geom_point(data=zfg,mapping = aes(x = fdate, y = format_time,shape=player),size=2.5) +
	 #first place
#	 geom_point(data=first,mapping = aes(x = fdate, y = format_time,shape=player),size=2) +

	 #tas
	 geom_point(data=tas,aes(x=fdate,y=format_time,shape=player),size=2) +

	 scale_shape_manual(values=c(15,2)) +
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
	 #label TAS
	 geom_text_repel(data=tas, aes(fdate,format_time,label=feature), size=4.5,nudge_y=-300,nudge_x=-100,point.padding=unit(0.5,'lines'),box.padding=unit(2,'lines')) +
	 geom_text_repel(data=subset(ftable, note!=""), aes(fdate,format_time,label=note),size=4.5,nudge_y=600,nudge_x=700,point.padding=unit(0.5,'lines'),box.padding=unit(2,'lines')) + #label all points matching date
#	 geom_vline(xintercept=as.numeric(mdy("10/19/2014")), linetype="dashed") + #how to make a dashed vline
#	 geom_hline(yintercept=as.numeric(as.POSIXct("2017-01-14 00:44:29")),linetype="dashed") + #how to make a dashed hline
#	 scale_y_datetime(date_labels = "%Mm") #y axis labels for runs < 1hr
	 #limits sets the Y Axis to start at zero
	 scale_y_datetime(limits=as.POSIXct(strptime(c("00:00","01:15"), format = "%H:%M")), date_labels = "%Hh%Mm",breaks = scales::pretty_breaks(n = 6)) + #y axis labels
#	 coord_cartesian(xlim = as.Date(c("2011-01-01", "2017-01-01")),ylim = as.POSIXct(strptime(c("00:00","00:59"), format = "%H:%M")))
	 coord_cartesian(xlim = as.Date(c("2011-01-01", "2017-03-25")),ylim = as.POSIXct(strptime(c("00:10","01:02"), format = "%H:%M")))
#	 scale_y_datetime(date_labels = "%M\'%S\"") 

print(graph)

dev.off()