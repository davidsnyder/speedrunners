#!/usr/bin/env Rscript

#Use data file "oot-100%-top3.tsv" with this R script

#graph_title = "The LoZ: Ocarina of Time 100% Top 3 Competition"
#citation_url = "Sources: www.speedrun.com, www.zeldaspeedruns.com, Twitch"
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
ftable <- table %>% select(player,format_time,date,console) %>% mutate(fdate = mdy(date)) #objectify date
ftable$format_time <- as.POSIXct(ftable$format_time, format = "%H:%M:%OS") #must convert to datetime, even though we only use hms

#top 3
clint <- subset(ftable, player=="ClintStevens")
clint_filter_list <- c("10/23/2015","10/31/2015","11/11/2015","11/27/2015")
clint_filtered <- subset(clint, !(date %in% clint_filter_list))
zfg <- subset(ftable, player=="ZFG")
sva <- subset(ftable, player=="sva")

tas <- data.frame(format_time=as.POSIXct("3:47:57.17",format="%H:%M:%OS"),fdate=mdy("07/06/2015"),player="MoronicTAS & GCTK24 (TAS)",feature="TAS")

graph <- ggplot() +
     #clint
     geom_line(data=clint, mapping = aes(x = fdate, y = format_time),linetype=2) +
	 geom_point(data=clint_filtered, mapping = aes(x = fdate, y = format_time,shape=player),size=2.2) +
	 #zfg
     geom_line(data=zfg, mapping = aes(x = fdate, y = format_time),linetype=1) +
	 geom_point(data=zfg, mapping = aes(x = fdate, y = format_time,shape=player),size=2.2) +
	 #sva
     geom_line(data=sva, mapping = aes(x = fdate, y = format_time),linetype=3) +
	 geom_point(data=sva, mapping = aes(x = fdate, y = format_time,shape=player),size=2.2) +
	 #tas
#	 geom_point(data=tas,aes(x=fdate,y=format_time,shape=player),size=2) +

	 scale_shape_manual(values=c(15,2,16)) +
	 labs(shape="",caption=citation_url) +
	 ggtitle(graph_title) +
	 theme(plot.title = element_text(hjust = 0.5)) +
	 xlab("") +
	 ylab("Personal Best Time") +
	 theme(legend.text=element_text(size=14)) +
	 theme(legend.key.size = unit(1.5, 'lines')) +
	  theme(legend.position= "bottom") +
	 theme(axis.text=element_text(size=14)) + #tick label size
	 theme(plot.title=element_text(size=18)) + #plot title size
	 theme(axis.title=element_text(size=16)) + #axis label size
	 theme(axis.title.y=element_text(margin=margin(0,15,0,0))) + #increase margin for y-axis label
	 theme(plot.margin=unit(c(0,.5,0,.5),"cm")) + #increase margin size around whole graph (t,r,b,l)
#	 geom_text_repel(data=subset(ftable, player=="Zoast"), aes(fdate,format_time,label=player), size=3) + #label all points matching player
#	 geom_text_repel(data=subset(ftable, date=="9/2/2016"), aes(fdate,format_time,label=player),size=4,nudge_y=100,point.padding=unit(0.25,'lines'),box.padding=unit(2,'lines')) + #label all points matching date
#	 geom_vline(xintercept=as.numeric(mdy("10/19/2014")), linetype="dashed") + #how to make a dashed vline
#	 geom_hline(yintercept=as.numeric(as.POSIXct("2017-01-14 00:44:29")),linetype="dashed") + #how to make a dashed hline
#	 scale_y_datetime(date_labels = "%Mm") #y axis labels for runs < 1hr
	 scale_y_datetime(date_labels = "%Hh%Mm") + #y axis labels for runs with hours
#	 scale_y_datetime(date_labels = "%M\'%S\"") 
 coord_cartesian(xlim = as.Date(c("2012-12-01", "2017-02-01")),ylim = as.POSIXct(strptime(c("04:14","05:30"), format = "%H:%M")))
print(graph)

dev.off()