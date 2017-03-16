#!/usr/bin/env Rscript

#Use data file "loz-top3.tsv" with this R script

graph_title = "The Legend of Zelda Any% Top 3 Competition"
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
ftable <- table %>% select(format_time,date,player) %>% mutate(fdate = mdy(date)) #objectify date
ftable$format_time <- as.POSIXct(ftable$format_time, format = "%H:%M:%OS") #must convert to datetime, even though we only use hms

#top 3
lack <- subset(ftable, player=="LackAttack24")
darkwing <- subset(ftable, player=="Darkwing_Duck_sda")
cantaloupe <- subset(ftable, player=="cantaloupeme")

tas <- data.frame(format_time=as.POSIXct("0:22:17.53",format="%H:%M:%OS"),fdate=mdy("09/29/2016"),player="Lord Tom (TAS)",feature="TAS")

graph <- ggplot() +
      	 #lack
      	 geom_line(data=lack, mapping = aes(x = fdate, y = format_time),linetype=1) +
	 geom_point(data=lack, mapping = aes(x = fdate, y = format_time,shape=player),size=1) +
	 #darkwing
      	 geom_line(data=darkwing, mapping = aes(x = fdate, y = format_time),linetype=2) +
	 geom_point(data=darkwing, mapping = aes(x = fdate, y = format_time,shape=player),size=1) +
	 #cantaloupe
      	 geom_line(data=cantaloupe, mapping = aes(x = fdate, y = format_time),linetype=3) +
	 geom_point(data=cantaloupe, mapping = aes(x = fdate, y = format_time,shape=player),size=1) +
	 #tas
#	 geom_point(data=tas,aes(x=fdate,y=format_time,shape=player),size=3) +

	 scale_shape_manual(values=c(16,17,15,10)) +
	 labs(shape="") +
	 ggtitle(graph_title) +
	 theme(plot.title = element_text(hjust = 0.5)) +
	 xlab("") +
	 ylab("Personal Best Time") +
	 theme(axis.text=element_text(size=14)) + #tick label size
	 theme(plot.title=element_text(size=18)) + #plot title size
	 theme(axis.title=element_text(size=16)) + #axis label size
	 theme(axis.title.y=element_text(margin=margin(0,15,0,0))) + #increase margin for y-axis label
	 theme(axis.title.x=element_text(margin=margin(15,0,0,0))) + #increase margin for x-axis label
	 theme(plot.margin=unit(c(1,1,1,1),"cm")) + #increase margin size around whole graph
#	 geom_text_repel(data=subset(ftable, player=="Zoast"), aes(fdate,format_time,label=player), size=3) + #label all points matching player
#	 geom_text_repel(data=subset(ftable, date=="9/2/2016"), aes(fdate,format_time,label=player),size=4,nudge_y=100,point.padding=unit(0.25,'lines'),box.padding=unit(2,'lines')) + #label all points matching date
#	 geom_vline(xintercept=as.numeric(mdy("10/19/2014")), linetype="dashed") + #how to make a dashed vline
#	 geom_hline(yintercept=as.numeric(as.POSIXct("2017-01-14 00:44:29")),linetype="dashed") + #how to make a dashed hline
	 scale_y_datetime(date_labels = "%Mm") #y axis labels for runs < 1hr
#	 scale_y_datetime(date_labels = "%H:%M") #y axis labels for runs with hours
#	 scale_y_datetime(date_labels = "%M\'%S\"") 

print(graph)

dev.off()