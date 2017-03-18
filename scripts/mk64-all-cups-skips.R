#!/usr/bin/env Rscript

graph_title = "Mario Kart 64 All Cups World Record Progression"
graph_subtitle = "NTSC 150cc Skips"
args = commandArgs(trailingOnly=TRUE)

# ./scripts/mk64-all-cups-skips.R data/mario-kart-64/all-cups-skips.tsv pdf/all-cups-skips.pdf

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
ftable <- table %>% select(format_time,player,date,notes) %>% mutate(fdate = mdy(date)) #objectify date
ftable$format_time <- as.POSIXct(ftable$format_time, format = "%H:%M:%OS") #must convert to datetime, even though we only use hms

selected <- subset(ftable, (date!="8/16/2012" & date!="1/27/2012"))

graph <- ggplot() +
      	 geom_line(ftable,mapping = aes(x = fdate, y = format_time)) +
	 geom_point(selected,mapping = aes(x = fdate, y = format_time,shape=player),size=2) +
	 scale_shape_manual(values=c(0,1,2,17,15,16)) +
	 labs(shape="") +
	 ggtitle(graph_title,graph_subtitle) +
	 theme(plot.title = element_text(hjust = 0.5)) +
	 theme(plot.subtitle = element_text(hjust = 0.5)) +	 
	 xlab("") +
	 ylab("Record Time") +
	 theme(axis.text=element_text(size=14)) + #tick label size
	 theme(plot.title=element_text(size=14)) + #plot title size
	 theme(axis.title=element_text(size=16)) + #axis label size
	 theme(axis.title.y=element_text(margin=margin(0,15,0,0))) + #increase margin for y-axis label
	 theme(axis.title.x=element_text(margin=margin(15,0,0,0))) + #increase margin for x-axis label
	 theme(plot.margin=unit(c(1,1,1,1),"cm")) + #increase margin size around whole graph
#	 geom_text_repel(data=subset(ftable, player=="Zoast"), aes(fdate,format_time,label=player), size=3) + #label all points matching player
	 geom_text_repel(data=ftable, aes(fdate,format_time,label=notes),size=2.5,point.padding=unit(0.8,'lines'),nudge_x=-20,box.padding=unit(1,'lines')) + #label all points matching date
#	 geom_text_repel(data=subset(ftable, date=="6/6/2012"), aes(fdate,format_time,label="Tie with Hotarubi"),size=4,nudge_x=200,nudge_y=100,point.padding=unit(0.25,'lines'),box.padding=unit(2,'lines')) + #label all points matching date	 
	 geom_vline(xintercept=as.numeric(mdy("5/5/2016")), linetype="dotted",alpha=0.6) +
         geom_text(data=data.frame(x=as.Date(mdy("5/5/2016")),y=as.POSIXct("0:31:3l.00",format="%H:%M:%OS")),aes(x=x,y=y,label="NTSC-J Switch"),hjust=1.05,size=3) +
#	 geom_hline(yintercept=as.numeric(as.POSIXct("2017-01-14 00:44:29")),linetype="dashed") + #how to make a dashed hline
	 scale_y_datetime(date_labels = "%Mm",breaks = scales::pretty_breaks(n = 8)) +
#	 scale_y_datetime(limits=as.POSIXct(strptime(c("02:04","02:09"), format = "%M:%S")), date_labels = "%Mm%Ss",breaks = scales::pretty_breaks(n = 8)) 
	 coord_cartesian(xlim = as.Date(c("2012-08-01", "2017-03-01")),ylim = as.POSIXct(strptime(c("0:25:30","0:32:30"), format = "%H:%M:%S")))
#	 scale_y_datetime(date_labels = "%M\'%S\"") 

print(graph)

dev.off()