#!/usr/bin/env Rscript

# use with file "data/super-metroid/super-metroid-any%-filtered.tsv"

#graph_title = "Super Metroid Any% World Record Progression"
#citation_url = "Source: www.deanyd.net/sm"
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
ftable <- table %>% select(format_time,date,player) %>% mutate(fdate = mdy(date)) #objectify date
ftable$format_time <- as.POSIXct(ftable$format_time, format = "%H:%M:%OS") #must convert to datetime, even though we only use hms

without_hotarubi <- subset(ftable, player!="Hotarubi")
without_smokey <- subset(without_hotarubi, player!="Smokey")

graph <- ggplot() +
     geom_line(data=ftable, mapping = aes(x = fdate, y = format_time)) +
	 geom_point(data=without_smokey,mapping = aes(x = fdate, y = format_time,shape=player),size=3) +
	 scale_shape_manual(values=c(0,16,2,1,17)) +
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
	 geom_text_repel(data=subset(ftable, date=="8/28/2014"), aes(fdate,format_time,label="PRKD change"),size=4.5,nudge_x=200,nudge_y=100,point.padding=unit(.75,'lines'),box.padding=unit(2,'lines')) + #label all points matching date
#	 geom_text_repel(data=subset(ftable, date=="6/6/2012"), aes(fdate,format_time,label="Tie with Hotarubi"),size=4,nudge_x=200,nudge_y=100,point.padding=unit(0.25,'lines'),box.padding=unit(2,'lines')) + #label all points matching date	 
#	 geom_vline(xintercept=as.numeric(mdy("10/19/2014")), linetype="dashed") + #how to make a dashed vline
#	 geom_hline(yintercept=as.numeric(as.POSIXct("2017-01-14 00:44:29")),linetype="dashed") + #how to make a dashed hline
	 scale_y_datetime(limits=as.POSIXct(strptime(c("00:41","00:53"), format = "%H:%M")), date_labels = "%Mm",breaks = scales::pretty_breaks(n = 10)) + #y axis labels
	 coord_cartesian(xlim = as.Date(c("2012-01-01", "2016-10-01")),ylim = as.POSIXct(strptime(c("00:42","00:50"), format = "%H:%M")))
#	 scale_y_datetime(date_labels = "%M\'%S\"") 

print(graph)

dev.off()