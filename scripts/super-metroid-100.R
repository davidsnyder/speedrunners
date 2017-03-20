#!/usr/bin/env Rscript

graph_title = "Super Metroid 100% Items Top 6 Competition"
citation_url = "Sources: www.deanyd.net/sm, www.speedrun.com, Twitch"
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
ftable <- table %>% select(player,format_time,date) %>% mutate(fdate = mdy(date)) #objectify date
ftable$format_time <- as.POSIXct(ftable$format_time, format = "%H:%M:%OS") #must convert to datetime, even though we only use hms

#top 6
zoast <- subset(ftable, player=="zoast")
kottpower <- subset(ftable, player=="Kottpower")
wildanaconda69 <- subset(ftable, player=="WildAnaconda69")
jack879 <- subset(ftable, player=="Jack879")
behemoth <- subset(ftable, player=="Behemoth")
oatsngoats <- subset(ftable, player=="oatsngoats")

graph <- ggplot(data = ftable) +
      	 #zoast
      	 geom_line(data=zoast, mapping = aes(x = fdate, y = format_time),linetype=5) +
	 geom_point(data=zoast, mapping = aes(x = fdate, y = format_time,shape=player),size=2) +
	 #kottpower
      	 geom_line(data=kottpower, mapping = aes(x = fdate, y = format_time),linetype=2) +
	 geom_point(data=kottpower, mapping = aes(x = fdate, y = format_time,shape=player),size=2) +
	 #oatsngoats
      	 geom_line(data=oatsngoats, mapping = aes(x = fdate, y = format_time),linetype=3) +
	 geom_point(data=oatsngoats, mapping = aes(x = fdate, y = format_time,shape=player),size=2) +
	 #behemoth
      	 geom_line(data=behemoth, mapping = aes(x = fdate, y = format_time),linetype=4) +
	 geom_point(data=behemoth, mapping = aes(x = fdate, y = format_time,shape=player),size=2) +
	 #jack879
      	 geom_line(data=jack879, mapping = aes(x = fdate, y = format_time),linetype=1) +
	 geom_point(data=jack879, mapping = aes(x = fdate, y = format_time,shape=player),size=2) +
	 #wildanaconda69
      	 geom_line(data=wildanaconda69, mapping = aes(x = fdate, y = format_time),linetype=6) +
	 geom_point(data=wildanaconda69, mapping = aes(x = fdate, y = format_time,shape=player),size=2) +

	 scale_shape_manual(values=c(17,10,15,2,16,0)) +
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
#	 geom_text_repel(data=subset(ftable, date=="8/28/2014"), aes(fdate,format_time,label="PRKD"),size=4,nudge_y=100,point.padding=unit(0.25,'lines'),box.padding=unit(2,'lines')) + #label all points matching date
#	 geom_vline(xintercept=as.numeric(mdy("10/19/2014")), linetype="dashed") + #how to make a dashed vline
#	 geom_hline(yintercept=as.numeric(as.POSIXct("2017-01-14 00:44:29")),linetype="dashed") + #how to make a dashed hline
	 scale_y_datetime(date_labels = "%Hh%Mm") + #y axis labels for runs with hours
	 scale_x_date(labels = scales::date_format("%b %Y"),breaks = scales::date_breaks("2 months")) +
	 theme(axis.text.x = element_text(angle=90,margin=margin(t=2))) +
	 coord_cartesian(xlim = as.Date(c("2016-08-01", "2017-03-10")),ylim = as.POSIXct(strptime(c("01:14","01:22"), format = "%H:%M")))
#	 scale_y_datetime(date_labels = "%M\'%S\"") 

print(graph)

dev.off()