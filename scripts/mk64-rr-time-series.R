#!/usr/bin/env Rscript

input_filename1 <- "mk64_rr_3lap_ns.tsv"
input_filename2 <- "mk64_rr_3lap_shortcut.tsv"
output_filename <- "mk64_rr.pdf"
output_title <- "Mario Kart 64 (N64) Rainbow Road 3lap Records"

library(tidyverse)
library(lubridate)
options(digits.secs=3)

pdf(output_filename)

#non shortcut
table <- read.table(input_filename1, sep = "\t", header = TRUE)
ftable <- table %>% select(format_time,date,player) %>% mutate(fdate = mdy(date)) #objectify date
ftable$format_time <- as.POSIXct(ftable$format_time, format = "%H:%M:%OS") #must convert to datetime, even though we only use hms

#shortcut
table2 <- read.table(input_filename2, sep = "\t", header = TRUE)
ftable2 <- table2 %>% select(format_time,date,player) %>% mutate(fdate = mdy(date)) #objectify date
ftable2$format_time <- as.POSIXct(ftable2$format_time, format = "%H:%M:%OS") #must convert to datetime, even though we only use hms

graph <- ggplot() +
      	 geom_line(data = ftable, mapping = aes(x = fdate, y = format_time)) +
      	 geom_line(data = ftable2, mapping = aes(x = fdate, y = format_time)) +	 
	 scale_y_datetime(date_labels = "%M\'%S\"") +
	 labs(title = output_title)
	 

print(graph)

dev.off()