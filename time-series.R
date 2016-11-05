#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

# input output
if (length(args) < 2) {
  stop("Provide input output", call.=FALSE)
}

library(tidyverse)
library(lubridate)
options(digits.secs=3)

pdf(args[2])

table <- read.table(args[1], sep = "\t", header = TRUE)
ftable <- table %>% select(format_time,date,player) %>% mutate(fdate = mdy(date)) #objectify date
ftable$format_time <- as.POSIXct(ftable$format_time, format = "%H:%M:%OS") #must convert to date, even though we only use hms
graph <- ggplot(data = ftable) + geom_point(mapping = aes(x = fdate, y = format_time, color = player))

print(graph)

dev.off()