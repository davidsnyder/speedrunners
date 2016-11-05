library(tidyverse)
library(lubridate)

pdf('graph.pdf')

loz <- read.table("loz_times.tsv",header = TRUE)
floz <- loz %>% select(format_time,date,player) %>% mutate(fdate = mdy(date)) #objectify date
floz$format_time <- as.POSIXct(floz$format_time, format = "%H:%M:%S") #must convert to date, even though we only use hms
graph <- ggplot(data = floz) + geom_point(mapping = aes(x = fdate, y = format_time, color = player))

print(graph)

dev.off()