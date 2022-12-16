

files <- dir(pattern="Day[0-9]+.R")

size <- numeric(0)

for (f in files) {
  
  txt <- readLines(f)
  
  txt <- sub("#.*$","",txt)
  
  sz <- sum(nchar(txt))
  
  day <- as.numeric(gsub("[^0-9]","", f))
  
  size[day] <- sz
}

size

library(dplyr)
library(ggplot2)
data.frame(day=1:length(size), size=size) %>%
  ggplot(aes(x=day,y=size)) +
  geom_line() +
  geom_point() +
  geom_smooth() + #method="lm") +
  scale_x_continuous(breaks=1:length(size)) +
  xlab("Day") + 
  ylab("Code size (bytes, excluding comments)")
