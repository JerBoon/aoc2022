

dat <- readLines("data/Day15.txt")

library(dplyr)
library(tidyr)

df <- 
  data.frame(dat=dat) %>%
  separate(dat,
           sep="[^0-9]+",
           into=c(NA, "sensor.x", "sensor.y", "beacon.x", "beacon.y"),
           convert=TRUE) %>%
  mutate(dist=abs(sensor.x-beacon.x)+abs(sensor.y-beacon.y))

df

horiz <- function(line=10) {
  covered <- numeric(0)
  for (i in 1:nrow(df)) {
    if (abs(df$sensor.y[i] - line) < df$dist[i]) {
      covered <- c(covered,
            (df$sensor.x[i]-(abs(df$sensor.y[i] - line) - df$dist[i])):
            (df$sensor.x[i]+((abs(df$sensor.y[i] - line) - df$dist[i]))))
    }
  }
  covered <- sort(unique(covered))
  beacons <- df$beacon.x[df$beacon.y==line]
  print(beacons)
  return(length(covered[!(covered %in% beacons)]))
}

horiz()
horiz(2000000)
