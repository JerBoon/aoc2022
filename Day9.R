

dat <- readLines("data/Day9.txt")

library(dplyr)
library(tidyr)



## Part 1

head.x <- 0
head.y <- 0
tail.x <- 0
tail.y <- 0
positions <- data.frame(x=0, y=0)

for (each in dat) {

  direction <- substr(each,1,1)
  steps <- as.integer(substr(each,3,nchar(each)))

  while (steps > 0)   {
    steps <- steps - 1
    
    # Move head
    if (direction == "U") head.y <- head.y + 1
    else if (direction == "D") head.y <- head.y - 1
    else if (direction == "L") head.x <- head.x - 1
    else if (direction == "R") head.x <- head.x + 1
    
    # Move tail
    if (abs(tail.x-head.x) > 1 | abs(tail.y-head.y) > 1) {
      tail.x <- tail.x + sign(head.x  - tail.x)
      tail.y <- tail.y + sign(head.y  - tail.y)
    }

    #print(c(direction, head.x,head.y, tail.x,tail.y))
    positions <- rbind(positions, data.frame(x=tail.x, y=tail.y))
  }

  
}

unique(positions)
nrow(unique(positions))


## Part 2

rope.x <- rep(0,10)
rope.y <- rep(0,10)
positions <- data.frame(x=0, y=0)

for (each in dat) {
  
  direction <- substr(each,1,1)
  steps <- as.integer(substr(each,3,nchar(each)))
  
  while (steps > 0)   {
    steps <- steps - 1
    
    # Move head
    if (direction == "U") rope.y[1] <- rope.y[1] + 1
    else if (direction == "D") rope.y[1] <- rope.y[1] - 1
    else if (direction == "L") rope.x[1] <- rope.x[1] - 1
    else if (direction == "R") rope.x[1] <- rope.x[1] + 1
    
    # Move tails
    for (i in 2:10) {
      if (abs(rope.x[i]-rope.x[i-1]) > 1 | abs(rope.y[i]-rope.y[i-1]) > 1) {
        rope.x[i] <- rope.x[i] + sign(rope.x[i-1]  - rope.x[i])
        rope.y[i] <- rope.y[i] + sign(rope.y[i-1]  - rope.y[i])
      }
    }
    
    #print(c(direction, head.x,head.y, tail.x,tail.y))
    positions <- rbind(positions, data.frame(x=rope.x[10], y=rope.y[10]))
  }
  
  
}

unique(positions)
nrow(unique(positions))
