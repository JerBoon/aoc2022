

dat <- readLines("data/Day8.txt")

library(dplyr)
library(tidyr)

width <- nchar(dat[1])
height <- length(dat)


## Part 1

visible <- matrix(FALSE,height,width)

for (i in 1:height) {
  # Left to right
  top <- " "
  for (j in 1:width) {
    if (substr(dat[i],j,j) > top) {
      top <- substr(dat[i],j,j)
      visible[i,j] <- TRUE
    }
  }
  
  # Right to left
  top <- " "
  for (j in width:1) {
    if (substr(dat[i],j,j) > top) {
      top <- substr(dat[i],j,j)
      visible[i,j] <- TRUE
    }
  }
  
}

for (j in 1:width) {
  # Top to bottom
  top <- " "
  for (i in 1:height) {
    if (substr(dat[i],j,j) > top) {
      top <- substr(dat[i],j,j)
      visible[i,j] <- TRUE
    }
  }

  # Bottom to top
  top <- " "
  for (i in height:1) {
    if (substr(dat[i],j,j) > top) {
      top <- substr(dat[i],j,j)
      visible[i,j] <- TRUE
    }
  }
}
View(visible)

sum(visible)

# Part 2

vdist <- function(x,y) {
  
  me <- substr(dat[y],x,x)
  v.left <- 0
  v.right <- 0
  v.up <- 0
  v.down <- 0
  
  if (x > 1) {
    v.left <- x-1
    for (i in (x-1):1)
      if (substr(dat[y],i,i) >= me) {
        v.left <- x-i
        break
      }
  }
  
  if (x < width) {
    v.right <- width-x
    for (i in (x+1):width)
      if (substr(dat[y],i,i) >= me) {
        v.right <- i-x
        break
      }
  }
  
  if (y > 1) {
    v.up <- y-1
    for (i in (y-1):1)
      if (substr(dat[i],x,x) >= me) {
        v.up <- y-i
        break
      }
  }
  
  if (y < height) {
    v.down <- height-y
    for (i in (y+1):height)
      if (substr(dat[i],x,x) >= me) {
        v.down <- i-y
        break
      }
  }
  
  return(v.left*v.right*v.up*v.down)
    
}

maxv <- 0

for(i in 1:width)
  for (j in 1:height){
    vd <- vdist(i,j) 
    if (vd > maxv) {
      print(c(i,j,vd)) # printing coords and vdist of latest found biggest...
      maxv <- vd
    }
  }

print(maxv)
