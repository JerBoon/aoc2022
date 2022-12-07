

dat <- readLines("data/Day5.txt")

library(dplyr)
library(tidyr)

moves <- dat[grep("^move",dat)]

stack.txt <- dat[grep("\\[",dat)]


## Build stack as list of vectors

{
stack <- list()
for (j in 1:((nchar(stack.txt[1])+1)/4))
  stack[[j]] <- as.character(NULL)
  
for(i in length(stack.txt):1)
{
  for (j in 1:((nchar(stack.txt[1])+1)/4))
    if(substr(stack.txt[i],j*4-2,j*4-2) != " ")
      stack[[j]] <-c(stack[[j]],substr(stack.txt[i],j*4-2,j*4-2))
}
}

stack.copy <- stack


## Part 1

for (i in 1:length(moves)) {
  nums <- unlist(strsplit(moves[i],"[^0-9]+"))
  n <- as.integer(nums[2])
  from <- as.integer(nums[3])
  to <- as.integer(nums[4])
  while (n > 0)
  {
    n <- n - 1
    stack[[to]] <- c(stack[[to]],stack[[from]][length(stack[[from]])])
    stack[[from]] <- stack[[from]][-length(stack[[from]])]
  }
}


for(i in 1:length(stack)) {
  cat(stack[[i]][length(stack[[i]])])
}

## Part 2

# N.B. Start with original stack configuration
stack <- stack.copy

for (i in 1:length(moves)) {
  nums <- unlist(strsplit(moves[i],"[^0-9]+"))
  n <- as.integer(nums[2])
  from <- as.integer(nums[3])
  to <- as.integer(nums[4])
  
  stack[[to]] <- c(stack[[to]],stack[[from]][(length(stack[[from]])-n+1):length(stack[[from]])])
  stack[[from]] <- stack[[from]][1:(length(stack[[from]])-n)]
}


for(i in 1:length(stack)) {
  cat(stack[[i]][length(stack[[i]])])
}
