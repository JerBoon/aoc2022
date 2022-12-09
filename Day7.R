

dat <- readLines("data/Day7.txt")

library(dplyr)
library(tidyr)


# Part 1

path <- "/"
fs <- data.frame(path="//",size=0,totsize=NA)

for (log in dat) {
  if (grepl("^\\$ cd ", log)) {
    dir <- substr(log,6,nchar(log))
    if (dir == "/")
      path <- "/"
    else if (dir == "..")
      path <- path[1:(length(path)-1)]
    else {
      path <- c(path,dir)
    }
  } else if (grepl("^dir ",log)) {
    dirname <- substr(log,5,nchar(log))
    fs <- rbind(fs,data.frame(path=paste0(paste(path,sep="",collapse="/"),"/",dirname,"/"),size=0,totsize=NA))
  } else if (grepl("^[0-9]",log)) {
    filename <- sub("^[0-9]* ","",log)
    size <- as.integer(sub(" .*$","",log))
    fs <- rbind(fs,data.frame(path=paste0(paste(path,sep="",collapse="/"),"/",filename),size=size,totsize=NA))
  }
}

for (eachdir in which(grepl("/$",fs$path))) {
  fs$totsize[eachdir] <- sum(fs$size[grep(paste0("^",fs$path[eachdir]),fs$path)])
}

sum(fs$totsize[fs$totsize <= 100000 & !is.na(fs$totsize)])


fs %>%
  filter(totsize >= 30000000 - (70000000 - fs$totsize[1])) %>%
  slice_min(n=1, order_by=totsize)
