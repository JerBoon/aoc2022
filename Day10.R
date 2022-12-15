



dat <- readLines("data/Day10.txt")

library(dplyr)
library(tidyr)



# Part 1

x <- 1
ss.totals <- 0
cycle <- 0
instr.cycles <- 0
instr.ptr <- 1

while(instr.ptr <= length(dat)) {
  x.add <- 0
  if (grepl("noop",dat[instr.ptr])) {
    instr.ptr <- instr.ptr + 1
    instr.cycles <- 0
  } else if (grepl("addx",dat[instr.ptr])) {
    if (instr.cycles < 1) {
      instr.cycles <- instr.cycles + 1
    } else {
      instr.cycles <- 0
      x.add <- as.integer(substr(dat[instr.ptr],6,nchar(dat[instr.ptr])))
      instr.ptr <- instr.ptr + 1
    }
  }
  cycle <- cycle + 1
  if ((cycle+20)%%40 == 0) {
    print(cycle*x)
    ss.totals <- ss.totals + cycle*x
  }
  x <- x + x.add
  x.add <- 0
}
print(ss.totals)

# Part 2

x <- 1
cycle <- 0
instr.cycles <- 0
instr.ptr <- 1

while(instr.ptr <= length(dat)) {
  x.add <- 0
  if (grepl("noop",dat[instr.ptr])) {
    instr.ptr <- instr.ptr + 1
    instr.cycles <- 0
  } else if (grepl("addx",dat[instr.ptr])) {
    if (instr.cycles < 1) {
      instr.cycles <- instr.cycles + 1
    } else {
      instr.cycles <- 0
      x.add <- as.integer(substr(dat[instr.ptr],6,nchar(dat[instr.ptr])))
      instr.ptr <- instr.ptr + 1
    }
  }
  cycle <- cycle + 1
  cat(ifelse (abs(((cycle-1)%%40)-x) <= 1, "#","."))
  if ((cycle%%40) == 0) {
    cat("\n")
  }
  x <- x + x.add
  x.add <- 0
}

