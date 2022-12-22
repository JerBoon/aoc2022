

dat <- read.delim("Data/Day21.txt", sep=":", strip.white=TRUE, header=FALSE, col.names = c("monkey","job"))


## Part 1

# Feels like this is fairly straightforward. 
# Do a string substitution with any monkeys whose values is known, evaluate any
# sums, then iterate until nothing is left..?


df <- dat

calced <- grep("^[-]*[0123456789]+$",df$job)

while (length(calced) > 0) {
  for (i in calced) {
    df$job <- sub(df$monkey[i],df$job[i], df$job)
  }
  
  # Which rows can now be calculated? Is the new "calced"
  calced <- which((grepl(" ", df$job) & !grepl("[a-z]",df$job)))
  for (r in calced)
  {
    df$job[r] <- eval(parse(text=df$job[r]))
  }
}

df$job[df$monkey=="root"]


## Part 2
# Oh dear, I thought part 1 seemed a bit too easy.... :]


