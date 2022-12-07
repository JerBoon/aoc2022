

txt <- readLines("data/Day6.txt")

chars <- unlist(strsplit(str,split=""))

## Part 1

nchar(chars)

for (i in 4:length(chars)) {
  if (chars[i] != chars[i-1] &
      chars[i] != chars[i-2] &
      chars[i] != chars[i-3] &
      chars[i-1] != chars[i-2] &
      chars[i-1] != chars[i-3] &
      chars[i-2] != chars[i-3])
  {
    print(i)
    break
  }
}


## Part 2

# Let's rewrite that part 1 code as a function that'll work for any length of marker :)

find_marker<- function(len=4) {

  for (i in len:length(chars)) {
    match <- FALSE
    for (j in 0:(len-2))
      for (k in (j+1):(len-1))
        if (chars[i-j] == chars[i-k])
          match <- TRUE
    if (!match)
      return(i)
  }
  return(-999)
}

find_marker(4)  #recomputing part 1 answer
find_marker(14)

