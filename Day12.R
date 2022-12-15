


dat <- readLines("data/Day12.txt")

width = nchar(dat[1])
height=length(dat)

mat <- matrix(strsplit(paste(dat,collapse=""),"")[[1]], nrow=height,ncol=width ,byrow=TRUE)

# Grab the start and end points, and map the data back to heihgt

start.x <- which(mat=="S", arr.ind=TRUE)[2]
start.y <- which(mat=="S", arr.ind=TRUE)[1]
end.x <- which(mat=="E", arr.ind=TRUE)[2]
end.y <- which(mat=="E", arr.ind=TRUE)[1]
mat[start.y,start.x] <- "a"
mat[end.y,end.x] <- "z"

mat



## Part 1 - using Dijkstra's algorithm?

# dist <- matrix(Inf, nrow=height, ncol=width)
# dist[start.y, start.x] <- 0
# dist
# 
# # to.calc is just a list of points to recalculate next cycle
# to.calc <- list(c(start.x,start.y))
# 
# # If any points to process..
# while (length(to.calc) > 0) {
#   #print(to.calc)
#   next.calc <- list()
#   
#   # calculate adjacent point for each 1 in the list
#   for (i in 1:length(to.calc)) {
#     # Look right
#     if (to.calc[[i]][1] < width)
#       if (which(mat[to.calc[[i]][2], to.calc[[i]][1]+1] == letters) - which(mat[to.calc[[i]][2], to.calc[[i]][1]] == letters) <= 1 &
#           dist[to.calc[[i]][2], to.calc[[i]][1]+1] > (dist[to.calc[[i]][2], to.calc[[i]][1]] + 1)) {
#         dist[to.calc[[i]][2], to.calc[[i]][1]+1] <- dist[to.calc[[i]][2], to.calc[[i]][1]] + 1
#         next.calc <- append(next.calc, list(c(to.calc[[i]][1]+1, to.calc[[i]][2])))
#       }
# 
#     # Look left
#     if (to.calc[[i]][1] > 1)
#       if (which(mat[to.calc[[i]][2], to.calc[[i]][1]-1] == letters) - which(mat[to.calc[[i]][2], to.calc[[i]][1]] == letters) <= 1 &
#           dist[to.calc[[i]][2], to.calc[[i]][1]-1] > (dist[to.calc[[i]][2], to.calc[[i]][1]] + 1)) {
#         dist[to.calc[[i]][2], to.calc[[i]][1]-1] <- dist[to.calc[[i]][2], to.calc[[i]][1]] + 1
#         next.calc <- append(next.calc, list(c(to.calc[[i]][1]-1, to.calc[[i]][2])))
#       }
#     
#     # Look down
#     if (to.calc[[i]][2] < height)
#       if (which(mat[to.calc[[i]][2]+1, to.calc[[i]][1]] == letters) - which(mat[to.calc[[i]][2], to.calc[[i]][1]] == letters) <= 1 &
#           dist[to.calc[[i]][2]+1, to.calc[[i]][1]] > (dist[to.calc[[i]][2], to.calc[[i]][1]] + 1)) {
#         dist[to.calc[[i]][2]+1, to.calc[[i]][1]] <- dist[to.calc[[i]][2], to.calc[[i]][1]] + 1
#         next.calc <- append(next.calc, list(c(to.calc[[i]][1], to.calc[[i]][2]+1)))
#       }
#     
#     # Look up
#     if (to.calc[[i]][2] >1)
#       if (which(mat[to.calc[[i]][2]-1, to.calc[[i]][1]] == letters) - which(mat[to.calc[[i]][2], to.calc[[i]][1]] == letters) <= 1 &
#           dist[to.calc[[i]][2]-1, to.calc[[i]][1]] > (dist[to.calc[[i]][2], to.calc[[i]][1]] + 1)) {
#         dist[to.calc[[i]][2]-1, to.calc[[i]][1]] <- dist[to.calc[[i]][2], to.calc[[i]][1]] + 1
#         next.calc <- append(next.calc, list(c(to.calc[[i]][1], to.calc[[i]][2]-1)))
#       }
#     
#   }
#   
#   to.calc <-  next.calc
#   #print(dist)
# }
# 
# print(dist[end.y,end.x])



## Part 2 Actually, if I calculate distances in reverse (from top)
## Should be able to give us both!

dist <- matrix(Inf, nrow=height, ncol=width)
dist[end.y, end.x] <- 0
dist

# to.calc is just a list of points to recalculate next cycle
to.calc <- list(c(end.x,end.y))

# If any points to process..
while (length(to.calc) > 0) {
  #print(to.calc)
  next.calc <- list()
  
  # calculate adjacent point for each 1 in the list
  for (i in 1:length(to.calc)) {
    # Look right
    if (to.calc[[i]][1] < width)
      if (which(mat[to.calc[[i]][2], to.calc[[i]][1]+1] == letters) - which(mat[to.calc[[i]][2], to.calc[[i]][1]] == letters) >= -1 &
          dist[to.calc[[i]][2], to.calc[[i]][1]+1] > (dist[to.calc[[i]][2], to.calc[[i]][1]] + 1)) {
        dist[to.calc[[i]][2], to.calc[[i]][1]+1] <- dist[to.calc[[i]][2], to.calc[[i]][1]] + 1
        next.calc <- append(next.calc, list(c(to.calc[[i]][1]+1, to.calc[[i]][2])))
      }
    
    # Look left
    if (to.calc[[i]][1] > 1)
      if (which(mat[to.calc[[i]][2], to.calc[[i]][1]-1] == letters) - which(mat[to.calc[[i]][2], to.calc[[i]][1]] == letters) >= -1 &
          dist[to.calc[[i]][2], to.calc[[i]][1]-1] > (dist[to.calc[[i]][2], to.calc[[i]][1]] + 1)) {
        dist[to.calc[[i]][2], to.calc[[i]][1]-1] <- dist[to.calc[[i]][2], to.calc[[i]][1]] + 1
        next.calc <- append(next.calc, list(c(to.calc[[i]][1]-1, to.calc[[i]][2])))
      }
    
    # Look down
    if (to.calc[[i]][2] < height)
      if (which(mat[to.calc[[i]][2]+1, to.calc[[i]][1]] == letters) - which(mat[to.calc[[i]][2], to.calc[[i]][1]] == letters) >= -1 &
          dist[to.calc[[i]][2]+1, to.calc[[i]][1]] > (dist[to.calc[[i]][2], to.calc[[i]][1]] + 1)) {
        dist[to.calc[[i]][2]+1, to.calc[[i]][1]] <- dist[to.calc[[i]][2], to.calc[[i]][1]] + 1
        next.calc <- append(next.calc, list(c(to.calc[[i]][1], to.calc[[i]][2]+1)))
      }
    
    # Look up
    if (to.calc[[i]][2] >1)
      if (which(mat[to.calc[[i]][2]-1, to.calc[[i]][1]] == letters) - which(mat[to.calc[[i]][2], to.calc[[i]][1]] == letters) >= -1 &
          dist[to.calc[[i]][2]-1, to.calc[[i]][1]] > (dist[to.calc[[i]][2], to.calc[[i]][1]] + 1)) {
        dist[to.calc[[i]][2]-1, to.calc[[i]][1]] <- dist[to.calc[[i]][2], to.calc[[i]][1]] + 1
        next.calc <- append(next.calc, list(c(to.calc[[i]][1], to.calc[[i]][2]-1)))
      }
    
  }
  
  to.calc <-  next.calc
  #print(dist)
}

# Part 1 answer
print(dist[start.y,start.x])

# part 2 answer

min(dist[dist < Inf & mat=="a"])

