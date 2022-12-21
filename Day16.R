

dat <- readLines("data/Day16.txt")

library(dplyr)
library(tidyr)

df <- data.frame(raw= dat) %>%
  mutate(raw = sub("Valve ","",raw)) %>%
  mutate(raw = sub(" has flow rate=",";", raw)) %>%
  mutate(raw = sub(" tunnel[s]* lead[s]* to valve[s]* ", "", raw)) %>%
  mutate(raw = gsub(" ","",raw)) %>%
  separate(col=raw, into=c("valve","flow_rate","leads_to"), sep=";", convert = TRUE)

df

best <- 0

## Part 1

choose <- function(current_loc = "AA",
                   came_from = "-",
                   moves_so_far = 0,
                   open_valves = character(0),
                   flow_rate = 0,
                   total_flow = 0,
                   sequence = "AA") {
  
  # These too bits to attempt to avoid going down inefficient branches. May need to tune!
  if (moves_so_far == 5 & total_flow == 0)
    return()
  if (moves_so_far == 20 & total_flow < 600)
    return()
  
  if (moves_so_far == 30)
  {
    if (total_flow > best) {
      print(paste(total_flow, paste(sequence, collapse=",")))
      best <<- total_flow
    }
    return()
  }
  
  # ---
  
  
  for (mov in strsplit(df$leads_to[df$valve == current_loc], ",")[[1]])
  {
    if (!(mov %in% came_from) &
        !(came_from[length(came_from)] %in% strsplit(df$leads_to[df$valve == mov],",")[[1]]))   ## to avoid going in little triangles to get from A to B!
    {
      choose(came_from = c(came_from, current_loc),  ## any places visited since last value open would be wasteful
             current_loc = mov,
             moves_so_far = moves_so_far + 1,
             open_valves = open_valves,
             total_flow = total_flow + flow_rate,
             flow_rate = flow_rate,
             sequence = c(sequence, mov))
    }
  }
  
  if (!(current_loc %in% open_valves) &
      df$flow_rate[df$valve == current_loc] > 0)
  {
    choose(current_loc = current_loc,
           came_from = "-", # legitimate to go back after opening, but reset
           moves_so_far = moves_so_far + 1,
           open_values <- c(open_valves, current_loc),
           total_flow = total_flow + flow_rate,
           flow_rate = flow_rate + df$flow_rate[df$valve == current_loc],
           sequence = c(sequence, paste("open",current_loc)))
  }
  
}

best <- 0
system.time(choose())
print("1651 AA,DD,open DD,CC,BB,open BB,AA,II,JJ,open JJ,II,AA,DD,EE,FF,GG,HH,open HH,GG,FF,EE,open EE,DD,CC,open CC,BB,AA,DD,EE,FF,GG")  # test results
