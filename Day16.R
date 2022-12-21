

dat <- readLines("data/Day16.txt")

library(dplyr)
library(tidyr)

# Bit of dplyr to simplify the dataset.

df <- data.frame(raw= dat) %>%
  mutate(raw = sub("Valve ","",raw)) %>%
  mutate(raw = sub(" has flow rate=",";", raw)) %>%
  mutate(raw = sub(" tunnel[s]* lead[s]* to valve[s]* ", "", raw)) %>%
  mutate(raw = gsub(" ","",raw)) %>%
  separate(col=raw, into=c("valve","flow_rate","leads_to"), sep=";", convert = TRUE)

df

best <- 0


## Part 1


# Is basically just a recursive function which explores a tree of every possible choice...
# But of course they've built a challenge which makes that somewhat inefficient (I reckon of maybe 
# ~ 3^30 possible routes available, or thereabouts, if you try every possible option)
#
# I never tested that version to completion. It ran for a couple of hours but I got bored...
# So have added a few special moves to avoid exploring some of the more obvious inefficient branches.


choose <- function(current_loc = "AA",
                   came_from = "-",
                   moves_so_far = 0,
                   open_valves = character(0),
                   flow_rate = 0,
                   total_flow = 0,
                   sequence = "AA") {
  
  # These too bits of code are to attempt to avoid going down inefficient branches. May need to tune!

  if (moves_so_far == 5 & total_flow == 0)
    return()
  if (moves_so_far == 20 & total_flow < 600)
    return()
  
  
  # If we're out of time (i.e. made 30 choices), then don't carry on!
  # If this is the best flow rate so far, then print out the sequence and flow for good measure...
  
  if (moves_so_far == 30)
  {
    if (total_flow > best) {
      print(paste(total_flow, paste(sequence, collapse=",")))
      best <<- total_flow
    }
    return()
  }
  
  # ---
  # Now branch for each possible choice of 
  # + Open this valve if it closed and non-zero
  # + Go down any tunnel that you've not either immediately come from, or 
  #   would obviously be a move consisting of taking 2 tunnels consecutively where one would have got you there
  
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
