

dat <- read.delim("data/Day2.txt",
                  blank.lines.skip=FALSE,
                  header=FALSE,
                  sep=" ",
                  col.names=c("them","you"))

library(dplyr)
library(tidyr)


## Part 1
dat %>%
  mutate(score = case_when(
    them=="A" & you=="X" ~ 1 + 3,
    them=="A" & you=="Y" ~ 2 + 6,
    them=="A" & you=="Z" ~ 3 + 0,
    them=="B" & you=="X" ~ 1 + 0,
    them=="B" & you=="Y" ~ 2 + 3,
    them=="B" & you=="Z" ~ 3 + 6,
    them=="C" & you=="X" ~ 1 + 6,
    them=="C" & you=="Y" ~ 2 + 0,
    them=="C" & you=="Z" ~ 3 + 3,
    TRUE ~ -99999999 # to help spot errors!
  )) %>%
  summarise(score= sum(score))


##Part 2
dat %>%
  mutate(guess = case_when(
    them=="A" & you=="X" ~ "C",
    them=="A" & you=="Y" ~ "A",
    them=="A" & you=="Z" ~ "B",
    them=="B" & you=="X" ~ "A",
    them=="B" & you=="Y" ~ "B",
    them=="B" & you=="Z" ~ "C",
    them=="C" & you=="X" ~ "B",
    them=="C" & you=="Y" ~ "C",
    them=="C" & you=="Z" ~ "A",
    TRUE ~ "-99999" # to help spot errors?
  )) %>%
  mutate(score = case_when(
    them=="A" & guess=="A" ~ 1 + 3,
    them=="A" & guess=="B" ~ 2 + 6,
    them=="A" & guess=="C" ~ 3 + 0,
    them=="B" & guess=="A" ~ 1 + 0,
    them=="B" & guess=="B" ~ 2 + 3,
    them=="B" & guess=="C" ~ 3 + 6,
    them=="C" & guess=="A" ~ 1 + 6,
    them=="C" & guess=="B" ~ 2 + 0,
    them=="C" & guess=="C" ~ 3 + 3,
    TRUE ~ -99999999 # to help spot errors!
  )) %>%
  summarise(score= sum(score))
