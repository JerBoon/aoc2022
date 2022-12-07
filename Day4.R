

dat <- read.delim("data/Day4.txt",
                  header=FALSE,
                  col.names=c("str"))

library(dplyr)
library(tidyr)


## Part 1

dat %>%
  separate(str,sep="[,-]", into=c("a1","a2","b1","b2"), remove=FALSE) %>%
  mutate(a1 = as.integer(a1),
         a2 = as.integer(a2),
         b1 = as.integer(b1),
         b2 = as.integer(b2)) %>%
  mutate(contains=case_when(a1 <= b1 & a2 >= b2 ~ TRUE,
                            b1 <= a1 & b2 >= a2 ~ TRUE,
                            TRUE ~ FALSE)) %>%
  summarise(n=sum(contains))

## Part 2

dat %>%
  separate(str,sep="[,-]", into=c("a1","a2","b1","b2"), remove=FALSE) %>%
  mutate(a1 = as.integer(a1),
         a2 = as.integer(a2),
         b1 = as.integer(b1),
         b2 = as.integer(b2)) %>%
  mutate(overlap=case_when(a1 <= b2 & a2 >= b1 ~ TRUE,
                            TRUE ~ FALSE)) %>%
  summarise(n=sum(overlap))
