

dat <- read.delim("data/Day1.txt",
                  blank.lines.skip=FALSE,
                  header=FALSE,
                  col.names="Calories")


library(dplyr)
library(tidyr)

##Wrangle an Elf# column, lose the blank lines
dat$Elf <- NA
dat$Elf[is.na(dat$Calories)] <- paste("Elf", 2:(sum(is.na(dat$Calories))+1))

dat <- dat %>%
  fill(Elf, .direction = "down") %>%
  mutate(Elf = case_when(is.na(Elf) ~ "Elf 1", TRUE ~ Elf)) %>%
  filter(!is.na(Calories))


## Part 1
dat %>%
  group_by(Elf) %>%
  summarise(Calories=sum(Calories)) %>%
  slice_max(order_by=Calories,n=1)

## Part 2
dat %>%
  group_by(Elf) %>%
  summarise(Calories=sum(Calories)) %>%
  slice_max(order_by=Calories,n=3) %>%
  summarise(Calories=sum(Calories))
