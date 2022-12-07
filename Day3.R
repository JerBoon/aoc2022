

dat <- read.delim("data/Day3.txt",
                  header=FALSE,
                  col.names=c("str"))

library(dplyr)
library(tidyr)

ranks <- data.frame(
  let=c(letters,LETTERS),
  val=1:52)

## Part 1

dat %>%
  mutate(pack=1:nrow(dat)) %>%
  mutate(str1 = substr(str,1,nchar(str)/2),
         str2 = substr(str,nchar(str)/2+1, nchar(str))) %>%
  select(-str) %>%
  separate_rows(str1, sep="") %>%
  separate_rows(str2, sep="") %>%
  filter(str1 == str2 & str1 > "") %>%
  distinct() %>%
  inner_join(ranks, by=c("str1"="let")) %>%
  summarise(val=sum(val))


## Part 2

dat %>%
  mutate(pack=1:nrow(dat)) %>%
  mutate(elf=((1:nrow(dat))+2) %/% 3) %>%
  separate_rows(str, sep="") %>%
  filter(str > "") %>%
  group_by(elf,str) %>%
  summarise(n=n_distinct(pack)) %>%
  ungroup() %>%
  filter(n==3) %>%
  inner_join(ranks, by=c("str"="let")) %>%
  summarise(val=sum(val))
  

  
