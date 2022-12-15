

dat <- read.delim("data/Day11.txt",
                  sep=":", 
                  strip.white=TRUE,
                  header=FALSE,
                  col.names=c("key","value"))

library(dplyr)
library(tidyr)


## Part 1

df <- 
  dat %>%
  mutate(monkey_no=case_when(grepl("^Monkey",key) ~ substr(key,8,10))) %>%
  fill(monkey_no,.direction="down") %>%
  filter(!grepl("^Monkey",key)) %>%
  mutate(key=gsub(" ","_",tolower(key))) %>%
  pivot_wider(names_from="key",values_from = "value") %>%
  mutate(inspections = 0)

df

for (n in 1:20){
  for (m in 1:nrow(df))
  {
    # Each monkey
    items <- strsplit(df$starting_items[m],split="[ ,]+")[[1]]
    items <- items[items != ""]
    df$inspections[m] <- df$inspections[m] + length(items)
    for (i in items) {
      old <- as.integer(i) 
      eval(parse(text=df$operation[m]))
      new <- new %/% 3
      if (new %% as.integer(sub("divisible by ","",df$test[m])) == 0)
        new_monkey <- as.integer(sub("throw to monkey ","",df$if_true[m]))+1
      else
        new_monkey <- as.integer(sub("throw to monkey ","",df$if_false[m]))+1
      df$starting_items[new_monkey] <- paste(df$starting_items[new_monkey],',',new)
    }
    df$starting_items[m] <- ""
  }
}
df

df %>%
  slice_max(order_by=inspections,n=2) %>%
  summarise(monkey_business = min(inspections) * max(inspections))

## Part 2

df <- 
  dat %>%
  mutate(monkey_no=case_when(grepl("^Monkey",key) ~ substr(key,8,10))) %>%
  fill(monkey_no,.direction="down") %>%
  filter(!grepl("^Monkey",key)) %>%
  mutate(key=gsub(" ","_",tolower(key))) %>%
  pivot_wider(names_from="key",values_from = "value") %>%
  mutate(inspections = 0)

df

div <- prod(as.integer(sub("divisible by ","",df$test[])))

for (n in 1:10000){
  for (m in 1:nrow(df))
  {
    # Each monkey
    items <- strsplit(df$starting_items[m],split="[ ,]+")[[1]]
    items <- items[items != ""]
    df$inspections[m] <- df$inspections[m] + length(items)
    for (i in items) {
      old <- as.numeric(i) 
      eval(parse(text=df$operation[m]))
      #new <- new %/% 3
      new <- new  %% div
      if (new %% as.integer(sub("divisible by ","",df$test[m])) == 0)
        new_monkey <- as.numeric(sub("throw to monkey ","",df$if_true[m]))+1
      else
        new_monkey <- as.numeric(sub("throw to monkey ","",df$if_false[m]))+1
      df$starting_items[new_monkey] <- paste(df$starting_items[new_monkey],',',new)
    }
    df$starting_items[m] <- ""
  }
}
df

df %>%
  slice_max(order_by=inspections,n=2) %>%
  summarise(monkey_business = min(inspections) * max(inspections))
