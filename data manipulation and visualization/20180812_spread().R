
###'######################################################################
###'
###' Spread()
###' 
###' 
###' 20180811 JoonHo Lee
###' 
###' 

###'######################################################################
###'
###' Basic settings
###'
###'

### Start with a clean slate
gc()            # force R to release memory it is no longer using
rm(list=ls())   # delete all the objects in the workspace


### Call libraries
library(tidyverse)


###'######################################################################
###'
###' A simple example
###'
###'

stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

stocksm <- stocks %>% 
  gather(stock, price, -time)

stocksm %>% 
  spread(stock, price)

stocksm %>% 
  spread(time, price)


# Spread and gather are complements
df <- data.frame(x = c("a", "b"), y = c(3, 4), z = c(5, 6))
df %>% spread(x, y) %>% gather(x, y, a:b, na.rm = TRUE)


# Use 'convert = TRUE' to produce variables of mixed type
df <- data.frame(row = rep(c(1, 51), each = 3),
                 var = c("Sepal.Length", "Species", "Species_num"),
                 value = c(5.1, "setosa", 1, 7.0, "versicolor", 2))
df %>% spread(var, value) %>% str
df %>% spread(var, value, convert = TRUE) %>% str



###'######################################################################
###'
###' Spread multiple columns
###'
###'

df <- data.frame(month = rep(1:3,2),
                 student = rep(c("Amy", "Bob"), each = 3),
                 A = c(9, 7, 6, 8, 6, 9),
                 B = c(6, 7, 8, 5, 6, 7))

df %>% 
  gather(variable, value, -(month:student)) %>%
  unite(temp, student, variable) %>%
  spread(temp, value)

