
###'######################################################################
###' 
###' Capture side effects  
###' 
###' - These functions wrap functions so that 
###'   instead of generating side effects through 
###'   printed output, messages, warnings, and errors, 
###'   they return enhanced output. 
###'   
###' - They are all adverbs because they modify the action of a verb (a function).
###' 
###' 
###' 20181124 JoonHo Lee
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


### Set working directory
setwd("~/miscellaneous-codes/data manipulation and visualization")


### Call packages
library(tidyverse)



###'######################################################################
###' 
###' safely()
###' 
###' - wrapped function instead returns a list with components result and error. 
###' - One value is always NULL.
###' 
###' 

safe_log <- safely(log)
safe_log(10)
safe_log("a")

list("a", 10, 100) %>%
  map(safe_log) %>%
  transpose()


###' This is a bit easier to work with if you supply a default value
###' of the same type and use the simplify argument to transpose():
safe_log <- safely(log, otherwise = NA_real_)

list("a", 10, 100) %>%
  map(safe_log) %>%
  transpose() %>%
  simplify_all()



###'######################################################################
###' 
###' possibly()
###' 
###' - To replace errors with a default value
###' 
###' 

list("a", 10, 100) %>%
  map_dbl(possibly(log, NA_real_))

