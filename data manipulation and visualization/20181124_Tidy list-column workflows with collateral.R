
###'######################################################################
###' 
###' Tidy list-column workflows with collateral 
###' 
###' 
###' - The biggest downside of being able to do complex analysis 
###'   on many list elements is that one little error can bring 
###'   a lot of computation down. 
###' 
###' - purrr comes with tools for dealing with errors, warnings and 
###'   other “side effects”, but it’s difficult to pair them effectively 
###'   with purrr’s massively powerful iteration tools.
###' 
###' - R package "collateral" gives you ways to drop-in replacements for map() 
###'   that use employ these side-effect capturing tools, 
###'   as well as an array of other helpers to make navigating everything 
###'   you’ve captured more pleasant.
###'   
###' 
###' See: https://cran.r-project.org/web/packages/collateral/vignettes/collateral.html
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
library(broom)
library(collateral)



###'######################################################################
###'
###' Basic exploratory analysis
###'
###'

### Assign a brief name
df <- diamonds


### Look at distributions
ggplot(df) + 
  geom_histogram(aes(x = price, y = stat(count)))


ggplot(diamonds) +
  geom_histogram(aes(x = price, y = stat(count))) +
  scale_x_log10()


ggplot(diamonds) +
  geom_histogram(aes(x = price, y = stat(count))) +
  facet_wrap(vars(cut), ncol = 1) +
  scale_x_log10() +
  ggtitle('Price vs. cut')


ggplot(diamonds) +
  geom_histogram(aes(x = price, y = stat(count))) +
  facet_wrap(vars(color), ncol = 1) +
  scale_x_log10() +
  ggtitle('Price vs. color')



###'######################################################################
###'
###' Grouping the traditional way
###'
###'

df %>% split(df$cut)

df %>%
  split(df$cut) %>%
  map_dbl(~ mean(.$price))

diamonds_list <- df %>% split(list(df$cut, df$color))  # become unwieldy

map_dbl(diamonds_list, ~ mean(.$price))

map_dbl(diamonds_list, ~ cor(.$price, .$depth))



###'######################################################################
###'
###' Nested data frames make this much easier to handle
###'
###'

nested_diamonds <- df %>%
  select(cut, color, clarity, depth, price) %>%
  nest(-cut, -color)


nested_diamonds


nested_diamonds %>%
  mutate(mean_price = map_dbl(data, ~ mean(.$price)),
         pd_cor = map_dbl(data, ~ cor(.$price, .$depth)))


diamonds_models =  
  nested_diamonds %>%
  mutate(
    price_mod = map(data, ~ lm(.$price ~ .$depth)),
    price_summary = map(price_mod, summary),
    price_rsq = map_dbl(price_summary, 'r.squared'))



###'######################################################################
###'
###' Dealing with side effects with purrr
###' 
###' - We can carry on like this, adding analyses to the groups, 
###'   but it’s also a pretty reckless way to operate. 
###'   
###' - There could be anything going on in these list columns, 
###'   and without being able to see them from the outside 
###'   it’d be easy to miss a problem that turns up. 
###'   
###' - Or, we might hit an error and be unsure which group is causing it.
###'
###'

###' For example, if we remove all of the rows in one of the data groups, 
###' we’ll get this error:
###' Error in mutate_impl(.data, dots) : Evaluation error: 0 (non-NA) cases.

nested_diamonds$data[[5]] = nested_diamonds$data[[5]] %>% filter(price < 300)

diamonds_models =
  nested_diamonds %>%
  mutate(
    price_mod = map(data, ~ lm(.$price ~ .$depth)),
    price_summary = map(price_mod, summary),
    price_rsq = map_dbl(price_summary, 'r.squared'))


###' purrr tries to tackle this with two functions: safely() and quietly(). 
###' The former catches errors; 
###' the latter catches warnings, messages and other output. 

safe_lm = safely(lm)
safe_summary = safely(summary)

purrr_models =
  nested_diamonds %>%
  mutate(price_mod = map(data, ~ safe_lm(.$price ~ .$depth)))

purrr_models


###' (1) We have to remember to wrap each function in safely() or quietly() ahead of time; 
###' (2) We have to check each element of the list column, or carefully extract the results, 
###' to locate the problem.

purrr_models$price_mod[[1]]

purrr_models$price_mod[[5]]

purrr_models %>% mutate(mod_result = map(price_mod, 'result'))



###'######################################################################
###'
###' Collateral: capture, identify and isolate side effects
###'
###'

nested_diamonds$data[[5]] = nested_diamonds$data[[5]] %>% filter(price < 300)

collat_models =
  nested_diamonds %>%
  mutate(price_mod = map_safely(data, ~ lm(.$price ~ .$depth)))

print(collat_models)

summary(collat_models$price_mod) 


collat_models %>%
  group_by(color) %>%
  summarise(
    n_res = tally_results(price_mod),
    n_err = tally_errors(price_mod))


collat_models %>%
  filter(has_errors(price_mod))



