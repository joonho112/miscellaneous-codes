
###'######################################################################
###'
###' Simulating Power with the paramtest Package
###' 
###' Based on Jeffrey Hughes' vignettes
###'
###'
###' 20180920 JoonHo Lee
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


### Load all packages used in the vignette
library('paramtest')
library('pwr')
library('ggplot2')
library('knitr')
library('nlme')
library('lavaan')
library('dplyr')



###'######################################################################
###'
###' Simulating a t-test with paramtest
###' 
###'

###' If we wanted to estimate the power for a two-sample t-test, 
###' we could calculate it analytically using the ‘pwr’ package. 

pwr.t.test(n = 50, d = .5, type = 'two.sample')


### Create user-defined function to generate and analyze data

t_func <- function(simNum, N, d) {
  
  x1 <- rnorm(N, 0, 1)
  x2 <- rnorm(N, d, 1)
  
  # run t-test on generated data
  t <- t.test(x1, x2, var.equal=TRUE)  
  stat <- t$statistic
  p <- t$p.value
  
  # return a named vector with the results we want to keep
  return(c(t = stat, p = p, sig = (p < .05)))

}


### Simulate data
power_ttest <- run_test(t_func, n.iter = 5000, output = 'data.frame', N = 50, d = .5)


### Calculate power
str(power_ttest)
power_ttest$results

results(power_ttest) %>%
  summarise(power=mean(sig))

















