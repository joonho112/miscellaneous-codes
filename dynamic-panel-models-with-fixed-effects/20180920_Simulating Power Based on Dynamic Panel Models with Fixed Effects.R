
###'######################################################################
###'
###' Calculating Statistical Power Based on 
###' 
###' a Dynamic Panel Model with Fixed Effects
###' 
###' 
###' 20180920 Soeun Yang & JoonHo Lee
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


### Call packages
library(tidyverse)
library(OrthoPanels)



###'######################################################################
###'
###' Data Generating Model
###' => A Dynamic Panel Model with Fixed Effects
###'    (Fixed Effects + Lagged Dependent Variables)
###' 
###' y_{it} = \rho*y_{it-1} + \beta*x_{it} + \eta_{i} + \epsilon_{1it}
###' x_{it} = 0.75*\eta_{i} + \epsilon_{2it}
###' \eta_{i} ~ Unif(-1, 1)
###' \epsilon_{1it} ~ N(0, 1^2)
###' \epsilon_{2it} ~ N(0, 4^2)
###'
###' @rho = the autoregressive parameter
###' @eta = individual fixed effects (unobservable but time-invariant individual effects)
###' @beta = the effect of time-varying predictor
###'
###'

### Define a function 

generate_simdata <- function(N, T, rho, beta, sig2, n_burn = 50) {
  
  ### (1) Set the number of burn-ins & # of columns
  LT <- T + n_burn
  
  
  ### (2) Simulate fixed effects eta from uniform distribution
  eta <- runif(N, -1, 1)  
  
  
  ### (3) Simulate x by adding normal random error
  x <- array(.75 * eta, dim = c(N, LT)) + rnorm(N * LT, sd = 4)
  
  
  ### (4) Simulate y
  
  y <- matrix(0, N, LT) 
  
  for (t in 1:LT) {
    
    # get the lagged dependent variable
    yy <- if (t > 1)
      y[, t - 1]
    else  # predicted y of the 1st year: y_{it} = y_{it-1}
      ((eta + beta * .75 * eta)/(1 - rho))
    
    # simulate y conforming to the data generating model
    y[, t] <- rho*yy + x[, t]*beta + eta + rnorm(N, sd = sqrt(sig2))
  }
  
  
  ###' (5) Collect x1 and y as a panel data format
  ###'     : after discarding the burn-ins 
  data.frame(i = rep(seq(N), LT - n_burn), 
             t = rep(seq(LT - n_burn), each = N),
             x1 = c(x[(n_burn * N + 1):(LT * N)]), 
             y = c(y[(n_burn * N + 1):(LT * N)]))
  
}



###'######################################################################
###'
###' Simulation
###'
###'

###' (1) Set fixed parameter values
sig2 <- 1     # level-1 measurement error variance
T <- 3        # number of measurement occasions
reps <- 100   # number of replications


###' (2) Set varying parameter values
###' 
###' y-axis: calculated power => simulation outcome
###' x-axis: N = number of individuals 
###' factor: beta = effect sizes
###' facet_grid: rho = autoregressive effects => 
###' 

N_vec <- c(10, 20, 30, 50, 100, 200, 400) 

beta_vec <- c(0.05, 0.10, 0.20, 0.40)

rho_vec <- c(0.1, 0.5, 0.9) 


### (3) Define empty list/dataframe to collect estimation results
opm_results <- list()
power_df <- data.frame()
l=

###' (4) Start looping

set.seed(2346)

for (i in seq_along(N_vec)){   
  for (j in seq_along(beta_vec)) {
    for (k in seq_along(rho_vec)) {
      
      ### Get varying parameters
      N <- N_vec[i]
      beta <- beta_vec[j]
      rho <- rho_vec[k]
      
      
      ### Generate 100 replicated datasets
      ds <- replicate(n = reps,
                      generate_simdata(N = N, T = T, 
                                       rho = rho, beta = beta, 
                                       sig2 = sig2), simplify = FALSE)
      
      
      ###' Estimate a dynamic panel model with fixed effects  
      ###' using the orthogonal reparameterization approach
      opms <- lapply(ds, function(d) {
        opm(y ~ x1, data = d, n.samp = 100)
      })
      
      opm_results[[paste(N, beta, rho, sep = "_")]] <- opms
      
      
      ### Extract 95% CI and calculate power
      CI95 <- t(sapply(opms, function(x){confint(x, level = 0.95)[3, ]}))
      
      H0_reject_rate <- !(CI95[, 1] < 0 & CI95[, 2] >= 0)
      
      power <- mean(H0_reject_rate)
      
      power_df <- bind_rows(power_df, data.frame(N, beta, rho, power))
      
    } # end of loop over rho
  } # end of loop over beta
} # end of loop over N



###'######################################################################
###'
###' Plot 
###'
###'

### Call plot functions
source("~/SACS/functions/plot_helper.R")
setwd("~/data-manipulation-and-visualization")


### Covert numeric to factor
power_df <- power_df %>%
  mutate(beta_factor = factor(beta, levels = beta_vec, 
                              labels = paste0("effect size = ", beta_vec)), 
         rho_factor = factor(rho, levels = rho_vec, 
                             labels = paste0("autoregressive effect = ", rho_vec)))


### Generate & Save the plot
p <- plot_trend_grp_facet(power_df, N, power, beta_factor, .~rho_factor) + 
  scale_x_continuous(breaks = N_vec) + 
  scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.1)) + 
  labs(title = "Statistical Power for the Dynamic Panel Model with Fixed Effects", 
       subtitle = "with two measurement occasions, significance level = 0.05", 
       y = "Power", 
       x = "Number of individuals (Sample Size)", 
       caption = NULL)

print(p)

ggsave("figures/Power for dynamic panel model.pdf", p, width = 10.5, height = 6.5)


