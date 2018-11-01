
###'######################################################################
###' 
###' (1) Simulation code for the overlap setting
###' 
###' - Simulation to see how Super Learner/BART compares with other methods 
###'   over
###'   A) repeated samples  
###'   B) varying coefficients 
###' 
###' - Probably best run as a batch job on a unix/linux machine
###' 
###' 
###' 20181011 JoonHo Lee
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
work_dir <- c("~/data-manipulation-and-visualization/20181011_BART")
setwd(work_dir)


### Call packages
library(tidyverse)
library(BayesTree)


### Call functions
source("code/functions.R")
source("helper_wrangling.R")
source("helper_plot.R")



###'######################################################################
###'
###' Simulation Settings  
###'
###'

### The number of iteration
niters <- 1000


### Set seed number
set.seed(2659232)



###'######################################################################
###'
###' Import Real Data
###' 
###' Simulations Based on Real Data 
###' (The infant Health and Development Program: IHDP)
###' 
###' - The simulations used here are based on covariate data from a real study
###' - Only outcomes are simulated
###' 
###' 

### Load file & Assign brief name
load(file="data/sim.data")
df <- imp1


###' Inspect the imported dataset
###' 985 observations and 29 variables
names(df)
dim(df)



###'######################################################################
###'
###' Create an obsevational study within experimental data
###' 
###' => Throwing away a nonrandom portion of the treatment group:
###'    All children with nonwhite mothers  
###'    
###' => Then the treatment and control groups are no longer balanced
###'    and simple comparisons of outcomes would lead to 
###'    biased estimates of the treatment effect
###'    
###'    

classmode(df, everything())
tabdf(df, treat)
tabdf(df, momwhite)

obs <- df %>%
  filter(!(treat == 1 & momwhite == 0))
tabdf(df, treat)



###'######################################################################
###'
###' Prepare covariates in right format
###' 
###' - 6 continuous and 19 binary covariates
###'
###'

### 6 continuous covariates
covs_cont_n <- c("bw", "b.head", "preterm", 
                 "birth.o", "nnhealth", "momage")


### 19 binary covariates
covs_cat_n <- c("sex", "twin", "b.marr", "mom.lths", "mom.hs", "mom.scoll", 
                "cig", "first", "booze", "drugs", "work.dur", "prenatal", 
                "ark", "ein", "har", "mia", "pen", "tex", "was")


### The number of covariates
covs_ols <- c(covs_cont_n, covs_cat_n)
p <- length(covs_ols)


### Generate a covariate matrix, X
### and record numbers of units and covariates
X <- obs %>%
  select(covs_ols)
dim(X); names(X)

N <- nrow(X)
dimx <- ncol(X)
Xmat <- as.matrix(X)


### Standardize the continuous variables
standardize_vec <- function(vector){
  vec_mean <- mean(vector, na.rm = TRUE)
  vec_sd <- sd(vector, na.rm = TRUE)
  vector_standardized <- (vector - vec_mean)/vec_sd
  return(vector_standardized)
}

X[, covs_cont_n] <- X %>%
  select(covs_cont_n) %>%
  mutate_all(.funs = standardize_vec)

Xmat <- as.matrix(X)



###'######################################################################
###'
###' Prepare arrays to save the three estimates:
###' 
###' (1) Treatment effect point estimates
###' (2) Coverage (0, 1)
###' (3) Length of interval estimates
###'
###'

### Define estimates to collect
estimates <- c("b_te", "b_cov", "b_cil",
               "r_te", "r_cov", "r_cil", 
               "ps_te", "ps_cov", "ps_cil", 
               "ipw_te", "ipw_cov", "ipw_cil", 
               "tau_est", "r2", 
               "b_wrong", "r_wrong", "ps_wrong", "ipw_wrong")

n_estimates <- length(estimates)


### Define methods to apply
methods <- c("bart", "reg", "psm", "ipw")

n_methods <- length(methods)


### Create 3 arrays for saving the estimates from the three response surfaces
results_A <- results_B <- results_C <- matrix(0, niters, n_estimates)
dimnames(results_A) <- dimnames(results_B) <- dimnames(results_C) <- list(NULL, estimates)


### Create 3 arrays for saving the precisions from the three response surfaces
precision_A <- precision_B <- precision_C <- matrix(0, niters, n_methods)
dimnames(precision_A) <- dimnames(precision_B) <- dimnames(precision_C) <- list(NULL, methods)



###'######################################################################
###'
###' Start loop over interations
###'
###'

for (i in seq(niters)){
  
  ###'######################################################################
  ###' 
  ###' Set different seed for each iteration 
  ###' 
  ###' => the problem with setting the seed outside of the loop is that 
  ###'    it would deliver the same Y for every case 
  ###'    where X have the same values
  ###' 
  ###' 
  
  if (i <= 500){
    
    set.seed(565 + i*5)
    
  } else if (i > 500) {
    
    set.seed(7565 + i*5)
    
  }
  
  
  ###'######################################################################
  ###'
  ###' Response surface (1) Linear, parallel across treatment groups   
  ###'
  ###' Y(0) ~ N(XB_A, sigma_y)
  ###' Y(1) ~ N(XB_A + tau, sigma_y)
  ###'
  ###' X: standardized covariate design matrix
  ###' B_A: coefficient vector
  ###'    => randomly sampled values from a vector (0, 1, 2, 3, 4) 
  ###'       with probabilities (0.5, 0.2, 0.15, 0.1, 0.05)
  ###'       which make smaller coefficients more likely
  ###' 
  ###' Note: No treatment effect heterogeneity
  ###'       Linear regression should trump the propensity score based methods
  ###'       because the strong parametric assumptions are satisfied
  ###'       
  ###'
  
  ### Set treatment effect and residual variance
  tau <- 4
  sigma_y <- 1
  
  
  ### Sample the TRUE values of regression coefficients
  beta_A <- sample(c(0:4), 
                   dimx + 1, 
                   replace = TRUE, 
                   prob = c(0.5, 0.2, 0.15, 0.1, 0.05))
  
  
  ### Simulate potential outcomes (response surfaces)
  YA_hat <- cbind(rep(1, N), Xmat) %*% beta_A
  
  YA0 <- rnorm(N, YA_hat, sigma_y)
  YA1 <- rnorm(N, YA_hat + tau, sigma_y)

  ### The vector of observed responses YA
  YA <- YA1
  YA[obs$treat == 0] <- YA0[obs$treat == 0]
  
  
  ### Treatment effect (No heterogeneity)
  tau_A <- 4
  
  
  
  ###'######################################################################
  ###'
  ###' Response surface (2) NON-linear, NOT parallel across treatment groups   
  ###'
  ###' Y(0) ~ N(exp((X + W)*B_B, sigma_y)
  ###' Y(1) ~ N(XB_B - omega_B, sigma_y)
  ###' 
  ###' - W is an offset matrix of the same dimension as X 
  ###'   with every value equal to 0.5
  ###'   
  ###' - B_B: coefficient vector
  ###'    => randomly sampled values from a vector (0, 0.1, 0.2, 0.3, 0.4) 
  ###'       with probabilities (0.6, 0.1, 0.1, 0.1, 0.1)
  ###'       which make smaller coefficients more likely
  ###'       
  ###' - omega_B is chosen in the overlapping setting 
  ###'   such that Conditional Average Treatment Effect on the Treated (CATT)
  ###'   equals 4
  ###'    
  ###'   
  
  ### Sample the TRUE values of regression coefficients
  beta_B <- c(sample(c(0.0, 0.1, 0.2, 0.3, 0.4), 
                     dimx + 1, 
                     replace = TRUE, 
                     prob = c(0.6, 0.1, 0.1, 0.1, 0.1)))
  
  ### Simulate potential outcomes (response surfaces)
  X_plus_W <- cbind(rep(1, N), (Xmat + 0.5))
  YB0_hat <- exp(X_plus_W %*% beta_B)
  YB1_hat <- X_plus_W %*% beta_B
  
  offset <- mean(YB1_hat[obs$treat == 1] - YB0_hat[obs$treat == 1]) - 4
  YB1_hat <- X_plus_W %*% beta_B - offset
  
  YB0 <- rnorm(N, YB0_hat, sigma_y)
  YB1 <- rnorm(N, YB1_hat, sigma_y)
  
  cbind(YB0, YB1)
  
  
  ### The vector of observed responses YB
  YB <- YB1
  YB[obs$treat == 0] <- YB[obs$treat == 0]
  
  
  ### Treatment effect
  tau_B_ind <- YB1_hat[obs$treat == 1] - YB0_hat[obs$treat == 1]  # individual TT
  tab_B <- mean(tau_B_ind)   # Conditional Average Treatment Effect on the Treated (CATT)
  
  
  
  ###'######################################################################
  ###'
  ###' Response surface (3) NON-linear, NOT parallel across treatment groups 
  ###'                     + includes quadratic terms of continuous covariates
  ###'                     + all pairwise interation terms
  ###' 
  ###' Y(0) ~ N(QB_C0, 1)
  ###' Y(1) ~ N(QB_C1 + omega_C, 1)
  ###' 
  ###' - B_C0: coefficient vector 
  ###'   (1) For the coefficients on the original covariates:
  ###'       randomly sampled values from a vector (0, 1, 2) 
  ###'       with probabilities (0.6, 0.3, 0.1)
  ###'       
  ###'   (2) For the coefficients for the quadratic terms
  ###'       randomly sampled values from a vector (0, 0.5, 1.0)
  ###'       with probabilities (0.8, 0.15, 0.05)
  ###' 
  ###' - B_C1: sampled identically but independently with B_C0
  ###'  
  ###' 
  
  ### Create matrix of all interactions etc for third response surface
  ytmp <- rnorm(N, 0, 1)
  mod_bal <- glm(formula = ytmp ~ (bw + b.head + preterm + birth.o + nnhealth + 
                                     momage + sex + twin + b.marr + mom.lths + 
                                     mom.hs + mom.scoll + cig + first + booze + drugs + 
                                     work.dur + prenatal + ark + ein + har + mia + pen + 
                                     tex+was)^2 + 
                   I(bw^2) + I(b.head^2) + I(preterm^2) + I(birth.o^2) + I(nnhealth^2) + 
                   I(momage^2), x = T, data = cbind.data.frame(Xmat))
  
  coefs <- mod_bal$coef[-1]
  XX <- mod_bal$x[, -1]
  XX <- XX[, !is.na(coefs)]
  XXXmat <- cbind(rep(1, N), XX)   # Prepare X design matrix
  
  
  ###' Sample the TRUE values of regression coefficients
  ###' (1) The coefficients on the original covariates
  beta_C_m0 <- sample(c(0, 1, 2), p + 1, replace = TRUE, prob = c(0.6, 0.3, 0.1))  
  beta_C_m1 <- sample(c(0, 1, 2), p + 1, replace = TRUE, prob = c(0.6, 0.3, 0.1)) 
  
  
  ###' Sample the TRUE values of regression coefficients
  ###' (2) The coefficients on the quadratic terms
  ###' => We make these pretty rare because they really represent 3-way interactions
  beta_C_q0 <- sample(c(0, 0.5, 1.0), 
                      ncol(XXXmat) - (p + 1), 
                      replace = TRUE, 
                      prob = c(0.8, 0.15, 0.05))
  
  beta_C_q1 <- sample(c(0, 0.5, 1.0), 
                      ncol(XXXmat) - (p + 1), 
                      replace = TRUE, 
                      prob = c(0.8, 0.15, 0.05))
  
  
  ###' Sample the TRUE values of regression coefficients
  ###' (3) Combine the sampled coefficients
  beta_C0 <- c(beta_C_m0, beta_C_q0)
  beta_C1 <- c(beta_C_m1, beta_C_q1)
  
  
  ### Simulate potential outcomes (response surfaces)
  YC0_hat <- XXXmat %*% beta_C0 
  YC1_hat <- XXXmat %*% beta_C1
  
  offset <- mean(YC1_hat[obs$treat == 1] - YC0_hat[obs$treat == 1]) - 4
  YC1_hat <- XXXmat %*% beta_C1 - offset 
  
  YC0 <- rnorm(N, YC0_hat, sigma_y)
  YC1 <- rnorm(N, YC1_hat, sigma_y)
  cbind(YC0, YC1)

  
  ### The vector of observed responses YC
  YC <- YC1
  YC[obs$treat == 0] <- YC0[obs$treat == 0]
  
  
  ### Treatment effect (No heterogeneity)
  tau_C_ind <- YC1_hat[obs$treat == 1] - YC0_hat[obs$treat == 1]  # individual TT
  tab_C <- mean(tau_C_ind)   # Conditional Average Treatment Effect on the Treated (CATT)
  
  
  
  ###'######################################################################
  ###'
  ###' Generate true individual level sample treatment effects & 
  ###' Collect them into result_A, B, C arrays
  ###'
  ###'
  
  ### Calculate TRUE treatment effects for each data generating model
  tau_As <- mean(YA1[obs$treat == 1] - YA0[obs$treat == 1])
  tau_Bs <- mean(YB1[obs$treat == 1] - YB0[obs$treat == 1])
  tau_Cs <- mean(YC1[obs$treat == 1] - YC0[obs$treat == 1])
  
  taus2 <- c(tau_As, tau_Bs, tau_Cs)
  
  
  ### Collect the TRUE treatment effects in result arrays
  idx <- which(colnames(results_A) == "tau_est")
  results_A[i, idx] <- tau_As
  results_B[i, idx] <- tau_Bs
  results_C[i, idx] <- tau_Cs
  
  
  
  ###'######################################################################
  ###'
  ###' Estimation (1) Linear regression
  ###'
  ###'
  
  
  
  
  ###'######################################################################
  ###'
  ###' Estimation (2) Propensity score matching
  ###'
  ###'
  
  
  
  
  ###'######################################################################
  ###'
  ###' Estimation (3) Propensity score weighting
  ###'
  ###'
  
  
  
  
  
  
  ###'######################################################################
  ###' 
  ###' Print the progress  
  ###' 
  
  cat(paste0("iteration ", years[i], " completed", "\n"))

} # End of loop


