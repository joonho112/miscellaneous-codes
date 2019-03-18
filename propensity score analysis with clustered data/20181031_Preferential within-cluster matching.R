
###'######################################################################
###' 
###' Preferential within-cluster matching
###' 
###' - An example from B. Arpino and M. Cannas (2016) <doi:10.1002/sim.6880>. 
###' - units that do not match within clusters can match between cluster 
###'   in the second step.   
###' 
###' 
###' 20181031 JoonHo Lee
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
work_dir <- c("~/miscellaneous-codes/propensity score analysis with clustered data")
setwd(work_dir)


### Call packages
library(tidyverse)
library(CMatching)



###'######################################################################
###'
###' Import example data
###'   
###' Kreft and De Leeuw, Introducing Multilevel Modeling, Sage (1988).
###' The data set is the subsample of NELS-88 data consisting of 10 handpicked schools 
###' from the 1003 schools in the full data set.
###' 
###' 

data(schools)



###'######################################################################
###'
###' Define variables
###' 
###' Suppose that the effect of homeworks on math score is 
###' unconfounded conditional on X and unobserved school features 
###' (we assume this only for illustrative purpouse)
###'
###' Note that when Group is missing, 
###' NULL or there is only one Group the function 
###' returns the output of the Match function with a warning.
###' 
###' 

X <- schools$ses 
# X <- as.matrix(schools[ , c("ses", "white", "public")])
Y <- schools$math
Tr <- ifelse(schools$homework > 1, 1, 0)
Group <- schools$schid







# Multivariate Matching on covariates in X (default parameters: one-to-one
# matching on X with replacement with a caliper of 0.25; see \code{Match}).
### Match preferentially within school
# first match within schools
# then (try to) match remaining units between schools
mpw <- MatchPW(Y=schools$math, Tr=Tr, X=schools$ses, Group=schools$schid, caliper=0.1)
# examine covariate balance
bmpw<- MatchBalance(Tr~ses,data=schools,match.out=mpw)
# proportion of matched observations
(mpw$orig.treated.nobs-mpw$ndrops) / mpw$orig.treated.nobs
# check drops by school
mpw$orig.ndrops.by.group
# proportion of matched observations after match-within only
(mpw$orig.treated.nobs-sum(mpw$orig.ndrops.by.group.after.within)) / mpw$orig.treated.nobs
# see complete output
mpw
# or use summary method for main results
summary(mpw)
#### Propensity score matching

# estimate the propensity score (ps) model
mod <- glm(Tr~ses+parented+public+sex+race+urban,
           family=binomial(link="logit"),data=schools)
eps <- fitted(mod)
# eg 1: preferential within-school propensity score matching
psmw <- MatchPW(Y=schools$math, Tr=Tr, X=eps, Group=schools$schid, caliper=0.1)
# We can use other strategies for controlling unobserved cluster covariates
# by using different specifications of ps (see Arpino and Mealli for details):
# eg 2: standard propensity score matching using ps estimated
# from a logit model with dummies for schools
mod <- glm(Tr ~ ses + parented + public + sex + race + urban
           +schid - 1,family=binomial(link="logit"),data=schools)
eps <- fitted(mod)
dpsm <- MatchPW(Y=schools$math, Tr=Tr, X=eps, caliper=0.1)
# this is equivalent to run Match with X=eps
# eg3: standard propensity score matching using ps estimated from

# multilevel logit model (random intercept at the school level)
require(lme4)
mod<-glmer(Tr ~ ses + parented + public + sex + race + urban + (1|schid),
           family=binomial(link="logit"), data=schools)
eps <- fitted(mod)
mpsm<-MatchPW(Y=schools$math, Tr=Tr, X=eps, Group=NULL, caliper=0.1)
# this is equivalent to run Match with X=eps














