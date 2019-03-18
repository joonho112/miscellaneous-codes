
###'######################################################################
###' 
###' Simulation study
###' 
###' - Propensity score matching with clustered data
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
library(foreign)
library(CMatching)


### Call functions
source("~/miscellaneous-codes/helper_wrangling.R")
source("~/miscellaneous-codes/helper_plot.R")



###'######################################################################
###'
###' Import empirical data containing covariates
###'
###' NAEA 2009 student level data
###'
###'

load(file = "NAEA_2009_Student-Level.rda")


### Arrange dataset by school ID and student ID
names(df)
df <- df %>%
  arrange(sch_id_09, std_id_09) 


### Number of students within schools
df_Nstudents <- df %>% 
  group_by(sch_id_09) %>%
  summarise(Nstudents = n_distinct(std_id_09))

p <- ggplot(data = df_Nstudents, aes(x = Nstudents)) + geom_histogram()
print(p)



###'######################################################################
###'
###' Simulate the cluster-level unobserved variable Z
###'
###'

### Convert the school ID into a factor
df <- df %>%
  mutate(schid = factor(sch_id_09))


###' Generate the cluster-level variable Z 
###' which represents unobserved heterogeneity across schools

df <- df %>%
  mutate(Z = rep(rnorm(nlevels(df$schid), 0, 0.25), table(df$schid)))



###'######################################################################
###'
###' Simulation setting: preparing simulation loops
###'
###'

### Number of iterations
iter <- 500


### Sample with replacement?
replacement <- TRUE


### Caliper
caliper <- 0.20


### Coefficient of Z in treatment equation
betaZ <- c(0, 0.2, 0.4, 0.6)


### Create storage vectors
sim_asams_noZ <- sim_asams_Z <- sim_asams <- list()
sim_drop <- pdrop <- list()
sim_att <- sim_relatt <-sim_se <- sim_ar <-sim_icc <- list()



###'######################################################################
###'
###' Start Loop
###'
###'

for (i in seq(iter)){
  
  
  ###'######################################################################
  ###'
  ###' Set text progress bar
  ###'
  ###'
  
  pb <- txtProgressBar(min = 0, max = iter, style = 3)
  setTxtProgressBar(pb, i)
  
  
  
  ###'######################################################################
  ###'
  ###' Generate random effects
  ###'
  ###'
  
  ### Generate random effects at the school level for the treatment equation
  set.seed(i + 6)
  df$ret <- rep(rnorm(nlevels(df$schid), 0, 0.2), table(df$schid))
  
  
  ### Generate random effects at the school level for the outcome equation
  set.seed(i + 9)
  df$rey <- rep(rnorm(nlevels(df$schid), 0, 0.2), table(df$schid))
  
  
  
  ###'######################################################################
  ###'
  ###' Generate simulated treatment
  ###'
  ###'
  
  ### TREATMENT MODEL:
  df$sim.probcs<- (1 + exp(-(4.96 + 0.46*df$female + 
                             0.54*df$singlemom09 + 0.74*df$singledad09 + 
                             1.15*df$custodial09 + 0.08*df$homework09 +
                             0.03*df$tutoring09 + 0.33*df$EBS09 + 0.85*df$ingang09 +
                             0.04*df$reading09 -0.18*df$watchTV09 + 0.65*df$comgame09 +
                             betaZ*df$Z + df$ret)))^-1
  
  
  ### Generate simulated treatment
  set.seed(i)
  unif <- runif(dim(df)[1], 0, 1)
  df$sim_cs <- ifelse(df$sim_probcs > unif, 1, 0)
  
  
  
  ###'######################################################################
  ###'
  ###' Generate simulated outcome
  ###'
  ###'
  
  ### OUTCOME MODEL:
  df$sim.problowapgt1 <-
    (1 + exp(-(8.46 +0.02*d$X1 -0.12*d$X2
               -0.20*d$X3 -0.47*d$X4 +0.11*d$X5
               +0.12*d$X6+0.58*d$X7-1*d$X8
               -0.38*d$X9 -0.34*d$X10 + 0.15*d$X11
               + 0.41*d$sim.cs + 0.4*d$Z +d$rey)))^-1
  
  
  ### Generate simulated outcome
  set.seed(i + 1)
  unif2 <- runif(dim(d)[1], 0, 1)
  df$sim.lowapg<-ifelse((df$sim.problowapgt1 > unif2), 1, 0)
  
  
  
  ###'######################################################################
  ###'
  ###' Propensity scores with logit model (strategies: NV, W, PW,)
  ###'
  ###'
  
  d$sim.ps <- d$sim.logps
  
  
  ### logit ps
  sim.fit1 <- glm(formula = sim.cs ~
                            X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11,
                data = df, family = binomial(link = "logit"))
  
  d$sim.ps <- fitted(sim.fit1)
  
  
  
  ###'######################################################################
  ###' 
  ###' Crude estimate
  ###' 
  ###' 
  
  trueatt <- mean(d$sim.problowapgt1[d$sim.cs == 1]) - 
    mean(d$sim.problowapgt0[d$sim.cs == 1])
  
  
  ### ATT and SE
  attbef <- mean(d$sim.lowapg[d$sim.cs==1]) - mean(d$sim.lowapg[d$sim.cs == 0])
  
  relattbef <- abs(attbef - trueatt)/trueatt
  
  p0 <-(length(d$sim.lowapg[d$sim.cs == 1])*mean(d$sim.lowapg[d$sim.cs == 1])
       +length(d$sim.lowapg[d$sim.cs == 0])*mean(d$sim.lowapg[d$sim.cs == 0]))/dim(d)[1]
  
  sebef <- sqrt((p0*(1-p0)*(1/length(d$sim.lowapg[d$sim.cs==1]) + 1/
                              length(d$sim.lowapg[d$sim.cs==0]))))
  
  
  ### Balance crude
  bal = MatchBalance(sim.cs ~
                     X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11 + Z,
                     data=d,ks = FALSE, nboots=0, print.level=0)
  
  asam_bef <-vector()
  
  for(i in 1:18){asam_bef[[i]] <- bal$BeforeMatching[[i]]$sdiff}
  
  
  
  ###'######################################################################
  ###' 
  ###' Naive matching (NV)
  ###' 
  ###' 
  
  rr = Match(
    Y=d$sim.lowapg, Tr=d$sim.cs,
    X=d$sim.ps, caliper=caliper,
    M=1, replace=replacement,ties=TRUE)
  
  
  ### drops
  mndrop<-table(d[rr$index.dropped,]$ord.Hospital)
  
  
  ### ATT and SE naive matching
  attNV <- rr$est
  
  relattNV<-abs(attNV-trueatt)/trueatt
  
  m0 <- lm(formula = sim.lowapg ~ sim.cs,
           data=md, weights=c(rr$weights,rr$weights))  #summary(m0)
  
  m0.vcovCL<-cluster.vcov(m0, md$Hospital)
  
  seNV<-coeftest(m0, m0.vcovCL)[4]
  
  
  ### Average number of replicates of control units in matched dataset
  ff <- (aggregate(rr$weights ~ rr$index.control,
                   data = cbind(rr$weights,rr$index.control), sum))
  arA<-sum(ff[,"rr$weights"][which(ff[,"rr$weights"]>1)]-1)/
    length(unique(ff[,"rr$index.control"]))  #in general
  
  
  ### Balance after naive matching (NV):
  bal_apm = MatchBalance(sim.cs ~
                         X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11 + Z
                         , data=d, match.out=rr, ks = FALSE, nboots=0, print.level=0)
  
  asamNV <-vector()
  
  for(i in 1:18){asamNV[[i]] <- bal_apm$AfterMatching[[i]]$sdiff}
  
  
  
  ###'######################################################################
  ###' 
  ###' Within-cluster matching (W)
  ###' 
  ###' 
  
  cumfreq <- c(0, cumsum(table(d$ord.Hospital)))
  inddrop<-vector(); indtreat<-vector(); indcontr<-vector()
  weights<-vector(); indatt<-vector() ; indse<-vector()
  
  ### the following cycle matches treated within hospitals
  for( i in 1:(nlevels(d$Hospital) - 2)){
    rr <- Match(
      Y =d[d$Hospital==levels(d$Hospital)[ i ],]$sim.lowapg,
      Tr =d[d$Hospital==levels(d$Hospital)[ i ],]$sim.cs,
      X =d[d$Hospital==levels(d$Hospital)[ i ],]$sim.ps,
      caliper=caliper*sd(d$sim.ps)
      /sd(d[d$Hospital==levels(d$Hospital)[ i ],]$sim.ps),
      M=1, replace=replacement,ties=TRUE)
    inddrop <-c(inddrop,cumfreq[i]+rr$index.dropped)
    indtreat<-c(indtreat,cumfreq[i]+rr$index.treated)
    indcontr<-c(indcontr,cumfreq[i]+rr$index.control)
    indatt<-c(indatt,rr$est)
    indse<-c(indse,rr$se)
    weights<-c(weights,rr$weights)
  }
  
  mwndrop<-table(d[inddrop,]$Hospital) # all drops
  mwd<-rbind(d[indtreat,],d[indcontr,]) # all matched obs
  
  
  ### ATT and SE for within-cluster matching (W)
  attW <- sum((mwd$sim.lowapg[mwd$sim.cs==1]
              -mwd$sim.lowapg[mwd$sim.cs==0])*weights/sum(weights))
  
  relattW<-abs(attW-trueatt)/trueatt
  m0<-lm(formula = sim.lowapg ~ sim.cs,
         data=mwd, weights=c(weights,weights))
  
  m0.vcovCL<-cluster.vcov(m0, mwd$Hospital)
  
  seW<-coeftest(m0, m0.vcovCL)[4]
  
  
  ### Average number of controls replicates
  ff<-(aggregate(weights ~ indcontr , data = cbind(weights,indcontr) , sum))
  arB<-sum(ff[,"weights"][which(ff[,"weights"]>1)]-1)/
    length(unique(ff[,"indcontr"]))#in general
  
  
  ### Balance after within-cluster matching
  bal_amw = MatchBalance(sim.cs ~
                         X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11 +Z
                         , data=mwd,ks = FALSE,
                         weights=c(weights,weights), nboots=0, print.level=0)
  
  asamW <-vector()
  
  for(i in 1:18){asamW[[i]] <- bal_amw$BeforeMatching[[i]]$sdiff}
  
  
  
  ###'######################################################################
  ###' 
  ###' Preferential within-cluster matching (PW)
  ###' first matching within-cluster; remaining units matched between clusters)
  ###' 
  ###' 
  
  umwd<-rbind(d[inddrop,],d[which(d$sim.cs==0),])
  
  
  ### Treated not matched within clusters and controls
  rrc <- Match(
    Y=umwd$sim.lowapg,
    Tr=umwd$sim.cs,
    X=umwd$sim.ps,
    caliper=caliper*sd(d$sim.ps)/sd(umwd$sim.ps),
    M=1, replace=replacement,ties=TRUE)
  mbdata<-rbind(umwd[rrc$index.treated,],umwd[rrc$index.control,])
  
  
  ### Treated matched between (but not within) and matched controls
  mwbd <- rbind(mwd,mbdata)
  
  
  ### Treated matched within or between (and matched controls)
  ### Drops after preferential within match
  mwbndrop<-table(d[rrc$index.dropped,]$ord.Hospital)
  
  
  ### ATT after preferential within-cluster matching
  attPW<-sum((mwbd$sim.lowapg[mwbd$sim.cs==1]
              -mwbd$sim.lowapg[mwbd$sim.cs==0])
             *c(weights,rrc$weights)
             /sum(c(weights,rrc$weights)))
  #mean(mwbd$sim.lowapg[mwbd$sim.cs==1]-mwbd$sim.lowapg[mwbd$sim.cs==0])
  # this only if TIES=FALSE
  
  
  relattWb<-abs(attPW-trueatt)/trueatt
  
  m0<-lm(formula = sim.lowapg ~ sim.cs,
         data=mwbd,weights=rep(c(weights,rrc$weights),2))
  
  m0.vcovCL<-cluster.vcov(m0, mwbd$Hospital)
  
  sePW<-coeftest(m0, m0.vcovCL)[4]
  
  
  ### Average number of controls replicates
  indcontrc<-c(indcontr,rrc$index.control)
  
  weightsc<-c(weights,rrc$weights)
  
  ff<-(aggregate(weightsc ~ indcontrc , data = cbind(weightsc,indcontrc) , sum))
  
  arC<-sum(ff[,"weightsc"][which(ff[,"weightsc"]>1)]-1)/length(unique(ff[,"indcontrc"]))#in general
  
  
  ### Balance after preferential within-cluster matching
  bal_amwb = MatchBalance(sim.cs ~
                            X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11 +Z,
                          data=mwbd,ks = FALSE,
                          weights=c(weights,weights,rrc$weights,rrc$weights),
                          nboots=0, print.level=0)
  asamPW <-vector()
  for(i in 1:18){asamPW[[i]] <- bal_amwb$BeforeMatching[[i]]$sdiff}  
  
  
  ###'######################################################################
  ###' 
  ###' Pooled matching using fixed-effects logit model
  ###' 
  ###' 
  
  ### Fixed-effects logit ps
  sim.fit2<-glm(formula = sim.cs ~
                X1+X2+X3+X4+X5+X6+X7
                +X8+X9+X10+X11+ Hospital -1,
                data=d, family = binomial(link = "logit"))
  
  d$sim.ps<-fitted(sim.fit2)
  
  
  ### Pooled matching using fixed-effects logit model
  rrD = Match(
    Y=d$sim.lowapg,
    Tr=d$sim.cs,
    X=d$sim.ps, caliper=caliper,
    M=1, replace=replacement,ties=TRUE)
  
  
  ### Drops
  mndropD<-table(d[rrD$index.dropped,]$ord.Hospital)
  
  
  ### Matched data
  mdD<-rbind(d[rrD$index.treated,],d[rrD$index.control,])
  
  
  ### ATT and SE
  attFE<-rrD$est
  relattFE<-abs(attFE-trueatt)/trueatt
  
  
  ### seFE <- rrD$se
  m0<-lm(formula = sim.lowapg ~ sim.cs, data=mdD,
         weights=c(rrD$weights,rrD$weights)) #summary(m0)
  
  m0.vcovCL<-cluster.vcov(m0, mdD$Hospital)
  
  seFE<-coeftest(m0, m0.vcovCL)[4]
  
  
  ### Average number of controls replicates
  ff<-(aggregate(rrD$weights ~ rrD$index.control ,
                 data = cbind(rrD$weights,rrD$index.control) , sum))
  
  arD<-sum(ff[,"rrD$weights"][which(ff[,"rrD$weights"]>1)]-1)/
    length(unique(ff[,"rrD$index.control"]))#in general
  
  
  ### Balance after matching using fixed-effects logit model
  bal_apmD = MatchBalance(sim.cs ~
                          X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11 + Z,
                          data=d, match.out=rrD, ks = FALSE,
                          nboots=0, print.level=0)
  
  asamFE <-vector()
  
  for(i in 1:18){asamFE[[i]] <- bal_apmD$AfterMatching[[i]]$sdiff}
  
  
  
  ###'######################################################################
  ###' 
  ###' Pooled match with ps from multilevel (random-effects) logit model (RE)
  ###' 
  ###' 
  
  ### Random-effects logit model
  sim.fit3 <- glmer(formula = sim.cs ~
                    X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+ (1 | Hospital),
                    data=d, family = binomial(link="logit"))
  
  d$sim.ps<-fitted(sim.fit3)
  
  
  ### Pooled matching with random-effects ps model
  rrE = Match(
    Y=d$sim.lowapg,
    Tr=d$sim.cs,
    X=d$sim.ps, caliper=caliper,
    M=1, replace=replacement,ties=TRUE)
  
  
  ### Drops
  mndropE<-table(d[rrE$index.dropped,]$Hospital)
  
  
  ### Matched dataset
  mdE<-rbind(d[rrE$index.treated,],d[rrE$index.control,])
  
  
  ### ATT and SE after matching with method RE
  attRE<-rrE$est
  relattRE<-abs(attRE-trueatt)/trueatt
  
  
  ### seRE<-rrE$se
  m0<-lm(formula = sim.lowapg ~ sim.cs,
         data=mdE, weights=c(rrE$weights,rrE$weights)) #summary(m0)
  
  m0.vcovCL<-cluster.vcov(m0, mdE$Hospital)
  
  seRE<-coeftest(m0, m0.vcovCL)[4]
  
  
  ### Average number of replicates
  ff<-(aggregate(rrE$weights ~ rrE$index.control ,
                 data = cbind(rrE$weights,rrE$index.control) , sum))
  
  arE<-sum(ff[,"rrE$weights"][which(ff[,"rrE$weights"]>1)]-1)/
    length(unique(ff[,"rrE$index.control"]))#in general
  
  
  ### Balance after matching using random-effects logit model
  bal_apmE = MatchBalance(sim.cs ~
                          X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11 + Z,
                          data=d, match.out=rrE, ks = FALSE, nboots=0, print.level=0)
  asamRE <-vector()
  for(i in 1:18){asamRE[[i]] <- bal_apmE$AfterMatching[[i]]$sdiff}
  
  
  
  ###'######################################################################
  ###' 
  ###' Udpate iteration results
  ###' 
  ###' 
  
  covnames<-c("X1","X2","X3","X4","X5",
              "X6","X7","X8","X9","X10",
              "X11","X12","X13",
              "X14","Z")
  
  strnames<-c("crude","NV","W","PW","FE","RE")
  
  drop[[r]]<-t(matrix(
    cbind(mndrop,mwndrop,mwbndrop,mndropD,mndropE),
    nrow=5,ncol=length(mndrop),byrow=TRUE,
    dimnames<-list(c("NV","W","PW","FE","RE"),
                   names(table(d$ord.Hospital)))))
  
  pdrop[[r]]<-apply(sim.drop[[r]],2,sum)/sum(d$sim.cs)
  
  sim.asamsr<-t(matrix(cbind(asam_bef,asamNV,asamW,
                             asamPW,asamFE,asamRE),nrow=6,ncol=length(asam_bef),
                       byrow=TRUE,dimnames<-strnames,covnames))
  
  
  sim.asams[[r]]<-apply(abs(sim.asamsr),2,mean)
  
  sim.asams.noZ[[r]]<-apply(abs(sim.asamsr[-18,]),2,mean)
  
  sim.asams.Z[[r]]<-abs(sim.asamsr["Z",])
  
  sim.att[[r]]<-c(attbef,attNV,attW,attPW,attFE,attRE,trueatt)
  
  names(sim.att[[r]])<-c(strnames,"true")
  
  sim.relatt[[r]]<-c(relattbef,relattNV,relattW,
                     relattWb,relattFE,relattRE,trueatt)
  
  names(sim.att[[r]])<-c(strnames,"true")
  
  sim.se[[r]]<-c(sebef,seNV,seW,sePW,seFE,seRE)
  
  names(sim.se[[r]])<-strnames
  
  sim.ar[[r]]<-c(arA,arB,arC,arD,arE)
  
  names(sim.ar[[r]])<-strnames[-1]

  
  
  ###'######################################################################
  ###'
  ###' End of Loop
  ###'
  
}



###'######################################################################
###'
###' Create tables with all results  
###'
###'

alldrops<-drop[[1]]

for (r in 2:R){alldrops<-rbind(alldrops,drop[[r]])}

allpdrops<-pdrop[[1]]

for (r in 2:R){allpdrops<-rbind(allpdrops,pdrop[[r]])}

allasams<-data.frame(t(matrix(unlist(sim.asams),nrow=6,ncol=R)))

colnames(allasams)<-strnames

allasamsX<-data.frame(t(matrix(unlist(sim.asams.noZ),nrow=6,ncol=R)))
colnames(allasamsX)<-strnames
allasamsH<-data.frame(t(matrix(unlist(sim.asams.Z),nrow=6,ncol=R)))
colnames(allasamsH)<-strnames
allatts<-data.frame(t(matrix(unlist(sim.att),nrow=7,ncol=R)))
colnames(allatts)<-strnames
allrelatts<-data.frame(t(matrix(unlist(sim.relatt),nrow=7,ncol=R)))
colnames(allrelatts)<-c(stranames,"true")
allse<-data.frame(t(matrix(unlist(sim.se),nrow=6,ncol=R)))
colnames(allse)<-strnames
allar<-data.frame(t(matrix(unlist(sim.ar),nrow=5,ncol=R)))
colnames(allar)<-strnames[-1]
allsquares<-(allatts-allatts[,"true"])^2
colnames(allsquares)<-strnames[,"true"]
save.image(file=paste("R",R,"betaZ",betaZ,"calip",caliper,
                      "replacement",replacement,".Rdata",sep=""))


  
  