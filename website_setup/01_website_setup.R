library(reshape2)
library(tidyverse)
library(bindata)
library(odin)
library(EasyABC)
library(dplyr)
library(odin)
library(lubridate)
library(EasyABC)
library(dplyr)
library(here)
library(fs)
library(dde)
library(knitr)
library(kableExtra)

source(here("code/calc_risk_probs_update.R"))

write_rds(risk.probs.SPAs, here("website_results", "risk.probs.SPAs.rds"))

Alpha.prior.mean <- risk.probs.SPAs[9,1]
Kappa.prior.mean <- risk.probs.SPAs[9,2]
Delta.prior.mean <- risk.probs.SPAs[9,3]

###################################################################################################
## READ IN EPIDEMIC MODEL CODE
code.dir=here("/code/")
path_seihqdr_model <- path(code.dir, "stochastic_SEIHQDR_A.R")

## Compile the model
seihqdr_generator <- odin::odin(path_seihqdr_model)

###################################################################################################
## INPUT DATA
# obs_cum_new_counts: cumulative counts for "Htotcum","D","Vcum","Idetectcum","H_new","D_new"
# no_obs: number of observation days
## Read and process the data

obs_cum_new_counts = t(read.csv(here("data", "cum_counts_042420.csv"), sep=",",stringsAsFactors = FALSE))
colnames<-c("Htotcum","D","Vcum","Idetectcum","H_new","D_new","H_capacity","Q_capacity")
colnames(obs_cum_new_counts) <- colnames
obs_cum_new_counts <- as.data.frame(obs_cum_new_counts)
obs_cum_new_counts <- obs_cum_new_counts[-1,]
obs_cum_new_counts[1:8] <- sapply(obs_cum_new_counts[1:8],as.character)
obs_cum_new_counts[1:8] <- sapply(obs_cum_new_counts[1:8],as.numeric)
no_obs <- dim(obs_cum_new_counts)[1]
#capacity_H_Q <- obs_cum_new_counts[,c(7:8)]
#obs_cum_new_counts <- obs_cum_new_counts[,c(1:6)]

step <- c(1:no_obs)
obs.shift <- cbind(step,obs_cum_new_counts)
data = obs.shift

###################################################################################################
## "SUMMARY STATISTICS":
## The cumulative number of cases at all (trusted) time points

sum.stats <- function(data){

  no_obs <- dim(data)[1]

  # Which values of variables to consider
  I.trust.n <- c(10:no_obs)  # The first 9 days of illness cases are unreliable/unavailable
  H.trust.n <- c(17:no_obs)  # The first 16 days of hospitalizations are unreliable/unavailable
  V.trust.n <- c(19:no_obs)  # The first 18 days of ventilation are unreliable/unavailable
  D.trust.n <- c(18:no_obs)  # The first 17 days of mortality are unreliable/unavailable
  Hnew.trust.n <- c(19:no_obs) # The first 18 days of new hospitalizations are unreliable/unavailable
  Dnew.trust.n <- c(28:no_obs) # The first 28 days of new deaths are unreliable/unavailable

  ss.I <- data$Idetectcum[I.trust.n]
  ss.H <- data$Htotcum[H.trust.n]
  ss.V <- data$Vcum[V.trust.n]
  ss.D <- data$D[D.trust.n]
  ss.Hnew <- data$H_new[Hnew.trust.n]
  ss.Dnew <- data$D_new[Dnew.trust.n]

  # Which variables to consider
  summarystats = c(ss.I, ss.H, ss.V, ss.D, ss.Hnew, ss.Dnew)

  return(summarystats)
}

## SUMMARY STATISTICS COMPUTED ON DATA
summarydata <- sum.stats(data)
summarydata

###################################################################################################
## SIMULATION MODEL FUNCTION TO COMPUTE
## A function implementing the model to be simulated
## It must take as arguments a vector of model parameter values par
## and it must return a vector of summary statistics

model.1sim.stats <- function(par){

  R0 <- par[1]
  r <- par[2]
  start_time <- par[3]
  R0_redux <- par[4]
  Delta <- par[5]
  Alpha <- par[6]
  Kappa <- par[7]
  p_V <- par[8]

  ### EPIDEMIC MODEL INPUTS
  d_IH <- 10   #days between illness onset and hospitalization
  d_IR <- 7    #days between illness onset and recovery (hospitalization not required)
  Alpha <- 0.14   #probability infected (I) requires hospitalization (vs. recovers)
  Br <- R0 * ( 1 / ( (r/ ((Alpha/d_IH) + ((1-Alpha)/d_IR)))  + (1-r)*d_IR ))
  Beta_t<- c(0, (start_time+11), (start_time+25), 180, 260, 500) #c(0, 79, 80, 260, 270, 500)
  Beta_y<- c(Br, Br, Br*R0_redux, Br*R0_redux, Br*R0_redux, Br*R0_redux )

  ### GENERATE SIMULATION
  x <- seihqdr_generator(Beta_t=Beta_t, Beta_y=Beta_y, S_ini=1e7, E_ini=10, r=r, Delta=Delta, Alpha=Alpha, Kappa=Kappa, p_QV=p_V)
  st <- start_time
  one_sim <- as.data.frame(x$run(0:(st+no_obs))[(st+1):(st+no_obs),])

  ### SUMMARY STATISTICS COMPUTED ON MODEL OUTPUT:
  summarymodel <- sum.stats(one_sim)

  return(summarymodel)
}


###################################################################################################
## PRIOR DISTRIBUTIONS
## A list joining the prior for each parameter
prior.R0 <- c("normal",2.7,0.3) #prior.R0 <- c("unif",2,3)
prior.r <- c("unif",0.05,0.3)
prior.st <- c("unif",30,100)
prior.R0.redux <- c("unif",0.3,0.55)
prior.Delta <- c("unif",0.2,0.8)
prior.Alpha <- c("unif",Alpha.prior.mean - (Alpha.prior.mean/6), Alpha.prior.mean + (Alpha.prior.mean/2))
prior.Kappa <- c("unif",Kappa.prior.mean - (Kappa.prior.mean/6), Kappa.prior.mean + (Kappa.prior.mean/2))
prior.p_V <- c("unif",0.6,0.9)
prior.par <-list(prior.R0, prior.r,prior.st,prior.R0.redux,prior.Delta,prior.Alpha,prior.Kappa,prior.p_V,prior.R0.redux)

## OTHER INPUTS
tolerance=c(15,4) # defining the sequence of tolerance levels

###################################################################################################
## RUN ABC ALGORITHM
ABC_Wegmann<-ABC_mcmc(method="Marjoram",model=model.1sim.stats,prior=prior.par,
                      summary_stat_target=summarydata, n_calibration=2000,
                      tolerance_quantile=0.1,verbose=TRUE,progress=TRUE)

# ABC_Marjoram<-ABC_mcmc(method="Marjoram",model=model.1sim.stats,prior=prior.par,
#                       summary_stat_target=summarydata, n_calibration=1000,
#                       tolerance_quantile=0.1,verbose=TRUE,progress=TRUE) # running ABC_mcmc

#ABC_out <- ABC_Marjoram
ABC_out <- ABC_Wegmann
write_rds(ABC_out, here("website_results", "ABC_out.rds"))
