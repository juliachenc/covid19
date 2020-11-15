



########################################################################################################################################
########################################################################################################################################

##################################################
##### INPUTS: RISK MODEL

### Read in JAM produced conditional effect estimates
# psi.mat = read.csv(path(data.dir, "psi.conditional.effect.estimates.csv"), sep=",", header=TRUE,row.names = 1)  #https://docs.google.com/spreadsheets/d/17QkcmFUCnxxaJPIfMkqWfIBhU2MaxaHGA03EtUBUM7o/edit#gid=0
# rownames(psi.mat) <- colnames(X.mat)

### Profile matrix
#X.mat <- as.matrix(dplyr::select(Pr.H.filter, -1))

### Estimated frequency of each risk profile
#freq.PREV.q <- as.vector(profile.cnt.SPAs[,9])



##################################################
##### INPUTS: OBSERVED LAC DATA ON ILLNESSES

# #Get observed LAC Age prevalence in Illnesses
# n.dates.LAC.obs.age <- 2
# freq.LAC.obs.age <- matrix(nrow=3, ncol=n.dates.LAC.obs.age)
# colnames(freq.LAC.obs.age) <- c("Apr.20","Jul.20")
# rownames(freq.LAC.obs.age) <- c("Age0.19","Age20.64","Age65.")
# freq.LAC.obs.age[,1] <- c(0.016, 0.75, 0.233)
# freq.LAC.obs.age[,2] <- c(0.089, 0.794, 0.117)



##################################################
##### INPUTS: EPIDEMIC MODEL

### Read in SEIR estimated Alpha, Kappa, Delta
# risk.probs.POSTERIORS.1 <- ABC.par.stats[1,] %>% select(c("Pr(H|I)1", "Pr(Q|H)1", "Pr(D|Q)1" ))
# risk.probs.POSTERIORS.2 <- as.data.frame(ABC.par.stats[1,] %>% select(c("Pr(H|I)2", "Pr(Q|H)2", "Pr(D|Q)2" )))
# risk.probs.POSTERIORS <- rbind(as.list(risk.probs.POSTERIORS.1),as.list(risk.probs.POSTERIORS.2))
# rownames(risk.probs.POSTERIORS) <- c("Apr.20","Jul.20")
# colnames(risk.probs.POSTERIORS) <- c("Alpha","Kappa","Delta")

## INPUT: risk.probs.POSTERIORS

## Transform to logit
logit.SEIR.est <- risk.probs.POSTERIORS
logit.SEIR.est[] <- lapply(risk.probs.POSTERIORS, function(x) round( log(x/(1-x)), 4 ) )


########################################################################################################################################
########################################################################################################################################
## Function to get estimated frequency of risk profiles in ILLNESS population re-scaled using observed LAC data

freq.ILL <- function(X.mat, freq.PREV.q, freq.LAC.obs.age, time){

  # Number of risk factors
  n.p <- ncol(X.mat)

  X.mat.AGE <- X.mat[,c(1:4)]

  # MARGINALIZE: Get marginal frequency of each risk factor (BY AGE GROUPS ONLY)
  freq.PREV.p <- t(freq.PREV.q) %*% X.mat.AGE

  # ## REMOVING AGE.65+
  # freq.PREV.p[4]<- 0.0000001* freq.PREV.p[4]
  # freq.PREV.p <- freq.PREV.p/sum(freq.PREV.p)

  # # MARGINALIZE AGE ONLY
  # freq.PREV.p.AGE <- as.vector(c(freq.PREV.p[,c(1:4)],rep(0,5) ))

  # Matrix multiply X.mat with marginal vector of risk factor prevalence frequencies FOR AGE ONLY to get the marginal AGE GROUP frequency in PREVALENCE corresponding to each profile
  marginal.PREV.freq.for.q <- X.mat.AGE %*% t(freq.PREV.p)

  # Get LAC OBSERVED marginal frequency of each AGE GROUP in ILLNESSES (adjusting so it is in 4 age groups vs. the 3 observed)
  t <- time
  g.age.p <- as.vector(rep(0, ncol(X.mat.AGE)))
  g.age.p[1] <- freq.LAC.obs.age[1,t]
  g.age.p[2] <- freq.LAC.obs.age[2,t]* freq.PREV.p[2]/sum(freq.PREV.p[2:3])
  g.age.p[3] <- freq.LAC.obs.age[2,t]* freq.PREV.p[3]/sum(freq.PREV.p[2:3])
  g.age.p[4] <- freq.LAC.obs.age[3,t]

  # Matrix multiply X.mat with LAC OBSERVED marginal frequency of each AGE GROUP in ILLNESSES to get the marginal AGE GROUP frequency in ILLNESSES corresponding to each profile
  marginal.ILL.freq.for.q <- X.mat.AGE %*% g.age.p

  # Dot product individual vector components, all q x 1, to get frequency in ILLNESSES over each risk profile
  freq.ILL.q <- freq.PREV.q * marginal.ILL.freq.for.q / marginal.PREV.freq.for.q

  # MARGINALIZE to get frequency in ILLNESSES over each risk factor
  freq.ILL.p <- t(freq.ILL.q) %*% X.mat

  freq.ILL <- vector(mode="list", length=2)
  freq.ILL[[1]]<-freq.ILL.q
  freq.ILL[[2]]<-freq.ILL.p

  return(freq.ILL)

}

####################################################################################
## FUNCTION TO GET POPULATION FREQUENCY FUNCTION FOR H, Q, AND D POPULATIONS
## The general idea is to get:
## freq.H.q <- P.H.I.q * freq.ILL.q / (P.H.I.q %*% t(freq.ILL.q))

get.population.freq <- function(prob.q.vec.IN, freq.q.IN, X.mat) {
  weighted.avg <- as.numeric( t(prob.q.vec.IN) %*% freq.q.IN )
  freq.q.OUT <- prob.q.vec.IN * freq.q.IN / as.numeric( t(prob.q.vec.IN) %*% freq.q.IN )  # FREQUENCY IN POPULATION
  freq.p.OUT <- t(freq.q.OUT) %*% X.mat
  freq.OUT <- vector(mode="list", length=2) #as.list[1,2]
  freq.OUT[[1]] <- freq.q.OUT
  freq.OUT[[2]] <- freq.p.OUT
  freq.OUT[[3]] <- weighted.avg
  return(freq.OUT)
}

####################################################################################
## FUNCTION TO ESTIMATE RISK PROBABILITIES WITH BASELINE FROM EPIDEMIC MODEL

get.Pr.q <- function(model, time, logit.SEIR.est, X.mat, freq.q.IN,  psi.mat){

  # INITIALIZE / EXTRACT
  time <- time
  model <- model
  logit.SEIR.est.m.t <- as.numeric(logit.SEIR.est[time, model])
  logit.SEIR.est.m.t.VEC <- rep( logit.SEIR.est.m.t,  times=nrow(X.mat))
  psi.m.vec <- psi.mat[,model]
  #freq.IN.q <- freq.IN[[2]]
  n.profiles <- nrow(X.mat)
  freq.q.IN.mat <- matrix(rep(freq.q.IN,each=n.profiles),nrow=n.profiles)

  # GET PROBABILITY VECTOR
  Pr.q <- expit( logit.SEIR.est.m.t.VEC + (X.mat - freq.q.IN.mat) %*% psi.m.vec )

  return(Pr.q)

}

####################################################################################
## FUNCTION TO LOOP OVER ALL TIME VALUES
## WITH ADJUSTMENTS MADE FOR SCENARIOS

##################################################################################################################
##################################################################################################################


n.dates <- ncol(freq.LAC.obs.age)

#percent.to.remove <- c(1,0.5) # Remove 100%
#factor.to.remove <- 4  # Age.65.
weighted.avg <- vector("list", n.dates)
weighted.avg.scenarios <- vector("list",length(percent.to.remove))
weighted.avg.scenarios.overall <- NULL

for (remove in 1:length(percent.to.remove)){
  for (t in 1:n.dates){
    percent.remove <- percent.to.remove[remove]
    time = t
    name.date <- colnames(freq.LAC.obs.age)[t]

    freq.I <- freq.ILL(X.mat, freq.PREV.q, freq.LAC.obs.age, time = time)

    Pr.H.I.q <- get.Pr.q(model=1, time=time, logit.SEIR.est, X.mat, freq.q.IN = freq.I[[2]],  psi.mat)
    freq.H <- get.population.freq(prob.q.vec.IN = Pr.H.I.q, freq.q.IN = freq.I[[1]], X.mat)

    Pr.Q.H.q <- get.Pr.q(model=2, time=time, logit.SEIR.est, X.mat, freq.q.IN = freq.H[[2]],  psi.mat)
    freq.Q <- get.population.freq(prob.q.vec.IN = Pr.Q.H.q, freq.q.IN = freq.H[[1]], X.mat)

    Pr.D.Q.q <- get.Pr.q(model=3, time=time, logit.SEIR.est, X.mat, freq.q.IN = freq.Q[[2]],  psi.mat)
    freq.D <- get.population.freq(prob.q.vec.IN = Pr.D.Q.q, freq.q.IN = freq.Q[[1]], X.mat)

    ##
    freq.I.filter <- freq.I[[1]]*(1-percent.remove*X.mat[,factor.to.remove])
    weighted.avg.H.filter <- t(Pr.H.I.q) %*% freq.I.filter

    freq.H.filter <- (get.population.freq(Pr.H.I.q, freq.I.filter, X.mat))[[1]]
    weighted.avg.Q.filter <- t(Pr.Q.H.q) %*% freq.H.filter

    freq.Q.filter <- (get.population.freq(Pr.Q.H.q, freq.H.filter, X.mat))[[1]]
    weighted.avg.D.filter <- t(Pr.D.Q.q) %*% freq.Q.filter

    weighted.avg.t <- cbind( weighted.avg.H.filter, weighted.avg.Q.filter, weighted.avg.D.filter )
    colnames(weighted.avg.t) <- c("Alpha.","Kappa.","Delta.")
    colnames(weighted.avg.t) <- paste0( colnames(weighted.avg.t), name.date)
    weighted.avg[[t]] <- weighted.avg.t
    ##
  }
  weighted.avg.scenarios[[remove]] <- do.call(cbind, weighted.avg)
}
weighted.avg.scenarios.overall <- do.call(rbind,weighted.avg.scenarios)
rownames(weighted.avg.scenarios.overall) <- paste0("removed.",percent.to.remove)



