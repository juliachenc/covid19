
## INPUTS

# Profile matrix
X.mat <- as.matrix(dplyr::select(Pr.H.filter, -1))

# Number of risk profiles
n.q <- nrow(X.mat)
# Number of risk factors
n.p <- ncol(X.mat)

# Estimated frequency of each risk profile
freq.PREV.q <- as.vector(profile.cnt.SPAs[,9])

# MARGINALIZE: Get marginal frequency of each risk factor
freq.PREV.p <- t(freq.PREV.q) %*% X.mat

# MARGINALIZE AGE ONLY
freq.PREV.p.AGE <- as.vector(c(freq.PREV.p[,c(1:4)],rep(0,5) ))

# Multiply X.mat with marginal vector of risk factor prevalence frequencies FOR AGE ONLY to get the marginal prevalence frequency corresponding to each profile
marginal.PREV.freq.for.q <- X.mat %*% freq.PREV.p.AGE

# Get observed marginal age g.age.p
time <- 1
g.age.p <- as.vector(rep(0, n.p))
g.age.p[1] <- freq.LAC.obs.age[1,time]
g.age.p[2] <- freq.LAC.obs.age[2,time]* freq.p.AGE[2]/sum(freq.p.AGE[2:3])
g.age.p[3] <- freq.LAC.obs.age[2,time]* freq.p.AGE[3]/sum(freq.p.AGE[2:3])
g.age.p[4] <- freq.LAC.obs.age[3,time]

## Get estimated frequency of risk profiles in ILLNESS population re-scaled using observed LAC data
# Multiply X.mat with marginal vector of observed risk factor illness frequencies FOR AGE ONLY to get the marginal prevalence frequency corresponding to each profile
marginal.ILL.freq.for.q <- X.mat %*% g.age.p

# Get "observed" ILL frequency for each risk profile
freq.ILL.q <- freq.PREV.q * marginal.ILL.freq.for.q / marginal.PREV.freq.for.q

# MARGINALIZE to get ILL frequency over risk factors
freq.ILL.p <- t(freq.ILL.q) %*% X.mat

####################################################################
## Function to get estimated frequency of risk profiles in ILLNESS population re-scaled using observed LAC data

freq.ILL.q <- function(X.mat, freq.PREV.q, freq.LAC.obs.age, time){

  # Number of risk factors
  n.p <- ncol(X.mat)

  # MARGINALIZE: Get marginal frequency of each risk factor
  freq.PREV.p <- t(freq.PREV.q) %*% X.mat

  # MARGINALIZE AGE ONLY
  freq.PREV.p.AGE <- as.vector(c(freq.PREV.p[,c(1:4)],rep(0,5) ))

  # Matrix multiply X.mat with marginal vector of risk factor prevalence frequencies FOR AGE ONLY to get the marginal AGE GROUP frequency in PREVALENCE corresponding to each profile
  marginal.PREV.freq.for.q <- X.mat %*% freq.PREV.p.AGE

  # Get LAC OBSERVED marginal frequency of each AGE GROUP in ILLNESSES (adjusting so it is in 4 age groups vs. the 3 observed)
  t <- time
  g.age.p <- as.vector(rep(0, n.p))
  g.age.p[1] <- freq.LAC.obs.age[1,t]
  g.age.p[2] <- freq.LAC.obs.age[2,t]* freq.PREV.p.AGE[2]/sum(freq.PREV.p.AGE[2:3])
  g.age.p[3] <- freq.LAC.obs.age[2,t]* freq.PREV.p.AGE[3]/sum(freq.PREV.p.AGE[2:3])
  g.age.p[4] <- freq.LAC.obs.age[3,t]

  # Matrix multiply X.mat with LAC OBSERVED marginal frequency of each AGE GROUP in ILLNESSES to get the marginal AGE GROUP frequency in ILLNESSES corresponding to each profile
  marginal.ILL.freq.for.q <- X.mat %*% g.age.p

  # Dot product individual vector components, all q x 1, to get frequency in ILLNESSES over each risk profile
  freq.ILL.q <- freq.PREV.q * marginal.ILL.freq.for.q / marginal.PREV.freq.for.q

  # MARGINALIZE to get frequency in ILLNESSES over each risk factor
  freq.ILL.p <- t(freq.ILL.q) %*% X.mat

  return(freq.ILL.q)
}

## INPUTS
# Profile matrix
X.mat <- as.matrix(dplyr::select(Pr.H.filter, -1))
# Estimated frequency of each risk profile
freq.PREV.q <- as.vector(profile.cnt.SPAs[,9])
# Date to use data for
time <- 2

##
freq.ILLNESS.q <- freq.ILL.q(X.mat, freq.PREV.q, freq.LAC.obs.age, time = time)







########################################################################################################################################
########################################################################################################################################



freq.ILL.q.2 <- function(X.mat, freq.PREV.q, freq.LAC.obs.age, time){

  # Number of risk factors
  n.p <- ncol(X.mat)

  X.mat.AGE <- X.mat[,c(1:4)]

  # MARGINALIZE: Get marginal frequency of each risk factor
  freq.PREV.p <- t(freq.PREV.q) %*% X.mat.AGE

  # # MARGINALIZE AGE ONLY
  # freq.PREV.p.AGE <- as.vector(c(freq.PREV.p[,c(1:4)],rep(0,5) ))

  # Matrix multiply X.mat with marginal vector of risk factor prevalence frequencies FOR AGE ONLY to get the marginal AGE GROUP frequency in PREVALENCE corresponding to each profile
  marginal.PREV.freq.for.q <- X.mat.AGE %*% t(freq.PREV.p)

  # Get LAC OBSERVED marginal frequency of each AGE GROUP in ILLNESSES (adjusting so it is in 4 age groups vs. the 3 observed)
  t <- time
  g.age.p <- as.vector(rep(0, ncol(X.mat.AGE)))
  g.age.p[1] <- freq.LAC.obs.age[1,t]
  g.age.p[2] <- freq.LAC.obs.age[2,t]* freq.PREV.p.AGE[2]/sum(freq.PREV.p.AGE[2:3])
  g.age.p[3] <- freq.LAC.obs.age[2,t]* freq.PREV.p.AGE[3]/sum(freq.PREV.p.AGE[2:3])
  g.age.p[4] <- freq.LAC.obs.age[3,t]

  # Matrix multiply X.mat with LAC OBSERVED marginal frequency of each AGE GROUP in ILLNESSES to get the marginal AGE GROUP frequency in ILLNESSES corresponding to each profile
  marginal.ILL.freq.for.q <- X.mat.AGE %*% g.age.p

  # Dot product individual vector components, all q x 1, to get frequency in ILLNESSES over each risk profile
  freq.ILL.q <- freq.PREV.q * marginal.ILL.freq.for.q / marginal.PREV.freq.for.q

  return(freq.ILL.q)
}


## INPUTS
# Profile matrix
X.mat <- as.matrix(dplyr::select(Pr.H.filter, -1))
# Estimated frequency of each risk profile
freq.PREV.q <- as.vector(profile.cnt.SPAs[,9])
# Date to use data for
time <- 2

freq.ILLNESS.q.2 <- freq.ILL.q(X.mat, freq.PREV.q, freq.LAC.obs.age, time = 2)


## NEXT MODEL

# MARGINALIZE to get frequency in ILLNESSES over each risk factor
freq.ILL.p <- t(freq.ILL.q) %*% X.mat







####################################################################################
## Model 2 Hospitalization input population

freq.H.q <- P.H.I.q * freq.ILL.q / (P.H.I.q * t(freq.ILL.q))


# # Number of risk profiles
# n.q <- nrow(profile.freq.vec.MODEL)
#
# # Number of risk factors
# n.p <- 9
#
# # Profile matrix
# X.mat <- as.matrix(dplyr::select(Pr.H.filter, -1))
#
# # Estimated frequency of each risk profile
# freq.PREV.q <- as.vector(profile.cnt.SPAs[,9])
#
# # MARGINALIZE: Get marginal frequency of each risk factor
# freq.PREV.p <- t(freq.PREV.q) %*% X.mat
#
# # MARGINALIZE AGE ONLY
# freq.PREV.p.AGE <- as.vector(c(freq.PREV.p[,c(1:4)],rep(0,5) ))
#
# # Multiply X.mat with marginal vector of risk factor prevalence frequencies FOR AGE ONLY to get the marginal prevalence frequency corresponding to each profile
# marginal.freq.PREV.for.q <- X.mat %*% freq.PREV.p.AGE
#
# # Get observed marginal age g.age.p
# time <- 1
# g.age.p <- as.vector(rep(0, n.p))
# g.age.p[1] <- freq.LAC.obs.age[1,time]
# g.age.p[2] <- freq.LAC.obs.age[2,time]* freq.p.AGE[2]/sum(freq.p.AGE[2:3])
# g.age.p[3] <- freq.LAC.obs.age[2,time]* freq.p.AGE[3]/sum(freq.p.AGE[2:3])
# g.age.p[4] <- freq.LAC.obs.age[3,time]
#
# # Multiply X.mat with marginal vector of observed risk factor illness frequencies FOR AGE ONLY to get the marginal prevalence frequency corresponding to each profile
# marginal.freq.ILL.for.q <- X.mat %*% g.age.p
#
# # Get "observed" ILL frequency for each risk profile
# freq.ILL.q <- freq.PREV.q * marginal.freq.ILL.for.q / marginal.freq.PREV.for.q
#
# # MARGINALIZE to get ILL frequency over risk factors
# freq.ILL.p <- t(freq.ILL.q) %*% X.mat
#
#
# ####################################################################################
# ##


