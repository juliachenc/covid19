
###############################################################################################################
## CALCULATE FREQUENCY OF EACH RISK PROFILE IN LAC POPULATION
###############################################################################################################

#########################################################################################################
### INPUTS REQUIRED
# Pr.H
# Pr.Q
# Pr.D
# n_profiles
# X.mat
# Pop.prevalence
# SIGMA


#########################################################################################################
### Calculate frequency of each profile in LAC population

### Create sample population using marginal prevalence of each risk factor and weighted corelation matrix between risk profiles
n_samples <- 50000
profile.cnt.SPAs <- matrix(0, nrow = n_profiles, ncol = 1)
sample.pop.df <- as.data.frame(rmvbin(n_samples, margprob=Pop.prev.matrix, sigma=SIGMA))
colnames(sample.pop.df)<-colnames(Pop.prev.matrix)

### Create an "any comorbidity" vector
any.comb <- c(rep(0,n_samples))

for (i in 1:nrow(sample.pop.df)) {
  if(sample.pop.df$diabetes[i]==1 || sample.pop.df$hypertension[i]==1 || sample.pop.df$copd[i]==1 || sample.pop.df$coronary[i]==1 || sample.pop.df$cancer[i]==1)
  {any.comb[i]=1}
}

### Create a new sample.pop df with the individual comborbidities deleted and the "any.comb" added
sample.pop.df <- sample.pop.df %>% dplyr::select(-c(diabetes,hypertension,copd,coronary,cancer))
sample.pop.df$any.comb <- any.comb

### Count population prevalence of profiles (much faster with as.matrix())
profiles <- X.mat
sample.pop.table <- as.matrix(sample.pop.df)

profile.cnt <- c(rep(0,dim(profiles)[1]))
for (i in 1:dim(profiles)[1]) {
  for (j in 1:dim(sample.pop.table)[1]) {
    if(all(profiles[i,]==sample.pop.table[j,]))
    {profile.cnt[i]<-(profile.cnt[i]+1)}
  }
}

### Get frequency distribution over profiles
profile.cnt.freq <- round(profile.cnt/sum(profile.cnt), digits=6)
profile.cnt.freq<-as.data.frame(cbind(profile.cnt.freq,X.mat))
colnames(profile.cnt.freq) <- c("LA County",colnames(X.mat))

# kable(profile.cnt.freq) %>%
#   kable_styling(bootstrap_options = "striped", full_width = F)

#########################################################################################################
### OUTPUTS
# profile.cnt.freq

