

############################################################################################################
############################################################################################################
## CLEAN AND PROCESS DATA FOR RISK ESTIMATES
############################################################################################################
############################################################################################################

n.profiles <- nrow(X.mat)
n.times <- length(times) 
seq.times <- 1:n.times

## 1) RENAME VARIABLES #############################################################################

#Change the profile numbers to factors and name the variable
Profile <- factor(seq(1,n.profiles))
data.FULL <- as.data.frame(cbind(Profile, X.mat, freq.PREV.q, Pr.OUT, freq.OUT))

## dataI ##
seqI <- 4*(seq.times-1)+1
dataI <- as.data.frame(freq.OUT[,seqI])
colnames(dataI) <- times
dataI <- cbind(Profile, dataI)

dataI.melted <- melt(dataI, id = 'Profile')
dataI.melted$DATE <- dataI.melted$variable
dataI.melted$variable <- NULL
dataI.melted$DATE <- factor(dataI.melted$DATE, levels =  times)
dataI.melted$prevalence.I <- dataI.melted$value
dataI.melted$value <- NULL

## dataH ##
seqH <- 4*(seq.times-1)+2
dataH <- as.data.frame(freq.OUT[,seqH])
colnames(dataH) <- times
dataH <- cbind(Profile, dataH)

dataH.melted <- melt(dataH, id = 'Profile')
dataH.melted$DATE <- dataH.melted$variable
dataH.melted$variable <- NULL
dataH.melted$DATE <- factor(dataH.melted$DATE, levels =  times)
dataH.melted$prevalence.H <- dataH.melted$value
dataH.melted$value <- NULL

## dataQ ##
seqQ <- 4*(seq.times-1)+3
dataQ <- as.data.frame(freq.OUT[,seqQ])
colnames(dataQ) <- times
dataQ <- cbind(Profile, dataQ)

dataQ.melted <- melt(dataQ, id = 'Profile')
dataQ.melted$DATE <- dataQ.melted$variable
dataQ.melted$variable <- NULL
dataQ.melted$DATE <- factor(dataQ.melted$DATE, levels =  times)
dataQ.melted$prevalence.Q <- dataQ.melted$value
dataQ.melted$value <- NULL

## dataD ##
seqD <- 4*(seq.times)
dataD <- as.data.frame(freq.OUT[,seqD])
colnames(dataD) <- times
dataD <- cbind(Profile, dataD)

dataD.melted <- melt(dataD, id = 'Profile')
dataD.melted$DATE <- dataD.melted$variable
dataD.melted$variable <- NULL
dataD.melted$DATE <- factor(dataD.melted$DATE, levels =  times)
dataD.melted$prevalence.D <- dataD.melted$value
dataD.melted$value <- NULL

##################################################################################################
## 2) DUMMY VARIABLES <- LEVELS ##################################################################

data.FULL$age <- "Blank"
data.FULL$age[data.FULL$Age0.19==1] <- "0-19"
data.FULL$age[data.FULL$Age20.44==1] <- "20-44"
data.FULL$age[data.FULL$Age45.64==1] <- "45-64"
data.FULL$age[data.FULL$Age65.==1] <- "65+"
data.FULL$Age0.19 <- NULL
data.FULL$Age20.44 <- NULL
data.FULL$Age45.64 <- NULL
data.FULL$Age65. <- NULL
data.FULL$age <- factor(data.FULL$age, levels =  c("0-19", "20-44", "45-64", "65+"))

data.FULL$BMI <- "Blank"
data.FULL$BMI[data.FULL$ObesityBMI.30==1] <- "BMI<30"
data.FULL$BMI[data.FULL$ObesityBMI30.40==1] <- "30<BMI<40"
data.FULL$BMI[data.FULL$ObesityBMI.40==1] <- "BMI>40"
data.FULL$ObesityBMI.30 <- NULL
data.FULL$ObesityBMI30.40 <- NULL
data.FULL$ObesityBMI.40 <- NULL

data.FULL$BMI <- factor(data.FULL$BMI, levels =  c("BMI<30", "30<BMI<40", "BMI>40"))

data.FULL$smoking <- "Blank"
data.FULL$smoking[data.FULL$SmokingYes==1] <- "Smoker"
data.FULL$smoking[data.FULL$SmokingYes==0] <- "Non Smoker"
data.FULL$smoking <- factor(data.FULL$smoking, levels = c("Smoker", "Non Smoker"))

data.FULL$SmokingYes <- NULL

data.FULL$comorbidity <- "Blank"
data.FULL$comorbidity[data.FULL$ComorbidityYes==1] <- "Comorbidity"
data.FULL$comorbidity[data.FULL$ComorbidityYes==0] <- "No Comorbidity"
data.FULL$comorbidity <- factor(data.FULL$comorbidity, levels = c("Comorbidity", "No Comorbidity"))

data.FULL$ComorbidityYes <- NULL

# ## 4) Create Risk Profile Groupings ##################################################################
# ### NOTE: Grouping done by P(D|I) probabilities
# data.FULL$riskprofile <- "Blank"
# data.FULL$riskprofile[data.FULL$"P(D|I).t1"<.01] <- "Risk 5"
# data.FULL$riskprofile[(data.FULL$"P(D|I).t1"<.05) & (data.FULL$"P(D|I).t1">.01)] <- "Risk 4"
# data.FULL$riskprofile[(data.FULL$"P(D|I).t1"<.1) & (data.FULL$"P(D|I).t1">.05)] <- "Risk 3"
# data.FULL$riskprofile[(data.FULL$"P(D|I).t1"<.2) & (data.FULL$"P(D|I).t1">.1)] <- "Risk 2"
# data.FULL$riskprofile[(data.FULL$"P(D|I).t1">.2)] <- "Risk 1"
# data.FULL$riskprofile <- factor(data.FULL$riskprofile, levels = c("Risk 1", "Risk 2", "Risk 3", "Risk 4", "Risk 5" ))

##############################################################################################################
##############################################################################################################
## Create dataframe summarizing risk profile info, prevalence of risk profile in general population,
##    and prevalence of risk profile in deceased population
##############################################################################################################
##############################################################################################################

#data.pop.prev <- data.FULL
data.pop.prev <- dataI
colnames(data.pop.prev) <- c("Profile", paste0( "I.prev.", times))
data.D.prev <- dataD
colnames(data.D.prev) <- c("Profile", paste0( "D.prev.", times))
data.prev <- merge(data.pop.prev, data.D.prev, by = "Profile")
data.prev <- cbind(data.prev, freq.PREV.q)
data.prev <- arrange(data.prev, data.prev$Profile)

##############################################################################################################
## Function for risk table with CFR and IFR
##############################################################################################################

risk.table.CFR.IFR.dates <- function(ABC.out.mat=ABC.out.mat, time.steps=time.steps, iter=iter, data.prev=data.prev, times.dates=times.dates, round.by=round.by) {

  ## MODEL OUTPUT TO PLOT

  # ABC.out.mat.test = ABC.out.mat[1:100,]
  # model.out <- correlated.param.SIM(ABC.out.mat=ABC.out.mat.test,iter=iter,time.steps=time.steps)
  
  model.out <- correlated.param.SIM(ABC.out.mat=ABC.out.mat,iter=iter,time.steps=time.steps)
    
  # Align dates
  init.date = as.Date("2020-03-01")
  model.out[["date"]] <- model.out[["date"]] + init.date
  model.out.filter <- model.out %>% filter(date %in% times.dates) %>% select(par.id, date, iter, Idetectcum, Itotcum, D)
  
  ########################################
  ## Get CFR and IFR for each iteration
  ########################################

  ## Function to multiply each value of I / D by the prevalence of each profile
  prev.multiply <- function(model.out, model.var, data.prev, t) {
    prev.vec <- data.prev[,t+1]
    var.matrix <- matrix(model.out[,model.var], nrow=nrow(model.out), ncol=length(prev.vec), byrow = FALSE)  ## Replicate the variable (I or D) n.profiles times
    out.prev <- sweep(var.matrix, MARGIN=2, prev.vec, `*`)
    return(out.prev)
  }
  
  ## Get CFR and IFR for each date
  n.profiles <- nrow(data.prev)
  CFR.OUT <- vector("list", n.times)
  IFR.OUT <- vector("list", n.times)
  
  for (t in 1:n.times){
    
    date.t <- as.Date(times.dates[t])
    model.out.t <- model.out.filter %>% filter(date==date.t)
    I.prev.t <- prev.multiply(model.out.t, "Idetectcum",data.pop.prev,t=t)
    Itot.prev.t <- prev.multiply(model.out.t, "Itotcum",data.pop.prev,t=t)
    D.prev.t <- prev.multiply(model.out.t, "D",data.D.prev,t=t)
    CFR.t <- D.prev.t / I.prev.t
    IFR.t <- D.prev.t / Itot.prev.t
    
    # Get median, up_95, low_95
    get.CI.FR <- function(FR.t, is.CFR, round.by){
      colnames(FR.t) <- Profile
      FR.t[!is.finite(FR.t)] <- 0
      FR.t <- reshape2::melt(as.data.frame(FR.t), measure.vars = c(1:ncol(FR.t)), variable.name = "Profile")
      FR.dt <- as.data.table(FR.t)
      FR.CI <- FR.dt[, list(
        median = quantile(value, c(.5), na.rm=TRUE),
        low_95 = quantile(value, c(.025), na.rm=TRUE),
        up_95 = quantile(value, c(.975), na.rm=TRUE)),
        by = "Profile"]
      FR.CI <- as.data.frame(FR.CI) %>% mutate_if(is.numeric, round, digits=round.by)
      if (is.CFR==TRUE) FR <- "CFR." else FR = "IFR."
      colnames(FR.CI) <- c("Profile", paste0(FR, paste0(c("","low_95.","up_95."), paste0("t",t))))
      rm(FR.dt)
      rm(FR.t)
      FR.CI$Profile <- NULL
      return(FR.CI)
    }
    CFR.CI.t <- get.CI.FR(CFR.t, is.CFR=TRUE, round.by=round.by)
    IFR.CI.t <- get.CI.FR(IFR.t, is.CFR=FALSE, round.by=round.by)
    
    # Save
    CFR.OUT[[t]] <- CFR.CI.t
    IFR.OUT[[t]] <- IFR.CI.t
  } # end over times i

  CFR.OUT <- do.call(cbind, CFR.OUT)    
  IFR.OUT <- do.call(cbind, IFR.OUT)
  
  # ### SANITY CHECKS: take weighted average of frequency of each profile and CFR, should equal population-average CFR
  # sum(CFR.OUT[,"CFR.t1"]*freq.OUT[,"freq.I.t1"])
  # sum(CFR.OUT[,"CFR.t2"]*freq.OUT[,"freq.I.t2"])
  # sum(CFR.OUT[,"CFR.t3"]*freq.OUT[,"freq.I.t3"])
  
  ########################################
  ## Create table output
  ########################################

  table.CFR.IFR <- cbind(CFR.OUT %>% select(c(paste0("CFR.",times))),IFR.OUT %>% select(c(paste0("IFR.",times))))
  table.CFR.IFR$Profile <- Profile
  data.Pr.CFR.IFR <- data.FULL %>% select(c(paste0("freq.I.",times), "Profile", "age","BMI","smoking","comorbidity","freq.PREV.q"))
  data.Pr.CFR.IFR <- merge(data.Pr.CFR.IFR, table.CFR.IFR, by = "Profile")

  # FILTER PROFILES TO SHOW IN THE TABLE TO ONLY THOSE > 0 PREVALENCE
  #data.Pr.CFR.IFR <- filter(data.Pr.CFR.IFR, paste0("freq.I.t1") > 0.000000004)
  
  ########################################
  ## Add risk group
  ## Note: Groupings done by CFR on t1
  ########################################
  
  riskprofile <- "Blank"
  riskprofile[data.Pr.CFR.IFR$"CFR.t1"<.01] <- "Risk 5"
  riskprofile[(data.Pr.CFR.IFR$"CFR.t1"<.04) & (data.Pr.CFR.IFR$"CFR.t1">.01)] <- "Risk 4"
  riskprofile[(data.Pr.CFR.IFR$"CFR.t1"<.08) & (data.Pr.CFR.IFR$"CFR.t1">.04)] <- "Risk 3"
  riskprofile[(data.Pr.CFR.IFR$"CFR.t1"<.16) & (data.Pr.CFR.IFR$"CFR.t1">.08)] <- "Risk 2"
  riskprofile[(data.Pr.CFR.IFR$"CFR.t1">.16)] <- "Risk 1"
  riskprofile <- factor(riskprofile, levels = c("Risk 1", "Risk 2", "Risk 3", "Risk 4", "Risk 5" ))
  
  data.Pr.CFR.IFR <- tibble::add_column(data.Pr.CFR.IFR, riskprofile, .after = "Profile")
  
  ##### Arrange
  data.Pr.CFR.IFR <- arrange(data.Pr.CFR.IFR, desc(data.Pr.CFR.IFR$"CFR.t1"))

  return(data.Pr.CFR.IFR)

}

CFR_to_print <- function(risk_table_CFR_FULL, table.dates, times.dates, round.by, filter.by){
  risk_table_CFR <- risk_table_CFR_FULL[,c("Profile","riskprofile","age","comorbidity","BMI","smoking","freq.PREV.q", paste0("freq.I.",times),            
                                           paste0("CFR.",times),paste0("IFR.",times))]
  risk_table_CFR <- arrange(risk_table_CFR, desc(risk_table_CFR$"CFR.t1"))
  risk_table_CFR <- risk_table_CFR %>% mutate_if(is.numeric, round, digits=round.by)
  colnames(risk_table_CFR)[colnames(risk_table_CFR)=="freq.PREV.q"] <- "Pop.Prev"
  colnames(risk_table_CFR)[colnames(risk_table_CFR)=="riskprofile"] <- "Group"
  colnames(risk_table_CFR)[colnames(risk_table_CFR)==paste0("freq.I.",times)] <- paste0("Inf.",times.dates)
  colnames(risk_table_CFR) <- c(colnames(risk_table_CFR)[1:7], paste0("Inf.",table.dates), paste0("CFR.",table.dates), paste0("IFR.",table.dates))
  risk_table_CFR <- filter(risk_table_CFR, Pop.Prev > filter.by)  # FILTER PROFILES TO SHOW IN THE TABLE TO ONLY THOSE > 0 PREVALENCE
  return(risk_table_CFR)
}

#############################################
## Grab the population-average CFR and IFR

pop_avg_median <- function(tables.out, risk_table_CFR, times.dates){
  cfr_table <- tables.out[[3]]
  for (i in 1:nrow(cfr_table)){ 
    table2[i,] <- cfr_table[i,] %>% mutate_if(is.character, function(x) unlist(strsplit(x, " "))[1])
  }
  table2 <- as.data.frame(apply(table2[,1:2], 2, as.numeric)) 
  colnames(table2) <- c("CFR","IFR")
  rownames(table2) <- rownames(cfr_table)
  
  times.dates <- as.character(times.dates)
  pop_avg <- c("Pop-Avg", rep("",times=9), table2[times.dates,"CFR"], table2[times.dates,"IFR"])
  names(pop_avg) <- colnames(risk_table_CFR)
  pop_avg <- as.data.frame(t(pop_avg))
  return(pop_avg)
}

########################################################################
# Probabilities of severe illness by risk profile table
# Requires risk_table_CFR_FULL and Profiles and Pr.OUT
########################################################################

probs_to_print <- function(risk_table_CFR_FULL, Profile,Pr.OUT, table.dates, times.dates, round.by, filter.by){
  profiles_table <- select(risk_table_CFR_FULL, -c( apply(expand.grid(c("freq.I.","CFR.","IFR."),times), 1, paste, collapse="")))
  profiles_table[,ncol(profiles_table)+1] <- select(risk_table_CFR_FULL,"CFR.t1")
  probs_table <- as.data.frame(cbind(Profile,Pr.OUT)) %>% select(-c(paste0("P(D|I).",times)))
  probs_table_FULL <- as.data.frame(merge(profiles_table,probs_table,by="Profile"))
  probs_table_FULL <- probs_table_FULL[,c( colnames(profiles_table), paste0("P(H|I).",times), paste0("P(Q|H).",times), paste0("P(D|Q).",times) )]
  probs_table_FULL <- arrange(probs_table_FULL, desc(probs_table_FULL$"CFR.t1")) %>% mutate_if(is.numeric, round, digits=round.by)
  probs_table_FULL$CFR.t1 <- NULL
  colnames(probs_table_FULL)[colnames(probs_table_FULL)=="freq.PREV.q"] <- "Pop.Prev"
  colnames(probs_table_FULL)[colnames(probs_table_FULL)=="riskprofile"] <- "Group"
  colnames(probs_table_FULL) <- c(colnames(probs_table_FULL)[1:7], paste0("P(H|I).",table.dates), paste0("P(Q|H).",table.dates), paste0("P(D|Q).",table.dates))
  probs_table_FULL <- filter(probs_table_FULL, Pop.Prev > 0.00001)  # FILTER PROFILES TO SHOW IN THE TABLE TO ONLY THOSE > 0 PREVALENCE
  return(probs_table_FULL)
}

print("Clean data, get CFR/IFR table: Done.")
