

############################################################################################################
############################################################################################################
## CLEAN AND PROCESS DATA FOR RISK ESITMATES
############################################################################################################
############################################################################################################

n.profiles <- nrow(X.mat)

## 1) RENAME VARIABLES #############################################################################

#Change the profile numbers to factors and name the variable
Profile <- factor(seq(1,n.profiles))
data.FULL <- as.data.frame(cbind(Profile, X.mat, Pr.OUT, freq.OUT))

# #Change the name of the variables
# data$I.prev.Apr.20 <- data$Apr.20
# data$I.prev.Jul.20 <- data$Jul.20
# data$Apr.20 <- NULL
# data$Jul.20 <- NULL

## dataI ##
dataI <- as.data.frame(freq.OUT[,c(1,5)])
colnames(dataI) <- c("Apr.20","Jul.20")
dataI <- cbind(Profile, dataI)

dataI.melted <- melt(dataI, id = 'Profile')
dataI.melted$DATE <- dataI.melted$variable
dataI.melted$variable <- NULL
dataI.melted$DATE <- factor(dataI.melted$DATE, levels =  c("Apr.20","Jul.20"))
dataI.melted$prevalence.I <- dataI.melted$value
dataI.melted$value <- NULL

## dataH ##
dataH <- as.data.frame(freq.OUT[,c(2,6)])
colnames(dataH) <- c("Apr.20","Jul.20")
dataH <- cbind(Profile, dataH)

dataH.melted <- melt(dataH, id = 'Profile')
dataH.melted$DATE <- dataH.melted$variable
dataH.melted$variable <- NULL
dataH.melted$DATE <- factor(dataH.melted$DATE, levels =  c("Apr.20","Jul.20"))
dataH.melted$prevalence.H <- dataH.melted$value
dataH.melted$value <- NULL

## dataQ ##
dataQ <- as.data.frame(freq.OUT[,c(3,7)])
colnames(dataQ) <- c("Apr.20","Jul.20")
dataQ <- cbind(Profile, dataQ)

dataQ.melted <- melt(dataQ, id = 'Profile')
dataQ.melted$DATE <- dataQ.melted$variable
dataQ.melted$variable <- NULL
dataQ.melted$DATE <- factor(dataQ.melted$DATE, levels =  c("Apr.20","Jul.20"))
dataQ.melted$prevalence.Q <- dataQ.melted$value
dataQ.melted$value <- NULL

## dataD ##
dataD <- as.data.frame(freq.OUT[,c(4,8)])
colnames(dataD) <- c("Apr.20","Jul.20")
dataD <- cbind(Profile, dataD)

dataD.melted <- melt(dataD, id = 'Profile')
dataD.melted$DATE <- dataD.melted$variable
dataD.melted$variable <- NULL
dataD.melted$DATE <- factor(dataD.melted$DATE, levels =  c("Apr.20","Jul.20"))
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


## 4) FILTER PROFILES TO SHOW IN THE TABLE TO ONLY THOSE > 0 PREVALENCE

#data <- filter(data, `LA County` > 0.0004)

## 4) Create Risk Profile Groupings ##################################################################
### NOTE: Grouping done by P(D|I) probabilities
data.FULL$riskprofile <- "Blank"
data.FULL$riskprofile[data.FULL$"P(D|I).Apr.20"<.01] <- "Risk 5"
data.FULL$riskprofile[(data.FULL$"P(D|I).Apr.20"<.05) & (data.FULL$"P(D|I).Apr.20">.01)] <- "Risk 4"
data.FULL$riskprofile[(data.FULL$"P(D|I).Apr.20"<.1) & (data.FULL$"P(D|I).Apr.20">.05)] <- "Risk 3"
data.FULL$riskprofile[(data.FULL$"P(D|I).Apr.20"<.2) & (data.FULL$"P(D|I).Apr.20">.1)] <- "Risk 2"
data.FULL$riskprofile[(data.FULL$"P(D|I).Apr.20">.2)] <- "Risk 1"
data.FULL$riskprofile <- factor(data.FULL$riskprofile, levels = c("Risk 1", "Risk 2", "Risk 3", "Risk 4", "Risk 5" ))

# data.FULL$riskprofile[data.FULL$"P(D|I).Apr.20"<.01] <- "Risk.5"
# data.FULL$riskprofile[(data.FULL$"P(D|I).Apr.20"<.05) & (data.FULL$"P(D|I).Apr.20">.01)] <- "Risk.4"
# data.FULL$riskprofile[(data.FULL$"P(D|I).Apr.20"<.1) & (data.FULL$"P(D|I).Apr.20">.05)] <- "Risk.3"
# data.FULL$riskprofile[(data.FULL$"P(D|I).Apr.20"<.2) & (data.FULL$"P(D|I).Apr.20">.1)] <- "Risk.2"
# data.FULL$riskprofile[(data.FULL$"P(D|I).Apr.20">.2)] <- "Risk.1"
# data.FULL$riskprofile <- factor(data.FULL$riskprofile, levels = c("Risk.1", "Risk.2", "Risk.3", "Risk.4", "Risk.5" ))

# ## Round all numeric variable to 7 digits
# data <- data %>%
#   mutate_if(is.numeric, round, digits = 4)



##############################################################################################################
##############################################################################################################
## Create dataframe summarizing risk profile info, prevalence of risk profile in general population,
##    and prevalence of risk profile in deceased population
##############################################################################################################
##############################################################################################################

data.pop.prev <- data.FULL
#data.pop.prev <- data #data[,c(1,10:18)]
#names(data.pop.prev)[names(data.pop.prev) == "LA County"] <- "pop.prev"
names(data.pop.prev)[names(data.pop.prev) == "freq.I.Apr.20"] <- "I.prev.Apr.20"
names(data.pop.prev)[names(data.pop.prev) == "freq.I.Jul.20"] <- "I.prev.Jul.20"

data.D.prev <- dataD #dataD[,c(9:10)]
#names(data.D.prev)[names(data.D.prev) == "LA County"] <- "D.prev"
names(data.D.prev)[names(data.D.prev) == "Apr.20"] <- "D.prev.Apr.20"
names(data.D.prev)[names(data.D.prev) == "Jul.20"] <- "D.prev.Jul.20"

data.prev <- merge(data.pop.prev, data.D.prev, by = "Profile")
data.prev <- arrange(data.prev, data.prev$Profile)


##############################################################################################################
## Function for risk table with CFR and IFR
##############################################################################################################

risk.table.CFR.IFR.dates <- function(ABC.out.mat, time.steps, par.vec.length, iter, data.prev) {

  ## MODEL OUTPUT TO PLOT

  #print("Starting model.out calc")

  model.out <- correlated.param.SIM(ABC.out.mat=ABC.out.mat, iter=iter,time.step=time.steps) #(ABC.out.mat[1:par.vec.length,],iter=iter,time.steps=time.steps, startObservedData = 0, scenario=4, intervention_date="2020-03-12",sd.redux=NULL)

  # Align dates
  init.date = as.Date("2020-03-01")
  model.out[["date"]] <- model.out[["date"]] + init.date
  model.out.Apr.20 <- model.out %>% filter(date==c(as.Date("2020-04-20")))
  model.out.Jul.20 <- model.out %>% filter(date==c(as.Date("2020-07-20")))

  #   ## FILTER OUTPUT TO TODAY'S DATE
  # dates.to.filter <- c(as.Date("2020-04-20"), as.Date("2020-07-20")) #c(as.Date("2020-04-20")) #Sys.Date() #c("2020-04-01")
  # filter.dates = TRUE
  # # Filter if
  # if (filter.dates==TRUE){
  #   ## Filter to today's date
  #   model.out <- model.out %>% filter(date==c(dates.to.filter))
  # }

  ########################################
  ## Get CFR and IFR for each iteration
  ########################################

  ## Function to multiply each value of I / D by the prevalence of each profile
  prev.multiply <- function(model.out, model.var, data.prev, prev.var) {
    prev.vec <- data.prev[,prev.var]
    var.matrix <- matrix(model.out[,model.var], nrow=nrow(model.out), ncol=length(prev.vec), byrow = FALSE)  ## Replicate the variable (I or D) n.profiles times
    out.prev <- sweep(var.matrix, MARGIN=2, prev.vec, `*`)
    return(out.prev)
  }

  n.profiles <- nrow(data.prev)

  I.prev.Apr.20 <- prev.multiply(model.out.Apr.20, "Idetectcum", data.prev, "I.prev.Apr.20")
  Itot.prev.Apr.20 <- prev.multiply(model.out.Apr.20, "Itotcum", data.prev, "I.prev.Apr.20")
  D.prev.Apr.20 <- prev.multiply(model.out.Apr.20, "D", data.prev, "D.prev.Apr.20")

  I.prev.Jul.20 <- prev.multiply(model.out.Jul.20, "Idetectcum", data.prev, "I.prev.Jul.20")
  Itot.prev.Jul.20 <- prev.multiply(model.out.Jul.20, "Itotcum", data.prev, "I.prev.Jul.20")
  D.prev.Jul.20 <- prev.multiply(model.out.Jul.20, "D", data.prev, "D.prev.Jul.20")

  CFR.Apr.20 <- D.prev.Apr.20 / I.prev.Apr.20
  CFR.Apr.20[!is.finite(CFR.Apr.20)] <- 0
  CFR.Apr.20[CFR.Apr.20>1] <- 1
  colnames(CFR.Apr.20) <- paste0("CFR.Apr.20.", c(1:n.profiles))

  IFR.Apr.20 <- D.prev.Apr.20 / Itot.prev.Apr.20
  IFR.Apr.20[!is.finite(IFR.Apr.20)] <- 0
  colnames(IFR.Apr.20) <- paste0("IFR.Apr.20.", c(1:n.profiles))

  CFR.Jul.20 <- D.prev.Jul.20 / I.prev.Jul.20
  CFR.Jul.20[!is.finite(CFR.Jul.20)] <- 0
  CFR.Jul.20[CFR.Jul.20>1] <- 1
  colnames(CFR.Jul.20) <- paste0("CFR.Jul.20.", c(1:n.profiles))

  IFR.Jul.20 <- D.prev.Jul.20 / Itot.prev.Jul.20
  IFR.Jul.20[!is.finite(IFR.Jul.20)] <- 0
  colnames(IFR.Jul.20) <- paste0("IFR.Jul.20.", c(1:n.profiles))

  ########################################
  ## Get CI for CFR and IFR
  ########################################

  get.CI.CFR.IFR <- function(model.out, CFR, IFR){

    ## Combine into a single dataframe
    traj <- as.data.frame(cbind(model.out[,1:4],CFR,IFR))
    melt.traj <- reshape2::melt(traj, measure.vars = c(5:ncol(traj)), variable.name = "Rate.Profile")
    traj.dt <- as.data.table(melt.traj)

    ## TO SAVE MEMORY
    rm(model.out)

    #print("Starting CI calc")

    traj.CI <- traj.dt[, list(
      median = quantile(value, c(.5), na.rm=TRUE),
      low_95 = quantile(value, c(.025), na.rm=TRUE),
      up_95 = quantile(value, c(.975), na.rm=TRUE)),
      by = c("date", "Rate.Profile")]
    traj.CI <- as.data.frame(traj.CI) %>% mutate_if(is.numeric, round, digits=5)

    return(traj.CI)
  }

  traj.CI.Apr.20 <- get.CI.CFR.IFR(model.out=model.out.Apr.20, CFR=CFR.Apr.20, IFR=IFR.Apr.20)
  traj.CI.Jul.20 <- get.CI.CFR.IFR(model.out=model.out.Jul.20, CFR=CFR.Jul.20, IFR=IFR.Jul.20)

  ## Separate out into CFR and IFR

  separate.name.CFR.IFR <- function(traj.CI, CFR=TRUE, n.profiles){
    if (CFR==TRUE){
      CI.profiles <- traj.CI[1:n.profiles,c(3:5)]
      colnames(CI.profiles) <- paste0("CFR.",colnames(CI.profiles))
      CI.profiles$Profile <- c(1:n.profiles)
    }
    else if (CFR==FALSE){
      CI.profiles <- traj.CI[(n.profiles+1):(2*n.profiles), c(3:5)]
      colnames(CI.profiles) <- paste0("IFR.", colnames(CI.profiles))
      CI.profiles$Profile <- c(1:n.profiles)
    }
    return(CI.profiles)
  }

  CFR.CI.Apr.20 <- separate.name.CFR.IFR(traj.CI.Apr.20, CFR=TRUE, n.profiles)
  IFR.CI.Apr.20 <- separate.name.CFR.IFR(traj.CI.Apr.20, CFR=FALSE, n.profiles)
  CFR.CI.Jul.20 <- separate.name.CFR.IFR(traj.CI.Jul.20, CFR=TRUE, n.profiles)
  IFR.CI.Jul.20 <- separate.name.CFR.IFR(traj.CI.Jul.20, CFR=FALSE, n.profiles)

  #  CFR.bind <- cbind(CFR.CI.Apr.20, CFR.CI.Jul.20)

  CFR.median.Apr.20 <- CFR.CI.Apr.20 %>% select(-c(CFR.low_95, CFR.up_95))
  CFR.median.Jul.20 <- CFR.CI.Jul.20 %>% select(-c(CFR.low_95, CFR.up_95))
  IFR.median.Apr.20 <- IFR.CI.Apr.20 %>% select(-c(IFR.low_95, IFR.up_95))
  IFR.median.Jul.20 <- IFR.CI.Jul.20 %>% select(-c(IFR.low_95, IFR.up_95))

  CFR.IFR <- as.data.frame(CFR.median.Apr.20$Profile)
  CFR.IFR$CFR.Apr.20 <- CFR.median.Apr.20$CFR.median
  CFR.IFR$CFR.Jul.20 <- CFR.median.Jul.20$CFR.median
  CFR.IFR$IFR.Apr.20 <- IFR.median.Apr.20$IFR.median
  CFR.IFR$IFR.Jul.20 <- IFR.median.Jul.20$IFR.median
  CFR.IFR[,1] <- NULL
  CFR.IFR$Profile <- CFR.median.Apr.20$Profile

  ########################################
  ## Create risk table
  ########################################

  risk_table_CFR <- data.prev # subset(data, select = c(Profile, riskprofile,  age, BMI, smoking, comorbidity, `LA County`))
  risk_table_CFR <- merge(risk_table_CFR, CFR.IFR, by = "Profile")

  # # FILTER PROFILES TO SHOW IN THE TABLE TO ONLY THOSE > 0 PREVALENCE
  # risk_table_CFR <- filter(risk_table_CFR, `I.prev.Apr.20` > 0.000000004)

  # ##### Arrange
  # risk_table_CFR <- arrange(risk_table_CFR, desc(risk_table_CFR$"CFR.Apr.20"))

  return(risk_table_CFR)

}

print("Clean data: Done.")
