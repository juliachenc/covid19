############################################################################################################
############################################################################################################
## CLEAN AND PROCESS DATA FOR RISK ESITMATES
############################################################################################################
############################################################################################################

## 1) RENAME VARIABLES #############################################################################

#Change the profile numbers to factors and name the variable
Profile <- factor(seq(1,nrow(data)))
data <- cbind(data,Profile)

## dataH ##

#Change the profile numbers to factors and name the variable
dataH <- cbind(dataH,Profile)

#Drop variables we don't care about (they are included in the whole dataset, this will avoid dupliactes)
dataH$Age0.19 <- NULL
dataH$Age20.44 <- NULL
dataH$Age45.64 <- NULL
dataH$Age65. <- NULL
dataH$ObesityBMI.30 <- NULL
dataH$ObesityBMI30.40 <- NULL
dataH$ObesityBMI.40 <- NULL
dataH$SmokingYes <- NULL
dataH$ComorbidityYes <- NULL

#melt data
dataH.melted <- melt(dataH, id = 'Profile')
dataH.melted$SPA.Name <- dataH.melted$variable
dataH.melted$variable <- NULL
dataH.melted$SPA.Name <- factor(dataH.melted$SPA.Name, levels =  c("Antelope Valley", "San Fernando", "San Gabriel", "Metro", "West", "South", "East", "South Bay", "LA County"))
dataH.melted$prevalence.H <- dataH.melted$value
dataH.melted$value <- NULL

## dataQ ##

#Change the profile numbers to factors and name the variable
dataQ <- cbind(dataQ,Profile)

#Drop variables we don't care about (they are included in the whole dataset, this will avoid dupliactes)
dataQ$Age0.19 <- NULL
dataQ$Age20.44 <- NULL
dataQ$Age45.64 <- NULL
dataQ$Age65. <- NULL
dataQ$ObesityBMI.30 <- NULL
dataQ$ObesityBMI30.40 <- NULL
dataQ$ObesityBMI.40 <- NULL
dataQ$SmokingYes <- NULL
dataQ$ComorbidityYes <- NULL

#melt data
dataQ.melted <- melt(dataQ, id = 'Profile')
dataQ.melted$SPA.Name <- dataQ.melted$variable
dataQ.melted$variable <- NULL
dataQ.melted$SPA.Name <- factor(dataQ.melted$SPA.Name, levels =  c("Antelope Valley", "San Fernando", "San Gabriel", "Metro", "West", "South", "East", "South Bay", "LA County"))
dataQ.melted$prevalence.Q <- dataQ.melted$value
dataQ.melted$value <- NULL

## dataD ##

#Change the profile numbers to factors and name the variable
dataD <- cbind(dataD,Profile)

#Drop variables we don't care about (they are included in the whole dataset, this will avoid dupliactes)
dataD$Age0.19 <- NULL
dataD$Age20.44 <- NULL
dataD$Age45.64 <- NULL
dataD$Age65. <- NULL
dataD$ObesityBMI.30 <- NULL
dataD$ObesityBMI30.40 <- NULL
dataD$ObesityBMI.40 <- NULL
dataD$SmokingYes <- NULL
dataD$ComorbidityYes <- NULL

#melt data
dataD.melted <- melt(dataD, id = 'Profile')
dataD.melted$SPA.Name <- dataD.melted$variable
dataD.melted$variable <- NULL
dataD.melted$SPA.Name <- factor(dataD.melted$SPA.Name, levels =  c("Antelope Valley", "San Fernando", "San Gabriel", "Metro", "West", "South", "East", "South Bay", "LA County"))
dataD.melted$prevalence.D <- dataD.melted$value
dataD.melted$value <- NULL

## PrH ##

#Change the profile numbers to factors and name the variable
Pr.H <- cbind(Pr.H,Profile)

#Change the name of the variable "Pr" to a more detailed name
Pr.H$Pr.H <- Pr.H$Pr
Pr.H$Pr <- NULL

#Drop variables we don't care about (they are included in the whole dataset, this will avoid dupliactes)
Pr.H$Age0.19 <- NULL
Pr.H$Age20.44 <- NULL
Pr.H$Age45.64 <- NULL
Pr.H$Age65. <- NULL
Pr.H$ObesityBMI.30 <- NULL
Pr.H$ObesityBMI30.40 <- NULL
Pr.H$ObesityBMI.40 <- NULL
Pr.H$SmokingYes <- NULL
Pr.H$ComorbidityYes <- NULL

## PrQ ##

#Change the profile numbers to factors and name the variable
Pr.Q <- cbind(Pr.Q,Profile)

#Change the name of the variable "Pr" to a more detailed name
Pr.Q$Pr.Q <- Pr.Q$Pr
Pr.Q$Pr <- NULL

#Drop variables we don't care about (they are included in the whole dataset, this will avoid dupliactes)
Pr.Q$Age0.19 <- NULL
Pr.Q$Age20.44 <- NULL
Pr.Q$Age45.64 <- NULL
Pr.Q$Age65. <- NULL
Pr.Q$ObesityBMI.30 <- NULL
Pr.Q$ObesityBMI30.40 <- NULL
Pr.Q$ObesityBMI.40 <- NULL
Pr.Q$SmokingYes <- NULL
Pr.Q$ComorbidityYes <- NULL

## PrD ##

#Change the profile numbers to factors and name the variable
Pr.D <- cbind(Pr.D,Profile)

Pr.D$Pr.D <- Pr.D$Pr
Pr.D$Pr <- NULL

#Drop variables we don't care about (they are included in the whole dataset, this will avoid dupliactes)
Pr.D$Age0.19 <- NULL
Pr.D$Age20.44 <- NULL
Pr.D$Age45.64 <- NULL
Pr.D$Age65. <- NULL
Pr.D$ObesityBMI.30 <- NULL
Pr.D$ObesityBMI30.40 <- NULL
Pr.D$ObesityBMI.40 <- NULL
Pr.D$SmokingYes <- NULL
Pr.D$ComorbidityYes <- NULL


## 2) DUMMY VARIABLES <- LEVELS ##################################################################

data$age <- "Blank"
data$age[data$Age0.19==1] <- "0-19"
data$age[data$Age20.44==1] <- "20-44"
data$age[data$Age45.64==1] <- "45-64"
data$age[data$Age65.==1] <- "65+"
data$Age0.19 <- NULL
data$Age20.44 <- NULL
data$Age45.64 <- NULL
data$Age65. <- NULL
data$age <- factor(data$age, levels =  c("0-19", "20-44", "45-64", "65+"))

data$BMI <- "Blank"
data$BMI[data$ObesityBMI.30==1] <- "BMI<30"
data$BMI[data$ObesityBMI30.40==1] <- "30<BMI<40"
data$BMI[data$ObesityBMI.40==1] <- "BMI>40"
data$ObesityBMI.30 <- NULL
data$ObesityBMI30.40 <- NULL
data$ObesityBMI.40 <- NULL

data$BMI <- factor(data$BMI, levels =  c("BMI<30", "30<BMI<40", "BMI>40"))

data$smoking <- "Blank"
data$smoking[data$SmokingYes==1] <- "Smoker"
data$smoking[data$SmokingYes==0] <- "Non Smoker"
data$smoking <- factor(data$smoking, levels = c("Smoker", "Non Smoker"))

data$SmokingYes <- NULL

data$comorbidity <- "Blank"
data$comorbidity[data$ComorbidityYes==1] <- "Comorbidity"
data$comorbidity[data$ComorbidityYes==0] <- "No Comorbidity"
data$comorbidity <- factor(data$comorbidity, levels = c("Comorbidity", "No Comorbidity"))

data$ComorbidityYes <- NULL


## 3) MERGE DATASETS ##################################################################

data <- merge(data, Pr.H, by = "Profile")
data <- merge(data, Pr.Q, by = "Profile")
data <- merge(data, Pr.D, by = "Profile")
## NEW STEP TO ADD P(D|I)
data$Pr.D.I <- data$Pr.H * data$Pr.Q * data$Pr.D

## 4) FILTER PROFILES TO SHOW IN THE TABLE TO ONLY THOSE > 0 PREVALENCE

#data <- filter(data, `LA County` > 0.0004)

## 4) Create Risk Profile Groupings ##################################################################
### NOTE: Grouping done by death risk level
data$riskprofile <- "Blank"
data$riskprofile[data$Pr.D.I<.00099] <- "Risk 5"
data$riskprofile[(data$Pr.D.I<.01) & (data$Pr.D.I>.00099)] <- "Risk 4"
data$riskprofile[(data$Pr.D.I<.03) & (data$Pr.D.I>.01)] <- "Risk 3"
data$riskprofile[(data$Pr.D.I<.06) & (data$Pr.D.I>.03)] <- "Risk 2"
data$riskprofile[(data$Pr.D.I>.07)] <- "Risk 1"
# data$riskprofile[data$Pr.D<.01] <- "Risk 5"
# data$riskprofile[(data$Pr.D<.07) & (data$Pr.D>.01)] <- "Risk 4"
# data$riskprofile[(data$Pr.D<.14) & (data$Pr.D>.07)] <- "Risk 3"
# data$riskprofile[(data$Pr.D<.24) & (data$Pr.D>.14)] <- "Risk 2"
# data$riskprofile[(data$Pr.D>.24)] <- "Risk 1"
data$riskprofile <- factor(data$riskprofile, levels = c("Risk 1", "Risk 2", "Risk 3", "Risk 4", "Risk 5" ))

## Round all numeric variable to 4 digits
data <- data %>%
  mutate_if(is.numeric, round, digits = 4)



##############################################################################################################
##############################################################################################################
## Create dataframe summarizing risk profile info, prevalence of risk profile in general population,
##    and prevalence of risk profile in deceased population
##############################################################################################################
##############################################################################################################

data.pop.prev <- data[,c(1,10:18)]
names(data.pop.prev)[names(data.pop.prev) == "LA County"] <- "pop.prev"

data.D.prev <- dataD[,c(9:10)]
names(data.D.prev)[names(data.D.prev) == "LA County"] <- "D.prev"

data.prev <- merge(data.pop.prev, data.D.prev, by = "Profile")
data.prev <- arrange(data.prev, data.prev$Profile)


##############################################################################################################
## Function for risk table with CFR and IFR
##############################################################################################################

risk.table.CFR.fn <- function(ABC.out.mat, time.steps, par.vec.length, iter, data.prev) {

  ## MODEL OUTPUT TO PLOT

  #print("Starting model.out calc")

  model.out <- correlated.param.SIM(ABC.out.mat[1:par.vec.length,],iter=iter,time.steps=time.steps, startObservedData = 0, scenario=4, intervention_date="2020-03-12",sd.redux=NULL)

  ## FILTER OUTPUT TO TODAY'S DATE
  dates.to.filter <- Sys.Date() #c("2020-04-01")
  filter.dates = TRUE

  # Align dates
  init.date = "2020-01-03"
  init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
  model.out[["date"]] <- model.out[["date"]] + init.date

  # Filter if
  if (filter.dates==TRUE){
    ## Filter to today's date
    model.out <- model.out %>% filter(date==c(dates.to.filter))
  }

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

  I.prev <- prev.multiply(model.out, "Idetectcum", data.prev, "pop.prev")
  Itot.prev <- prev.multiply(model.out, "Itotcum", data.prev, "pop.prev")
  D.prev <- prev.multiply(model.out, "D", data.prev, "D.prev")

  CFR <- D.prev / I.prev
  CFR[!is.finite(CFR)] <- 0
  CFR[CFR>1] <- 1
  colnames(CFR) <- paste0("CFR.", c(1:n.profiles))

  IFR <- D.prev / Itot.prev
  IFR[!is.finite(IFR)] <- 0
  colnames(IFR) <- paste0("IFR.", c(1:n.profiles))

  ########################################
  ## Get CI for CFR and IFR
  ########################################

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
  traj.CI <- as.data.frame(traj.CI) %>% mutate_if(is.numeric, round, digits=4)

  ## Separate out into CFR and IFR

  CFR.CI <- traj.CI[1:n.profiles,c(3:5)]
  colnames(CFR.CI) <- paste0("CFR.",colnames(CFR.CI))
  CFR.CI$Profile <- c(1:n.profiles)

  IFR.CI <- traj.CI[(n.profiles+1):(2*n.profiles), c(3:5)]
  colnames(IFR.CI) <- paste0("IFR.", colnames(IFR.CI))
  IFR.CI$Profile <- c(1:n.profiles)

  ########################################
  ## Create risk table
  ########################################

  risk_table_CFR <- subset(data, select = c(Profile, riskprofile,  age, BMI, smoking, comorbidity, `LA County`))
  risk_table_CFR <- merge(risk_table_CFR, CFR.CI, by = "Profile")
  risk_table_CFR <- merge(risk_table_CFR, IFR.CI, by = "Profile")

  # FILTER PROFILES TO SHOW IN THE TABLE TO ONLY THOSE > 0 PREVALENCE
  risk_table_CFR <- filter(risk_table_CFR, `LA County` > 0.0004)

  ##### Arrange
  risk_table_CFR <- arrange(risk_table_CFR, desc(risk_table_CFR$"CFR.median"))

  return(risk_table_CFR)

}


