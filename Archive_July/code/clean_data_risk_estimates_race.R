############################################################################################################
############################################################################################################
## CLEAN AND PROCESS DATA FOR RISK ESITMATES - RACE/ETHNICITY
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
dataH.melted$race.Name <- dataH.melted$variable
dataH.melted$variable <- NULL
dataH.melted$race.Name <- factor(dataH.melted$race.Name, levels =  c(rownames(race.data)))
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
dataQ.melted$race.Name <- dataQ.melted$variable
dataQ.melted$variable <- NULL
dataQ.melted$race.Name <- factor(dataQ.melted$race.Name, levels =  c(rownames(race.data)))
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
dataD.melted$race.Name <- dataD.melted$variable
dataD.melted$variable <- NULL
dataD.melted$race.Name <- factor(dataD.melted$race.Name, levels =  c(rownames(race.data)))
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


## 4) Create Risk Profile Groupings ##################################################################
### NOTE: Grouping done by death risk level
data$riskprofile <- "Blank"
data$riskprofile[data$Pr.D<.01] <- "Risk 5"
data$riskprofile[(data$Pr.D<.07) & (data$Pr.D>.01)] <- "Risk 4"
data$riskprofile[(data$Pr.D<.14) & (data$Pr.D>.07)] <- "Risk 3"
data$riskprofile[(data$Pr.D<.24) & (data$Pr.D>.14)] <- "Risk 2"
data$riskprofile[(data$Pr.D>.24)] <- "Risk 1"
# data$riskprofile[data$Pr.D<.01] <- "Risk 5"
# data$riskprofile[(data$Pr.D<.09) & (data$Pr.D>.01)] <- "Risk 4"
# data$riskprofile[(data$Pr.D<.19) & (data$Pr.D>.09)] <- "Risk 3"
# data$riskprofile[(data$Pr.D<.3) & (data$Pr.D>.19)] <- "Risk 2"
# data$riskprofile[(data$Pr.D>.3)] <- "Risk 1"
data$riskprofile <- factor(data$riskprofile, levels = c("Risk 1", "Risk 2", "Risk 3", "Risk 4", "Risk 5" ))

## Round all numeric variable to 3 digits
data <- data %>%
  mutate_if(is.numeric, round, digits = 3)

