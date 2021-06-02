###################################################################################################
## COVID INPUT DATA
# latest_data: cumulative counts for "Htotcum","D","Vcum","Idetectcum","H_new","D_new"
# no_obs: number of observation days
## Read and process the data

latest_covid_data <- function(truncate=0){
  cum_file <- sort(dir_ls(data.dir, regexp = "cum_counts21_030221.csv"), decreasing = TRUE)[1]
  latest_data = t(read.csv(cum_file, sep=",",stringsAsFactors = FALSE))
  colnames<-c("Htotcum","D","Vcum","Idetectcum","H_new","D_new","I_detect_new","V_new")
  nvars <- ncol(latest_data)
  colnames(latest_data) <- colnames
  latest_data <- as.data.frame(latest_data)
  latest_data <- latest_data[-1,]
  latest_data[1:nvars] <- sapply(latest_data[1:nvars],as.character)
  latest_data[1:nvars] <- sapply(latest_data[1:nvars],as.numeric)
  latest_data <- latest_data %>% dplyr::select(-V_new)
  no_obs <- nrow(latest_data)

  latest_data <- latest_data[c(1:(no_obs-truncate)),]

  ## Change date to number
  # step <- c(1:no_obs)
  # data <- cbind(step,latest_data)
  return(latest_data)
}


# latimes_readin <- function(){
#   
#   # load COVID county-level hospital data
#   hospital_readin <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/cdph-hospital-patient-county-totals.csv",
#                                                      stringsAsFactors = TRUE) )
#   hospital_readin <- hospital_readin %>% dplyr::filter(county=="Los Angeles")
#   hospital_readin$Htot <- hospital_readin$positive_patients + hospital_readin$suspected_patients
#   hospital_readin$Q <- hospital_readin$icu_positive_patients + hospital_readin$icu_suspected_patients
#   # hospital_readin$Htot <- hospital_readin$positive_patients 
#   # hospital_readin$Q <- hospital_readin$icu_positive_patients
#   hospital <- select(hospital_readin, c(date, Htot, Q))
#   hospital$date <- as.Date(hospital$date)
#   
#   # latest_data_dates <- latest_data
#   # latest_data_dates$date <- as.Date("2020-03-01") + 0:(nrow(latest_data)-1)
#   # 
#   # data_joined <- left_join(latest_data_dates, hospital, by="date", keep=FALSE)
#   
#   
#   # load COVID county-level case and death data
#   case_readin <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-county-totals.csv",
#                                                  stringsAsFactors = TRUE) )
#   case_readin <- case_readin %>% dplyr::filter(county=="Los Angeles")
#   case_readin$Idetectcum <- case_readin$confirmed_cases
#   case_readin$I_detect_new <- case_readin$new_confirmed_cases
#   case_readin$D <- case_readin$deaths
#   case_readin$D_new <- case_readin$new_deaths
#   cases <- select(case_readin, c(date,Idetectcum,I_detect_new,D,D_new))
#   cases$date <- as.Date(cases$date)
#   cases <- cases %>% dplyr::filter(date > "2020-02-29") %>% arrange(date)
#   
#   # smooth the outlier in new deaths over preceding 2 weeks
#   D.max.at <- which(cases$D_new>500)
#   D.max.cap <- 200
#   to.smooth.14 <- (cases$D_new[D.max.at] - D.max.cap)/14
#   cases$D_new[D.max.at] <- D.max.cap
#   cases$D_new[c((D.max.at-14):(D.max.at-1))] <- cases$D_new[c((D.max.at-14):(D.max.at-1))] + to.smooth.14
#   
#   # recalculate cumulative after smoothing new D
#   for (i in 2:nrow(cases)){
#     cases$D[i] <- cases$D[i-1] + cases$D_new[i]
#   }
#   
#   # join I and D with H and Q
#   la_data <- left_join(cases, hospital, by="date", keep=FALSE)
#   #la_data[is.na(la_data)] <- 0
#   
#   # la_data$I_detect_new = zoo::rollmean(la_data$I_detect_new, k = 7, fill = NA, align = 'right') %>% round(digits=0)
#   # la_data$D_new = zoo::rollmean(la_data$D_new, k = 7, fill = NA, align = 'right') %>% round(digits=0)
#   
#   return(la_data)
# }
# 
# #la_data <- latimes_readin()

########################################################################################
## PLOTTING INPUT
########################################################################################

### IF PLOTTING ALL VARIABLES
all.variables <- c("S",
                   "I_detect_new",
                   "I",
                   "Idetectcum",
                   "Itot",
                   "Itotcum",
                   "H_new",
                   "Htot",
                   "Htotcum",
                   "Q",
                   "Qcum",
                   "V",
                   "Vcum",
                   "D_new",
                   "D",
                   "R"
)

vars.plus.R <- all.variables

### IF ONLY PLOTTING VARIABLES AGAINST DATA
only.vars.with.data <- c(
  "I_detect_new",
  "Idetectcum",
  "H_new",
  "Htotcum",
  "Vcum",
  "D",
  "D_new"
)

########################################################################################
## HELPER FUNCTIONS

## CONFIDENCE INTERVALS
posterior.CI <- function(posterior.var, round.by=3){
  median = quantile(posterior.var, c(.5), na.rm=TRUE)
  low_95 = quantile(posterior.var, c(.025), na.rm=TRUE)
  low_50 = quantile(posterior.var, c(.25), na.rm=TRUE)
  mean = mean(posterior.var)
  up_50 = quantile(posterior.var, c(.75), na.rm=TRUE)
  up_95 = quantile(posterior.var, c(.975), na.rm=TRUE)
  posterior.CI <- as.data.frame(cbind(low_95,low_50,median,mean,up_50,up_95))
  posterior.CI <- round(posterior.CI, digits=round.by)
  return(posterior.CI)
}

get.CFR.IFR.by.date <- function(traj.CI, CFR.or.IFR, date.in, round.by=4){
  if (CFR.or.IFR=="CFR"){
    state.name.in="CFRobs"
  }
  if (CFR.or.IFR=="IFR"){
    state.name.in="CFRactual"
  }
  posterior.CI.out <- traj.CI %>% filter(date %in% as.Date(date.in))
  posterior.CI.out <- posterior.CI.out %>% filter(state.name==state.name.in) %>% select(-c(state.name,N)) %>% mutate_if(is.numeric, round, digits=round.by)
  return(posterior.CI.out)
}

# FORMAT AS "mean(low_95,up_95)"
posterior.CI.FORMAT <- function(var.CI,use.mean=1){
  var.CI <- as.data.frame(var.CI)
  low_95 <- var.CI$low_95
  mean <- var.CI$mean
  up_95 <- var.CI$up_95
  median <- var.CI$median
  if (use.mean==1){
    var.95.CI <- paste0(mean, " (", low_95, ",", up_95,")")
  }
  if (use.mean==0){
    var.95.CI <- paste0(median, " (", low_95, ",", up_95,")")
  }
  return(var.95.CI)
}

var.format <- function(var.CI,use.mean.select){
  n.idx <- nrow(var.CI)
  var.format = as.data.frame(matrix(nrow=n.idx, ncol=1))
  for (i in 1:n.idx){
    var.format[i,] <- posterior.CI.FORMAT(var.CI[i,],use.mean=use.mean.select)
  }
  return(var.format)
}


########################################################################################
## SPECIFYING EPIDEMIC MODEL TO BE SIMULATED AND SCENARIOS
########################################################################################
correlated.param.SIM <- function(week_par_sim,iter,time.steps) {
  
  #TEST.out <- vector("list",length(week_par_sim))
  
  num.iter.id = length(unique(week_par_sim$iter.id))
  TEST.out <- vector("list",length(week_par_sim))
  
  for (idx in 1:num.iter.id) {
    week_par_sim_idx <- week_par_sim %>% dplyr::filter(iter.id==idx)
    week_idx = length(unique(week_par_sim$week.id))-2
    
    Alpha_t <- c(0, seq(45,  week_idx*7+45, by = 7))
    Kappa_t <- c(0, seq(45,  week_idx*7+45, by = 7))
    Delta_t <- c(0, seq(45,  week_idx*7+45, by = 7))
    Beta_t <- c(0, seq(45,  week_idx*7+45, by = 7))
    r_t <- c(0, seq(45,  week_idx*7+45, by = 7))
    
    Alpha_y <- week_par_sim_idx$V4
    Kappa_y <- week_par_sim_idx$V5
    Delta_y <-week_par_sim_idx$V3
    r_y = week_par_sim_idx$V2
    p_V =   0.27 # week_par_sim_idx$V6
    R0_y = week_par_sim_idx$V1
    
    Br.function <- function(R0.in, r.in, Alpha.in){
      d_IH <- 10   #days between illness onset and hospitalization
      d_IR <- 7    #days between illness onset and recovery (hospitalization not required)
      Br <- R0.in * ( 1 / ( (r.in/ ((Alpha.in/d_IH) + ((1-Alpha.in)/d_IR)))  + (1-r.in)*d_IR ))
      return(Br)
    }
    
    Beta_y = as.vector(length(Beta_t))
    
    for (j in 1:length(R0_y)){
      Beta_y[j] = c(Br.function(R0.in<-R0_y[j], r.in<-r_y[j], Alpha_y[j]<-Alpha_y[j]))
    }
    
    
    ## COMPILE
    x <- seihqdr_generator(Alpha_t=Alpha_t, Alpha_y=Alpha_y, Kappa_t=Kappa_t, Kappa_y=Kappa_y, Delta_t=Delta_t, Delta_y=Delta_y, Beta_t=Beta_t, Beta_y, r_t=r_t, r_y=r_y, S_ini=1e7, E_ini=10, p_QV=p_V)
    
    ## SIMULATE
    TEST<-as.data.frame(plyr::rdply(iter, x$run(0:time.steps),.id="iter"))
    
    ## BIND INCLUDING OFFSETING OBSERVED DATA BY START DATE
    start_time = 45
    TEST.out[[idx]] <- cbind(data.frame(par.id = idx, date = -start_time+TEST$step), TEST)
  }
  
  ## ADD TO DATAFRAME OVER ALL PARAMETER VALUES
  TEST.out <- do.call(rbind, TEST.out)
  return(TEST.out)
  
}
########################################################################################
## GETTING MODEL OUTPUT + SUMMARY STATISTICS FUNCTION
########################################################################################
# num.to.sample <- 20
# ABC.out.mat <- ABC_out$param[1:num.to.sample,]
# par.vec.length <- num.to.sample
# iter <- 10
# time.steps <- 300
# vars.to.plot <- vars.plus.R

model.output.to.plot.SIM <- function(week_par_sim, iter, time.steps, vars.to.plot) {
  
  library(data.table)
  init.date.data="2020-03-01"
  
  ## MODEL OUTPUT TO PLOT
  TEST.out <- correlated.param.SIM(week_par_sim,iter=iter,time.steps=time.steps)
  
  ### Add CFR and IFR to the list (EXTRA STEP NOW THAT THIS IS BEING USED ALSO FOR summary_table)
  traj <- dplyr::mutate(TEST.out, Itot=I+A, CFRobs=(D/Idetectcum), CFRactual=(D/(Itotcum)) )
  traj <-  dplyr::select(traj,c(1:4,CFRobs,CFRactual,vars.to.plot))
  ###
  
  ## TO SAVE MEMORY
  rm(TEST.out)
  
  print("Starting CI calc")
  
  ### MELTING AND APPLYING SUMMARY STAT FUNCTIONS
  df.traj <- reshape2::melt(traj, measure.vars = c(5:ncol(traj)), variable.name = "state.name")
  df.traj_dt <- as.data.table(df.traj)
  
  traj.CI <- df.traj_dt[, list(
    N=.N,
    mean = mean(value),
    median = quantile(value, c(.5),na.rm=TRUE),
    low_95 = quantile(value, c(.025),na.rm=TRUE),
    up_95 = quantile(value, c(.975),na.rm=TRUE),
    up_50 = quantile(value,.75,na.rm=TRUE),
    low_50 = quantile(value,.25,na.rm=TRUE)),
    by = c("date", "state.name")]
  traj.CI <- as.data.frame(traj.CI)
  
  ## TO ALIGN DATES: MODEL
  init.date = init.date.data #"2020-01-03"
  init.date <- as.Date(init.date)
  traj.CI[["date"]] <- traj.CI[["date"]] + init.date
  
  return(traj.CI)
  
}

########################################################################################
########################################################################################
## FUNCTIONS FOR ABC
########################################################################################
########################################################################################

###################################################################################################
## "SUMMARY STATISTICS":
## The cumulative number of cases at all (trusted) time points
###################################################################################################

sum.stats.SIMTEST <- function(data){
  
  ss.I <- data$Idetectcum #[I.trust.n]
  ss.H <- data$Htotcum #[H.trust.n]
  ss.V <- data$Vcum #[V.trust.n]
  ss.D <- data$D #[D.trust.n]
  ss.Hnew <- data$H_new #[Hnew.trust.n]
  ss.Dnew <- data$D_new #[Dnew.trust.n]
  # ss.R <- data$R #[R.trust.n]
  
  
  # Which variables to consider
  summarystats = c(ss.I, ss.H, ss.V, ss.D, ss.Hnew, ss.Dnew)
  return(summarystats)
}


###################################################################################################
## SIMULATION MODEL FUNCTION TO COMPUTE FOR ABC ALGORITHM
## A function implementing the model to be simulated
## It must take as arguments a vector of model parameter values par
## and it must return a vector of summary statistics
###################################################################################################

model.1sim.stats.no.R <- function(par){
  
  start_time <- 45 #par[3]
  R0 <- max(par[1],2.5)
  r <- max(par[2],.01)
  #R0_redux <- par[3] #par[4]
  Delta <- max(par[3],.01)#par[5]
  Alpha <- max(par[4],.01)#par[6]
  Kappa <- max(par[5],.01) #par[7]
  p_V <- max(par[6],.01)#par[8]
  
  ################################### For week 3 later #####################################
  
  #if (i == week.no){
  # week 0, week0, week1 week 2...
  Alpha_t = c(0, seq(45,  i*7+45, by = 7))
  Kappa_t = c(0, seq(45,  i*7+45, by = 7))
  Beta_t = c(0, seq(45,  i*7+45, by = 7))
  Delta_t = c(0, seq(45,  i*7+45, by = 7))
  r_t = c(0, seq(45,  i*7+45, by = 7))
  
  #week.seq = seq(3, i, 1)
  
  ## 4.5.21: I changed the 3rd entry of the sequences below from week_par_mean[2,3] to week_par_mean[1,3]
  Delta_y = c(week_par_mean[1,3]) # Delta
  Kappa_y = c(week_par_mean[1,5]) #Kappa
  Alpha_y = c(week_par_mean[1,4]) #Alpha
  r_y = c(week_par_mean[1,2]) #r
  p_QV = c(week_par_mean[1,6]) #p_V
  R0_y <- c(week_par_mean[1,1]) #  R0

  for (j in 1:i){
    Delta_y = append(Delta_y, week_par_mean[j,3])
    Kappa_y = append(Kappa_y, week_par_mean[j,5])
    Alpha_y = append(Alpha_y, week_par_mean[j,4])
    r_y = append(r_y, week_par_mean[j,2])
    p_QV = append(p_QV, week_par_mean[j,6])
    R0_y = append(R0_y, week_par_mean[j,1])
  }
  
  Delta_y = append(Delta_y, Delta)
  Kappa_y = append(Kappa_y, Kappa)
  Alpha_y = append(Alpha_y, Alpha)
  r_y = append(r_y, r)
  p_QV = append(p_QV, p_V)
  R0_y = append(R0_y, R0)
  
  #print("Alpha_t")
  #print(Alpha_t)
  #print("R0_y")
  #print(R0_y)
  
  Br.function <- function(R0.in, r.in, Alpha.in){
    d_IH <- 10   #days between illness onset and hospitalization
    d_IR <- 7    #days between illness onset and recovery (hospitalization not required)
    Br <- R0.in * ( 1 / ( (r.in/ ((Alpha.in/d_IH) + ((1-Alpha.in)/d_IR)))  + (1-r.in)*d_IR ))
    return(Br)
  }
  
  Beta_y = as.vector(length(Beta_t))
  
  for (j in 1:length(R0_y)){
    Beta_y[j] = c(Br.function(R0.in<-R0_y[j], r.in<-r_y[j], Alpha_y[j]<-Alpha))
  }
  
  #print(Beta_t)
  #print(Beta_y)
  #print(R0_y)
  
  ### GENERATE SIMULATION
  x <- seihqdr_generator(Alpha_t=Alpha_t, Alpha_y=Alpha_y, Kappa_t=Kappa_t, Kappa_y=Kappa_y, Delta_t=Delta_t, Delta_y=Delta_y, Beta_t=Beta_t, Beta_y=Beta_y, r_t=r_t, r_y=r_y, S_ini=1e7, E_ini=10, p_QV=p_V)
  
  st <- start_time
  last_date <- max(Beta_t)
  one_sim <- as.data.frame(x$run(1:last_date)[(st):(last_date),])
  #one_sim <- as.data.frame(x$run(0:(st+no_obs))[(st+1):(st+no_obs),])
  
  ### SUMMARY STATISTICS COMPUTED ON MODEL OUTPUT:
  summarymodel <- sum.stats.SIMTEST(one_sim) #last_date_data = "2021-01-18")
  
  return(summarymodel)
  #}
}


