

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

correlated.param.SIM <- function(ABC.out.mat,iter,time.steps) {

  TEST.out <- vector("list", nrow(ABC.out.mat))

  for (idx in 1:nrow(ABC.out.mat)) {

    ### PARAMETER ESTIMATES FROM ABC

    R0 <- ABC.out.mat[idx,1]
    r1 <- ABC.out.mat[idx,2]
    start_time <- round(ABC.out.mat[idx,3])
    R0_redux <- ABC.out.mat[idx,4]
    Delta <- ABC.out.mat[idx,5]
    Alpha <- ABC.out.mat[idx,6]
    Kappa <- ABC.out.mat[idx,7]
    p_V <- ABC.out.mat[idx,8]



    ### BRING IN BETA_T ALPHA_T KAPPA_T DELTA_T FUNCTIONS
    fn_t_readin_code <- path(code.paper.dir, "fn_t_readin_code_FULL.R")
    source(fn_t_readin_code, local=TRUE)

    # print("Beta_t_dates")
    # print(Beta_t_dates)
    # print("R0_y")
    # print(R0_y)
    # print("r_t_dates")
    # print(r_t_dates)
    # print(r_y)
    # print("Alpha_t_dates")
    # print(Alpha_t_dates)

    ## COMPILE
    x <- seihqdr_generator(Alpha_t=Alpha_t, Alpha_y=Alpha_y, Kappa_t=Kappa_t, Kappa_y=Kappa_y, Delta_t=Delta_t, Delta_y=Delta_y, Beta_t=Beta_t, Beta_y=Beta_y, r_t=r_t, r_y=r_y, S_ini=1e7, E_ini=10, p_QV=p_V)

    ## SIMULATE
    TEST<-as.data.frame(plyr::rdply(iter, x$run(0:time.steps),.id="iter"))

    ## BIND INCLUDING OFFSETING OBSERVED DATA BY START DATE
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

model.output.to.plot.SIM <- function(ABC.out.mat, par.vec.length, iter, time.steps, vars.to.plot) {

  library(data.table)
  init.date.data="2020-03-01"

  ## MODEL OUTPUT TO PLOT
  TEST.out <- correlated.param.SIM(ABC.out.mat[1:par.vec.length,],iter=iter,time.steps=time.steps)

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

  # no_obs <- nrow(data)
  # last_date <- as.numeric(as.Date(last_date_data) - as.Date("2020-03-01"))
  #
  # # Which values of variables to consider
  # I.trust.n <- c(10:no_obs)  # The first 9 days of illness cases are unreliable/unavailable
  # H.trust.n <- c(17:last_date)  # The first 16 days of hospitalizations are unreliable/unavailable
  # V.trust.n <- c(19:last_date)  # The first 18 days of ventilation are unreliable/unavailable
  # D.trust.n <- c(18:no_obs)  # The first 17 days of mortality are unreliable/unavailable
  # Hnew.trust.n <- c(19:last_date) # The first 18 days of new hospitalizations are unreliable/unavailable
  # Dnew.trust.n <- c(28:no_obs) # The first 28 days of new deaths are unreliable/unavailable
  # R.trust.n <- c(0:35)

  ### For now let's take all values, instead of removing the first 10 to 28...we need these first 10 to 28 to fit the estimate parameters after 7 to 14 days!
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

  ### Here I'm getting the maximum between the value chosen by the ABC algorithm and 0.01. I do this because when we use the mean and stdev of the
  ### previous week's parameter values, we can get a stdev that ranges more widely than makes sense for the parameter. So we fix that by just
  ### ensuring that the minimum value the parameter can take is 0.01.
  R0 <- max(par[1],2.5)
  r <- max(par[2],.01)
  #R0_redux <- par[3] #par[4]
  Delta <- max(par[3],.01)#par[5]
  Alpha <- max(par[4],.01)#par[6]
  Kappa <- max(par[5],.01) #par[7]
  p_V <- max(par[6],.01)#par[8]

  # print("p_V")
  # print(p_V)

  #For week 2
  #Delta_t_dates <- c(Jan16, March1, March8)
  #Delta_t <- c(0, 45, 52)
  #Delta_y <- c(Delta[mean_week_1], Delta[mean_week_1], Delta1)
  #print(paste0("no_obs   ", no_obs))

  ################################### For week 2 #####################################
  Alpha_t = c(0, 45, 52, 59)
  Kappa_t = c(0, 45, 52, 59)
  Beta_t = c(0, 45, 52, 59)
  Delta_t = c(0, 45, 52, 59)
  r_t = c(0, 45, 52, 59)


  ## @JULIA FOUND AN ERROR 4.5.21: You're taking week_par_mean[2,3] but that is equal to 0! So you're setting a 0 value in the Parameter_y distributions!
  ## I fixed by setting days 0 - 52 equal to the values estimated for week 1, the only entry in week_par_mean
  # Delta_y = c(week_par_mean[1,3],week_par_mean[1,3], week_par_mean[2,3], Delta)
  # Kappa_y = c(week_par_mean[1,5],week_par_mean[1,5],week_par_mean[2,5], Kappa)
  # Alpha_y = c(week_par_mean[1,4],week_par_mean[1,4],week_par_mean[2,4], Alpha)
  # r_y = c(week_par_mean[1,2], week_par_mean[1,2], week_par_mean[2,2], r)
  # p_QV = c(week_par_mean[1,6], week_par_mean[1,6],week_par_mean[2,6], p_V)
  # #For week 1 - R0_t
  # R0_y <- c(week_par_mean[1,1],week_par_mean[1,1],week_par_mean[2,1], R0)

  Delta_y = c(week_par_mean[1,3],week_par_mean[1,3], week_par_mean[1,3], Delta)
  Kappa_y = c(week_par_mean[1,5],week_par_mean[1,5],week_par_mean[1,5], Kappa)
  Alpha_y = c(week_par_mean[1,4],week_par_mean[1,4],week_par_mean[1,4], Alpha)
  r_y = c(week_par_mean[1,2], week_par_mean[1,2], week_par_mean[1,2], r)
  p_QV = c(week_par_mean[1,6], week_par_mean[1,6],week_par_mean[1,6], p_V)
  #For week 1 - R0_t
  R0_y <- c(week_par_mean[1,1],week_par_mean[1,1],week_par_mean[1,1], R0)

  # print("p_QV")
  # print(p_QV)

  #--> apply Br.function to all values in R

  Br.function <- function(R0.in, r.in, Alpha.in){
    d_IH <- 10   #days between illness onset and hospitalization
    d_IR <- 7    #days between illness onset and recovery (hospitalization not required)
    Br <- R0.in * ( 1 / ( (r.in/ ((Alpha.in/d_IH) + ((1-Alpha.in)/d_IR)))  + (1-r.in)*d_IR ))
    return(Br)
  }

  #--> Beta_y
  Beta_y = as.vector(length(Beta_t))
  Beta_y =  c(Br.function(R0.in<-R0_y[1], r.in<-r, Alpha.in<-Alpha),
              Br.function(R0.in<-R0_y[2], r.in<-r, Alpha.in<-Alpha),
              Br.function(R0.in<-R0_y[3], r.in<-r, Alpha.in<-Alpha),
              Br.function(R0.in<-R0_y[4], r.in<-r, Alpha.in<-Alpha))


  # print(paste0("R0_y :    ", R0_y))
  # print(paste0("Beta_y :    ", Beta_y))
  # print(paste0("Beta_t :    ", Beta_t))
  # print(paste0("Alpha_y :    ", Alpha_y))
  # print(paste0("Alpha_t :    ", Alpha_t))
  # print(paste0("r_y :    ", r_y))
  # print(paste0("r_t :    ", r_t))
  ##################################################################################

  ### BRING IN BETA_T ALPHA_T KAPPA_T DELTA_T FUNCTIONS
  ####fn_t_readin_code <- path(code.paper.dir, "fn_t_readin_code_FULL.R")
  ####source(fn_t_readin_code, local=TRUE)

  # length.B <- length(Beta_y)
  # Beta_y[length.B] <- Beta_y[length.B]*0.9
  # Beta_y[length.B] <- Beta_y[length.B-1]*0.9

  ### GENERATE SIMULATION
  x <- seihqdr_generator(Alpha_t=Alpha_t, Alpha_y=Alpha_y, Kappa_t=Kappa_t, Kappa_y=Kappa_y, Delta_t=Delta_t, Delta_y=Delta_y, Beta_t=Beta_t, Beta_y=Beta_y, r_t=r_t, r_y=r_y, S_ini=1e7, E_ini=10, p_QV=p_V)

  st <- start_time
  last_date <- max(Beta_t)
  one_sim <- as.data.frame(x$run(1:last_date)[(st):(last_date),])
  #one_sim <- as.data.frame(x$run(0:(st+no_obs))[(st+1):(st+no_obs),])

  ### SUMMARY STATISTICS COMPUTED ON MODEL OUTPUT:
  summarymodel <- sum.stats.SIMTEST(one_sim) #last_date_data = "2021-01-18")

  return(summarymodel)
}
