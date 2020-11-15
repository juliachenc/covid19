
########################################################################################
## SPECIFYING EPIDEMIC MODEL TO BE SIMULATED AND SCENARIOS
########################################################################################

## 1 SCENARIO 0: R0 DO NOTHING (NO INTERVENTIONS: PROJECTION USING MODEL ESTIMATED PARAMETERS)
## 2 SCENARIO 1: R0 DO NOTHING + SHIELD SPECIFIC POPULATIONS [Trump/Atlas Scenario]
## 3 SCENARIO 2: STAGE 3 SELF-ADAPTIVE R0
## 4 SCENARIO 3A: STAGE 3 SELF-ADAPTIVE R0 + SHEILD SPECIFIC POPULATIONS
## 5 SCENARIO 3B: STAGE 3 SELF-ADAPTIVE R0 + SHEILD SPECIFIC POPULATIONS at 50%
## 6 SCENARIO 6: OBSERVED TREND
## 7 SCENARIO 7A: OBSERVED TREND Rt + SHIELD SPECIFIC POPULATIONS
## 8 SCENARIO 7B: OBSERVED TREND Rt + SHIELD SPECIFIC POPULATIONS at 50%

## 9 SCENARIO 4: CONTINUE TOTAL LOCKDOWN 1 MONTH LONGER
## 10 SCENARIO 5: CONTINUE TOTAL LOCKDOWN 1 MONTH LONGER + SHIELD SPECIFIC POPULATIONS



subpop.scenarios.integrated <- function(ABC.out.mat,iter,time.steps,scenario=NULL,Alpha.Kappa.Delta=NULL, percent.idx=NULL, S_ini.USER) {

  Alpha1.FILTER <- Alpha.Kappa.Delta[percent.idx,1]
  Alpha2.FILTER <- Alpha.Kappa.Delta[percent.idx,4]
  Kappa1.FILTER <- Alpha.Kappa.Delta[percent.idx,2]
  Kappa2.FILTER <- Alpha.Kappa.Delta[percent.idx,5]
  Delta1.FILTER <- Alpha.Kappa.Delta[percent.idx,3]
  Delta2.FILTER <- Alpha.Kappa.Delta[percent.idx,6]

  Alpha_decrease_date <- as.numeric( as.Date("2020-06-15") - as.Date("2020-03-01"))
  Alpha_stabilize_date <- as.numeric( as.Date("2020-07-01") - as.Date("2020-03-01"))

  Alpha_y_FILTER<-c(
    Alpha1.FILTER, # 0,
    Alpha1.FILTER, # Alpha_decrease_date-1,
    Alpha1.FILTER, # Alpha_decrease_date,
    Alpha2.FILTER, # Alpha_stabilize_date,
    Alpha2.FILTER # Alpha_stabilize_date + 400
  )
  Kappa_y_FILTER<-c(
    Kappa1.FILTER, # 0,
    Kappa1.FILTER, # Alpha_decrease_date-1,
    Kappa1.FILTER, # Alpha_decrease_date,
    Kappa2.FILTER, # Alpha_stabilize_date,
    Kappa2.FILTER # Alpha_stabilize_date + 400
  )
  Delta_y_FILTER<-c(
    Delta1.FILTER, # 0,
    Delta1.FILTER, # Alpha_decrease_date-1,
    Delta1.FILTER, # Alpha_decrease_date,
    Delta2.FILTER, # Alpha_stabilize_date,
    Delta2.FILTER # Alpha_stabilize_date + 400
  )

  # print("PRINT TEST2")
  # print(Alpha1.FILTER)
  # print(Alpha_y_FILTER)

  S_ini.INPUT <- S_ini.USER
  print(paste0("S_ini:", S_ini.INPUT))

  intervention_date <- as.Date(intervention_date)
  intervention_date_numeric <- as.numeric(intervention_date - as.Date("2020-03-01"))
  todays_date_numeric = as.numeric(Sys.Date() - as.Date("2020-03-01"))

  TEST.out <- vector("list", nrow(ABC.out.mat))

  for (i in 1:nrow(ABC.out.mat)) {

    ### PARAMETER ESTIMATES FROM ABC
    R0 <- ABC.out.mat[i,1]
    r <- ABC.out.mat[i,2]
    start_time <- round(ABC.out.mat[i,3])
    R0_redux1 <- ABC.out.mat[i,4]
    p_V <- ABC.out.mat[i,8]
    R0_redux2 <- ABC.out.mat[i,9]
    # Delta1 <- ABC.out.mat[i,5]
    # Alpha1 <- ABC.out.mat[i,6]
    # Kappa1 <- ABC.out.mat[i,7]
    # Delta2 <- ABC.out.mat[i,10]
    # Alpha2 <- ABC.out.mat[i,11]
    # Kappa2 <- ABC.out.mat[i,12]
    Alpha1 <- Alpha.Kappa.Delta[3,1]
    Alpha2 <- Alpha.Kappa.Delta[3,4]
    Kappa1 <- Alpha.Kappa.Delta[3,2]
    Kappa2 <- Alpha.Kappa.Delta[3,5]
    Delta1 <- Alpha.Kappa.Delta[3,3]
    Delta2 <- Alpha.Kappa.Delta[3,6]

    ## MODEL INPUTS REQUIRED HERE TO RECOMPILE THE MODEL WITH UPDATED BETA(T) FUNCTION
    d_IH <- 10   #days between illness onset and hospitalization
    d_IR <- 7    #days between illness onset and recovery (hospitalization not required)
    Br <- R0 * ( 1 / ( (r/ ((Alpha1/d_IH) + ((1-Alpha1)/d_IR)))  + (1-r)*d_IR ))

    # Alpha_decrease_date <- as.numeric( as.Date("2020-06-15") - as.Date("2020-03-01"))
    # Alpha_stabilize_date <- as.numeric( as.Date("2020-07-01") - as.Date("2020-03-01"))

    Alpha_t<-c(
      0,
      Alpha_decrease_date-1,
      Alpha_decrease_date,
      Alpha_stabilize_date,
      Alpha_stabilize_date + 400
    )
    Kappa_t <- Alpha_t
    Delta_t <- Alpha_t

    Alpha_y_OBS<-c(
      Alpha1, # 0,
      Alpha1, # Alpha_decrease_date-1,
      Alpha1, # Alpha_decrease_date,
      Alpha2, # Alpha_stabilize_date,
      Alpha2 # Alpha_stabilize_date + 400
    )
    Kappa_y_OBS<-c(
      Kappa1, # 0,
      Kappa1, # Alpha_decrease_date-1,
      Kappa1, # Alpha_decrease_date,
      Kappa2, # Alpha_stabilize_date,
      Kappa2 # Alpha_stabilize_date + 400
    )
    Delta_y_OBS<-c(
      Delta1, # 0,
      Delta1, # Alpha_decrease_date-1,
      Delta1, # Alpha_decrease_date,
      Delta2, # Alpha_stabilize_date,
      Delta2 # Alpha_stabilize_date + 400
    )

    # Alpha_decrease_date <- as.numeric( as.Date("2020-06-15") - as.Date("2020-03-01"))
    # Alpha_stabilize_date <- as.numeric( as.Date("2020-07-01") - as.Date("2020-03-01"))
    #
    # Alpha_y_FILTER<-c(
    #   Alpha1.FILTER, # 0,
    #   Alpha1.FILTER, # Alpha_decrease_date-1,
    #   Alpha1.FILTER, # Alpha_decrease_date,
    #   Alpha2.FILTER, # Alpha_stabilize_date,
    #   Alpha2.FILTER # Alpha_stabilize_date + 400
    # )
    # Kappa_y_FILTER<-c(
    #   Kappa1.FILTER, # 0,
    #   Kappa1.FILTER, # Alpha_decrease_date-1,
    #   Kappa1.FILTER, # Alpha_decrease_date,
    #   Kappa2.FILTER, # Alpha_stabilize_date,
    #   Kappa2.FILTER # Alpha_stabilize_date + 400
    # )
    # Delta_y_FILTER<-c(
    #   Delta1.FILTER, # 0,
    #   Delta1.FILTER, # Alpha_decrease_date-1,
    #   Delta1.FILTER, # Alpha_decrease_date,
    #   Delta2.FILTER, # Alpha_stabilize_date,
    #   Delta2.FILTER # Alpha_stabilize_date + 400
    # )
    #
    # print(Alpha1.FILTER)
    # print(Alpha_y_FILTER)

    ## SCENARIO 0: R0 DO NOTHING (NO INTERVENTIONS: PROJECTION USING MODEL ESTIMATED PARAMETERS)
    if (scenario==0){
      Beta_t<- c(
        0,
        (start_time),
        (start_time+todays_date_numeric+400))
      Beta_y<- Br*c(
        1,
        1,
        1)

      Alpha_y <- Alpha_y_OBS
      Kappa_y <- Kappa_y_OBS
      Delta_y <- Kappa_y_OBS

    }

    ## SCENARIO 1: R0 DO NOTHING + SHIELD SPECIFIC POPULATIONS
    else if (scenario==1){

      Rt_begins_to_drop_date <- as.numeric( as.Date("2020-03-12") - as.Date("2020-03-01"))
      Rt_stabilize_date <- as.numeric( as.Date("2020-03-20") - as.Date("2020-03-01"))

      Beta_t<- c(
        0,
        (start_time),
        (start_time+todays_date_numeric+400))
      Beta_y<- Br*c(
        1,
        1,
        1)

      Alpha_y <- Alpha_y_FILTER
      Kappa_y <- Kappa_y_FILTER
      Delta_y <- Kappa_y_FILTER

    }

    ## SCENARIO 2: STAGE 3 SELF-ADAPTIVE R0
    else if (scenario==2){
      Rt_begins_to_drop_date <- as.numeric( as.Date("2020-03-12") - as.Date("2020-03-01"))
      Rt_stabilize_date <- as.numeric( as.Date("2020-04-27") - as.Date("2020-03-01"))
      Beta_t<- c(
        0,
        Rt_begins_to_drop_date,
        Rt_stabilize_date,
        Rt_stabilize_date + 400
      )
      Beta_y<- c(
        Br, #0,
        Br, #Rt_begins_to_drop_date,
        Br*R0_redux2, #Rt_stabilize_date,
        Br*R0_redux2 #Rt_stabilize_date + 400
      )

      Alpha_y <- Alpha_y_OBS
      Kappa_y <- Kappa_y_OBS
      Delta_y <- Kappa_y_OBS
    }

    ## SCENARIO 3: STAGE 3 SELF-ADAPTIVE R0 + SHEILD SPECIFIC POPULATIONS
    else if (scenario==3){

      Rt_begins_to_drop_date <- as.numeric( as.Date("2020-03-12") - as.Date("2020-03-01"))
      Rt_stabilize_date <- as.numeric( as.Date("2020-04-27") - as.Date("2020-03-01"))

      Beta_t<- c(
        0,
        Rt_begins_to_drop_date,
        Rt_stabilize_date,
        Rt_stabilize_date + 400
      )
      Beta_y<- c(
        Br, #0,
        Br, #Rt_begins_to_drop_date,
        Br*R0_redux2, #Rt_stabilize_date,
        Br*R0_redux2 #Rt_stabilize_date + 400
      )
      Alpha_y <- Alpha_y_FILTER
      Kappa_y <- Kappa_y_FILTER
      Delta_y <- Kappa_y_FILTER

    }

    ## SCENARIO 4: CONTINUE TOTAL LOCKDOWN 1 MONTH LONGER
    else if (scenario==4){

      Alpha_y <- Alpha_y_OBS
      Kappa_y <- Kappa_y_OBS
      Delta_y <- Kappa_y_OBS

      Beta_t<- c(
        0,                              #1
        (start_time),                #2
        (start_time+1),                #3
        (start_time+intervention_date_numeric),   #2020-03-12    #4
        (start_time+intervention_date_numeric+15),#2020-03-27    #5
        start_time + as.Date("2020-05-27")-as.Date("2020-03-01"), #6 THIS IS THE STEP THAT MAKES LOCKDOWN 1 MONTH LONGER
        start_time+ as.Date("2020-07-04")-as.Date("2020-03-01"), #7
        (start_time+todays_date_numeric),   #CURRENT DATE         #8
        (start_time+todays_date_numeric+400))#DATE FAR IN FUTURE   #9

      Beta_y<- c(
        Br, #1
        Br, #2
        Br, #3
        Br, #4
        (Br*R0_redux1), #5
        (Br*R0_redux1), #6
        (Br*R0_redux2), #7
        (Br*R0_redux2*.9), #8
        (Br*R0_redux2*.9)) #9
    }

    ## SCENARIO 5: CONTINUE TOTAL LOCKDOWN 1 MONTH LONGER + SHIELD SPECIFIC POPULATIONS
    else if (scenario==5){
      Alpha_y <- Alpha_y_FILTER
      Kappa_y <- Kappa_y_FILTER
      Delta_y <- Kappa_y_FILTER

      Beta_t<- c(
        0,                              #1
        (start_time),                #2
        (start_time+1),                #3
        (start_time+intervention_date_numeric),   #2020-03-12    #4
        (start_time+intervention_date_numeric+15),#2020-03-27    #5
        start_time + as.Date("2020-05-27")-as.Date("2020-03-01"), #6 THIS IS THE STEP THAT MAKES LOCKDOWN 1 MONTH LONGER
        start_time+ as.Date("2020-07-04")-as.Date("2020-03-01"), #7
        (start_time+todays_date_numeric),   #CURRENT DATE         #8
        (start_time+todays_date_numeric+400))#DATE FAR IN FUTURE   #9

      Beta_y<- c(
        Br, #1
        Br, #2
        Br, #3
        Br, #4
        (Br*R0_redux1), #5
        (Br*R0_redux1), #6
        (Br*R0_redux2), #7
        (Br*R0_redux2*.9), #8
        (Br*R0_redux2*.9)) #9
    }

    ## SCENARIO 6: OBSERVED TREND
    else if (scenario==6){
      Alpha_y <- Alpha_y_OBS
      Kappa_y <- Kappa_y_OBS
      Delta_y <- Kappa_y_OBS

      Beta_t<- c(
        0,                              #1
        (start_time),                #2
        (start_time+1),                #3
        (start_time+intervention_date_numeric),   #2020-03-12    #4
        (start_time+intervention_date_numeric+15),#2020-03-27    #5
        (start_time+intervention_date_numeric+46),#2020-04-27    #6
        (start_time+intervention_date_numeric+114),#2020-07-04   #7
        (start_time+todays_date_numeric),   #CURRENT DATE         #8
        (start_time+todays_date_numeric+400))#DATE FAR IN FUTURE   #9

      Beta_y<- c(
        Br, #1
        Br, #2
        Br, #3
        Br, #4
        (Br*R0_redux1), #5
        (Br*R0_redux1), #6
        (Br*R0_redux2), #7
        (Br*R0_redux2*.9), #8
        (Br*R0_redux2*.9)) #9
    }

    ## SCENARIO 7: OBSERVED TREND + SHIELD SPECIFIC POPULATIONS
    else if (scenario==7){

      Alpha_y <- Alpha_y_FILTER
      Kappa_y <- Kappa_y_FILTER
      Delta_y <- Kappa_y_FILTER

      Beta_t<- c(
        0,                              #1
        (start_time),                #2
        (start_time+1),                #3
        (start_time+intervention_date_numeric),   #2020-03-12    #4
        (start_time+intervention_date_numeric+15),#2020-03-27    #5
        (start_time+intervention_date_numeric+46),#2020-04-27    #6
        (start_time+intervention_date_numeric+114),#2020-07-04   #7
        (start_time+todays_date_numeric),   #CURRENT DATE         #8
        (start_time+todays_date_numeric+400))#DATE FAR IN FUTURE   #9

      Beta_y<- c(
        Br, #1
        Br, #2
        Br, #3
        Br, #4
        (Br*R0_redux1), #5
        (Br*R0_redux1), #6
        (Br*R0_redux2), #7
        (Br*R0_redux2*.9), #8
        (Br*R0_redux2*.9)) #9
    }

    ## COMPILE
    x <- seihqdr_generator(Alpha_t=Alpha_t, Alpha_y=Alpha_y, Kappa_t=Kappa_t, Kappa_y=Kappa_y, Delta_t=Delta_t, Delta_y=Delta_y, Beta_t=Beta_t, Beta_y=Beta_y, S_ini=S_ini.INPUT, E_ini=10, r=r, p_QV=p_V)

    ## SIMULATE
    TEST<-as.data.frame(plyr::rdply(iter, x$run(0:time.steps),.id="iter"))

    ## BIND INCLUDING OFFSETING OBSERVED DATA BY START DATE
    TEST.out[[i]] <- cbind(data.frame(par.id = i, date = 0-start_time+TEST$step), TEST)
  }

  ## ADD TO DATAFRAME OVER ALL PARAMETER VALUES
  TEST.out <- do.call(rbind, TEST.out)

  return(TEST.out)

}


########################################################################################
## GETTING MODEL OUTPUT + SUMMARY STATISTICS FUNCTION
########################################################################################

model.output.to.plot.subpop.scenarios <- function(ABC.out.mat, par.vec.length, iter, time.steps, vars.to.plot, init.date.data="2020-03-01",scenario, Alpha.Kappa.Delta=NULL, percent.idx=NULL, S_ini.USER){

  library(data.table)

  ## MODEL OUTPUT TO PLOT
  TEST.out <- subpop.scenarios.integrated(ABC.out.mat[1:par.vec.length,],iter=iter,time.steps=time.steps, scenario=scenario, Alpha.Kappa.Delta, percent.idx, S_ini.USER)

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
  #init.date = init.date.data #"2020-01-03"
  #init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
  init.date <- as.Date("2020-03-01")
  traj.CI[["date"]] <- traj.CI[["date"]] + init.date

  return(traj.CI)

}


########################################################################################
## TESTING OUTPUT
########################################################################################

## 1 SCENARIO 0: R0 DO NOTHING (NO INTERVENTIONS: PROJECTION USING MODEL ESTIMATED PARAMETERS)
## 2 SCENARIO 1: R0 DO NOTHING + SHIELD SPECIFIC POPULATIONS [Trump/Atlas Scenario]
## 3 SCENARIO 2: STAGE 3 SELF-ADAPTIVE R0
## 4 SCENARIO 3A: STAGE 3 SELF-ADAPTIVE R0 + SHEILD SPECIFIC POPULATIONS
## 5 SCENARIO 3B: STAGE 3 SELF-ADAPTIVE R0 + SHEILD SPECIFIC POPULATIONS at 50%
## 6 SCENARIO 6: OBSERVED TREND
## 7 SCENARIO 7A: OBSERVED TREND Rt + SHIELD SPECIFIC POPULATIONS
## 8 SCENARIO 7B: OBSERVED TREND Rt + SHIELD SPECIFIC POPULATIONS at 50%

ABC.out.mat <- ABC_out$param[1:1000,]
par.vec.length <- 100
iter <- 20 #20
time.steps <- 200
vars.to.plot <- vars.plus.R
Alpha.Kappa.Delta <- weighted.avg.scenarios.overall

scenario = 0
percent.idx = NULL
S_ini.USER <- 1e7
traj.S1 <- model.output.to.plot.subpop.scenarios(ABC.out.mat=ABC.out.mat, par.vec.length=par.vec.length, iter=iter, time.steps=time.steps, vars.to.plot = vars.to.plot, scenario = scenario,
                                                 Alpha.Kappa.Delta=Alpha.Kappa.Delta,percent.idx=percent.idx,S_ini.USER=S_ini.USER)

scenario = 1
S_ini.USER <- .9e7
percent.idx = 1
traj.S2 <- model.output.to.plot.subpop.scenarios(ABC.out.mat=ABC.out.mat, par.vec.length=par.vec.length, iter=iter, time.steps=time.steps, vars.to.plot = vars.to.plot, scenario = scenario,
                                                 Alpha.Kappa.Delta=Alpha.Kappa.Delta,percent.idx=percent.idx,S_ini.USER=S_ini.USER)
scenario = 2
S_ini.USER <- 1e7
percent.idx = NULL
traj.S3 <- model.output.to.plot.subpop.scenarios(ABC.out.mat=ABC.out.mat, par.vec.length=par.vec.length, iter=iter, time.steps=time.steps, vars.to.plot = vars.to.plot, scenario = scenario,
                                                 Alpha.Kappa.Delta=Alpha.Kappa.Delta,percent.idx=percent.idx,S_ini.USER=S_ini.USER)
scenario = 3
S_ini.USER <- .9e7
percent.idx = 1
traj.S4 <- model.output.to.plot.subpop.scenarios(ABC.out.mat=ABC.out.mat, par.vec.length=par.vec.length, iter=iter, time.steps=time.steps, vars.to.plot = vars.to.plot, scenario = scenario,
                                                 Alpha.Kappa.Delta=Alpha.Kappa.Delta,percent.idx=percent.idx,S_ini.USER=S_ini.USER)
scenario = 3
S_ini.USER <- .95e7
percent.idx = 2
traj.S5 <- model.output.to.plot.subpop.scenarios(ABC.out.mat=ABC.out.mat, par.vec.length=par.vec.length, iter=iter, time.steps=time.steps, vars.to.plot = vars.to.plot, scenario = scenario,
                                                 Alpha.Kappa.Delta=Alpha.Kappa.Delta,percent.idx=percent.idx,S_ini.USER=S_ini.USER)
scenario = 6
S_ini.USER <- 1e7
percent.idx = NULL
traj.S6 <- model.output.to.plot.subpop.scenarios(ABC.out.mat=ABC.out.mat, par.vec.length=par.vec.length, iter=iter, time.steps=time.steps, vars.to.plot = vars.to.plot, scenario = scenario,
                                                 Alpha.Kappa.Delta=Alpha.Kappa.Delta,percent.idx=percent.idx,S_ini.USER=S_ini.USER)
scenario = 7
S_ini.USER <- .9e7
percent.idx = 1
traj.S7 <- model.output.to.plot.subpop.scenarios(ABC.out.mat=ABC.out.mat, par.vec.length=par.vec.length, iter=iter, time.steps=time.steps, vars.to.plot = vars.to.plot, scenario = scenario,
                                                 Alpha.Kappa.Delta=Alpha.Kappa.Delta,percent.idx=percent.idx,S_ini.USER=S_ini.USER)
scenario = 7
S_ini.USER <- .95e7
percent.idx = 2
traj.S8 <- model.output.to.plot.subpop.scenarios(ABC.out.mat=ABC.out.mat, par.vec.length=par.vec.length, iter=iter, time.steps=time.steps, vars.to.plot = vars.to.plot, scenario = scenario,
                                                 Alpha.Kappa.Delta=Alpha.Kappa.Delta,percent.idx=percent.idx,S_ini.USER=S_ini.USER)








#
#
# init.date.data="2020-03-01"
# plot.thru.curr.date = as.numeric(Sys.Date() - as.Date(init.date.data))
# time.steps.4.plot = 160 #175 #plot.thru.curr.date
# data.in <- data.in.tmp
# vars.to.plot <- vars.plus.R
#
# plot.SHIELD <-
#   plot.model.data.all(traj.CI = traj.SHIELD, data.in = data.in, init.date.data = "2020-03-01", date.offset.4plot = 15, time.steps.4plot=time.steps.4.plot,
#                       vars.to.plot=vars.plus.R)
# plot.SHIELD

