
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
## SUMMARY TABLE
########################################################################################

summary.table <- function(ABC.out.mat, par.vec.length, iter, time.steps, init.date.data="2020-03-01") {

  library(data.table)

  ## MODEL OUTPUT TO PLOT
  TEST.out <- correlated.param.SIM(ABC.out.mat[1:par.vec.length,],iter=iter,time.steps=time.steps, startObservedData = 0, scenario=4, intervention_date="2020-03-12",sd.redux=NULL)

  traj <- dplyr::mutate(TEST.out, CFRobs=(D/Idetectcum), CFRactual=(D/(Itotcum)) )
  traj <-  dplyr::select(traj,c(1:4,CFRobs,CFRactual))

  ## TO SAVE MEMORY
  rm(TEST.out)

  print("Starting CI calc")

  ### MELTING AND APPLYING SUMMARY STAT FUNCTIONS
  df.traj <- reshape2::melt(traj, measure.vars = c(5:ncol(traj)), variable.name = "state.name")

  library(data.table)
  df.traj_dt <- as.data.table(df.traj)

  traj.CI <- df.traj_dt[, list(
    N=.N,
    median = quantile(value, c(.5), na.rm=TRUE),
    low_95 = quantile(value, c(.025), na.rm=TRUE),
    #low_50 = quantile(value,.25, na.rm=TRUE),
    #up_50 = quantile(value,.75, na.rm=TRUE),
    up_95 = quantile(value, c(.975), na.rm=TRUE)),
    by = c("date", "state.name")]
  traj.CI <- as.data.frame(traj.CI)

  ## TO ALIGN DATES: MODEL
  init.date = init.date.data #"2020-01-03"
  init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
  traj.CI[["date"]] <- traj.CI[["date"]] + init.date

  ## GET TODAY'S DATE
  CFR.today<- traj.CI %>% filter(date==Sys.Date()) %>% select(-c(date,N)) %>% mutate_if(is.numeric, round, digits=4)
  #rownames(CFR.today) <- c("Case Fatality Rate: Observed", "Case Fatality Rate: True, Estimated")
  rownames(CFR.today) <- c("Case Fatality Rate: Observed", "Case Fatality Rate: True, Estimated")
  CFR.today <- select(CFR.today, -c(1))

  # GET QUANTILES FOR VARIABLE

  posterior.CI <- function(posterior.var){
    median = quantile(posterior.var, c(.5), na.rm=TRUE)
    low_95 = quantile(posterior.var, c(.025), na.rm=TRUE)
    #mean = mean(posterior.var)
    up_95 = quantile(posterior.var, c(.975), na.rm=TRUE)
    posterior.CI <- as.data.frame(cbind(low_95,median,up_95))
    return(posterior.CI)
  }

  R0.posterior.CI <- round(posterior.CI(ABC_out$param[,1]),digits=2)
  r.posterior.CI <- posterior.CI(ABC_out$param[,2])
  R0redux.posterior.CI <- posterior.CI(ABC_out$param[,4])
  Alpha.posterior.CI <- posterior.CI(ABC_out$param[,6])
  Kappa.posterior.CI <- posterior.CI(ABC_out$param[,7])
  Delta.posterior.CI <- posterior.CI(ABC_out$param[,5])
  #Pr.D.given.I.CI <- posterior.CI(ABC_out$param[,6]*ABC_out$param[,7]*ABC_out$param[,5])

  #posterior.vars <- rbind(r.posterior.CI, R0.posterior.CI, R0redux.posterior.CI,Alpha.posterior.CI,Kappa.posterior.CI,Delta.posterior.CI,Pr.D.given.I.CI) %>% mutate_if(is.numeric, round, digits=4)
  posterior.vars <- rbind(r.posterior.CI, R0.posterior.CI, R0redux.posterior.CI,Alpha.posterior.CI,Kappa.posterior.CI,Delta.posterior.CI) %>% mutate_if(is.numeric, round, digits=4)
  rownames(posterior.vars) <- c("r, fraction obs. illnesses", "R0", "R0 fraction reduction", "Pr(Hospital|Illness)", "Pr(ICU|Hospital)", "Pr(Death|ICU)")

  all.posterior.vars <- rbind(CFR.today, posterior.vars)
  colnames(all.posterior.vars) <- c("Median", "Lower 95% CI", "Upper 95% CI")

  return(all.posterior.vars)

}

#summary.table(ABC.out.mat, par.vec.length=200, iter=20, time.steps=400, init.date.data="2020-03-01")


########################################################################################
## PLOTTING ALL FACETED FUNCTION
########################################################################################

plot.model.data.all <- function(traj.CI, data.in, init.date.data, date.offset.4plot, time.steps.4plot, vars.to.plot) {

  ## Select only more recent dates
  init.date <- init.date.data
  init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
  startDatePlot <- init.date - date.offset.4plot #15
  endDatePlot <- startDatePlot + time.steps.4plot - 40
  traj.CI <- traj.CI %>% dplyr::filter(date > startDatePlot) %>% dplyr::filter(date < endDatePlot)

  ## Filter to variables selected to plot
  traj.CI <- traj.CI %>% dplyr::filter(state.name==c(vars.to.plot))

  if(!is.null(data.in)){
    ## ALIGN DATES: DATA
    no_obs <- nrow(data.in)
    step <- 0:(no_obs-1)
    date <- init.date + step
    data.date <- cbind(date,data.in)
    rownames(data.date) <- step

    ## Select only more recent dates
    data.date <- data.date %>% dplyr::filter(date > startDatePlot)
    data <- reshape2::melt(data.date, measure.vars = c(2:ncol(data.date)), variable.name = "state.name")
  }

  ## PLOTTING
  #traj.CI.line <- reshape2::melt(traj.CI[c("date", "state.name", "mean", "median")], id.vars = c("date", "state.name"))
  traj.CI.line <- reshape2::melt(traj.CI[c("date", "state.name", "median")], id.vars = c("date", "state.name"))
  traj.CI.area <- reshape2::melt(traj.CI[c("date", "state.name", "low_95", "low_50", "up_50", "up_95")], id.vars = c("date", "state.name"))
  traj.CI.area$type <- sapply(traj.CI.area$variable, function(x) {str_split(x, "_")[[1]][1]})
  traj.CI.area$CI <- sapply(traj.CI.area$variable, function(x) {str_split(x, "_")[[1]][2]})
  traj.CI.area$variable <- NULL
  traj.CI.area <- reshape2::dcast(traj.CI.area, "date+state.name+CI~type")

  p <- ggplot(transform(traj.CI.area, state.name = factor(state.name, levels=vars.to.plot)))

  longnames <- c("Susceptible",
                 "New Obs. Infected",
                 "Current Obs. Infected",
                 "Cum. Obs. Infected",
                 "Current Tot. Infected",
                 "Cum. Tot. Infected",
                 "New in Hospital",
                 "Current in Hospital",
                 "Cum. in Hospital",
                 "Current in ICU",
                 "Cum. in ICU",
                 "Current Ventilation",
                 "Cum. Ventilation",
                 "New Deaths",
                 "Cum. Deaths",
                 "Recovered")

  names(longnames) <-  c("S",
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

  # longnames <- c("Susceptible",
  #                "Infected (Total Est.)",
  #                "Cumul. Infected (Total Est.)",
  #                "Infected (Obs.)",
  #                "Cumul. Infected (Obs.)",
  #                "In Hospital",
  #                "New in Hospital",
  #                "Cumul. Hospital",
  #                "In ICU",
  #                "Cumul. ICU",
  #                "In Ventilation",
  #                "Cumul. Ventilation",
  #                "Cumul. Dead",
  #                "New Deaths",
  #                "Recovered")

    # names(longnames) <- c("S",
  #                       "Itot",
  #                       "Itotcum",#
  #                       "I",
  #                       "Idetectcum",
  #                       "Htot",
  #                       "H_new",
  #                       "Htotcum",
  #                       "Q",
  #                       "Qcum", #
  #                       "V",
  #                       "Vcum",
  #                       "D",
  #                       "D_new",
  #                       "R")

  p <- p + facet_wrap(~state.name, labeller = labeller(state.name = longnames), scales = "free_y")
  p <- p + geom_ribbon(data = traj.CI.area, aes_string(x = "date", ymin = "low", ymax = "up", alpha = "CI", fill = "state.name"),show.legend = c(fill=FALSE))
  p <- p + geom_line(data = traj.CI.line, aes_string(x = "date", y = "value", linetype = "variable", colour = "state.name"), size = 1, show.legend = c(colour=FALSE))

  p <- p + scale_alpha_manual("Percentile", values = c("95" = 0.20, "50" = 0.50), labels = c("95" = "95th", "50" = "50th"))
  p <- p + scale_linetype("Stats")
  p <- p + guides(linetype = guide_legend(order = 1))

  ## ADD DATA
  if(!is.null(data.in)){
    p <- p + geom_point(data = data, aes_string(x = "date", y = "value"), size = .5, colour = "black")
  }

  p <- p + theme_bw() + theme(legend.position = "top", legend.box = "horizontal")
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "1 month" , date_labels = "%d-%b-%y")
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_text(size = 12, face = "bold"))
  p <- p + ylab("Numbers in Compartments") + xlab(NULL)
  p <- p + scale_y_continuous(labels = scales::comma)
  p
}


########################################################################################
## PLOTTING ONE AT A TIME FUNCTION INCLUDING SCENARIO SPECIFICATIONS
########################################################################################

# Manual ylim so I can fix height across charts

plot.model.single <- function(traj.CI, data.in, init.date.data=NULL, date.offset.4plot=NULL, time.steps.4plot=NULL, ymax=NULL, plot.capacity=NULL, var.to.plot=NULL, use.title=NULL, scenario=NULL, intervention_date=NULL, sd.redux=NULL, scenario.no=NULL) {

  ## Filter only to variable of interest
  traj.CI <- traj.CI %>%  dplyr::filter(state.name==var.to.plot)

  ## Select only more recent dates
  init.date <- init.date.data
  init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
  startDatePlot <- init.date - date.offset.4plot #15
  endDatePlot <- startDatePlot + time.steps.4plot - 40  # the constant 40 because the traj are not aligned to start date
  traj.CI <- traj.CI %>% dplyr::filter(date > startDatePlot) %>% dplyr::filter(date < endDatePlot)


  ## Data in -- plot only for the selected variable
  if(!is.null(data.in)){

    if(var.to.plot %in% colnames(data.in)) {  # FIX LATER -- REMOVE TMP

      ## Filter only to variable of interest
      data.in<- data.in %>% dplyr::select(var.to.plot)

      ## ALIGN DATES: DATA
      no_obs <- nrow(data.in)
      step <- 0:(no_obs-1)
      date <- init.date + step
      data.date <- cbind(date,data.in)
      rownames(data.date) <- step

      ## Select only more recent dates
      data.date <- data.date %>% dplyr::filter(date > startDatePlot)
      data <- reshape2::melt(data.date, measure.vars = c(2:ncol(data.date)), variable.name = "state.name")
    }

    else {data.in = NULL}
  }

  ## PLOTTING
  #traj.CI.line <- reshape2::melt(traj.CI[c("date", "state.name", "mean", "median")], id.vars = c("date", "state.name"))
  traj.CI.line <- reshape2::melt(traj.CI[c("date", "state.name", "median")], id.vars = c("date", "state.name"))
  traj.CI.area <- reshape2::melt(traj.CI[c("date", "state.name", "low_95", "low_50", "up_50", "up_95")], id.vars = c("date", "state.name"))
  traj.CI.area$type <- sapply(traj.CI.area$variable, function(x) {str_split(x, "_")[[1]][1]})
  traj.CI.area$CI <- sapply(traj.CI.area$variable, function(x) {str_split(x, "_")[[1]][2]})
  traj.CI.area$variable <- NULL
  traj.CI.area <- reshape2::dcast(traj.CI.area, "date+state.name+CI~type")

  # longnames <- c("Susceptible",
  #                "Infected (Total Est.)",
  #                "Cumul. Infected (Total Est.)",
  #                "Infected (Obs.)",
  #                "Cumul. Infected (Obs.)",
  #                "In Hospital",
  #                "New in Hospital",
  #                "Cumul. Hospital",
  #                "In ICU",
  #                "Cumul. ICU",
  #                "In Ventilation",
  #                "Cumul. Ventilation",
  #                "Cumul. Dead",
  #                "New Deaths",
  #                "Recovered")
  # names(longnames) <- c("S",
  #                       "Itot",
  #                       "Itotcum",#
  #                       "I",
  #                       "Idetectcum",
  #                       "Htot",
  #                       "H_new",
  #                       "Htotcum",
  #                       "Q",
  #                       "Qcum", #
  #                       "V",
  #                       "Vcum",
  #                       "D",
  #                       "D_new",
  #                       "R")

  longnames <- c("Susceptible",
                 "New Obs. Infected",
                 "Current Obs. Infected",
                 "Cum. Obs. Infected",
                 "Current Tot. Infected",
                 "Cum. Tot. Infected",
                 "New in Hospital",
                 "Current in Hospital",
                 "Cum. in Hospital",
                 "Current in ICU",
                 "Cum. in ICU",
                 "Current Ventilation",
                 "Cum. Ventilation",
                 "New Deaths",
                 "Cum. Deaths",
                 "Recovered")

  names(longnames) <-  c("S",
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

  ## Colors

  cols.list <- c(
    "salmon",
    "sandybrown",
    "navajowhite3",
    "olivedrab4",
    "olivedrab2",
    "mediumseagreen",
    "mediumaquamarine",
    "mediumturquoise",
    "cyan2",
    "lightskyblue",
    "steelblue2",
    "mediumpurple",
    "mediumorchid",
    "plum1",
    "violetred1",
    "deeppink3",
    "deeppink4"
  )
  names(cols.list) <- names(longnames)
  color.this.var <- as.character(cols.list[var.to.plot])

  ### THE PLOT
  p <- ggplot(traj.CI.area)

  if(!is.null(ymax)){
    p <- p + facet_wrap(~state.name, labeller = labeller(state.name = longnames), scales = "fixed")
  }
  else {p <- p + facet_wrap(~state.name, labeller = labeller(state.name = longnames), scales = "free_y")}

  p <- p + geom_ribbon(data = traj.CI.area, aes_string(x = "date", ymin = "low", ymax = "up", alpha = "CI"), fill = color.this.var, show.legend = c(fill=FALSE))
  p <- p + geom_line(data = traj.CI.line, aes_string(x = "date", y = "value", linetype = "variable"), colour = color.this.var, size=1, show.legend = c(colour=FALSE))

  ## ADD LEGENDS
  p <- p + scale_alpha_manual("Percentile", values = c("95" = 0.20, "50" = 0.50), labels = c("95" = "95th", "50" = "50th"))
  p <- p + scale_linetype("Stats")
  p <- p + guides(linetype = guide_legend(order = 1))

  if(!is.null(plot.capacity)){

    # Define the capacity lookup
    capacity.vars <- c("Idetectcum","Htot","Q","V")
    #capacity.vals <- c(1500000,23299,3380,1250)
    capacity.vals <- c(1500000,4000,2245,1000)
    capacity.lookup <- data.frame(t(capacity.vals))
    colnames(capacity.lookup) <- capacity.vars

    # Get capacity value
    capacity.current <- as.numeric(capacity.lookup[var.to.plot])

    # Create df with capacity
    capacity <- rep(capacity.current, nrow(traj.CI))
    traj.CI2 <- traj.CI
    traj.CI2$capacity <- capacity
    traj.CI.capacity <- reshape2::melt(traj.CI2[c("date", "state.name", "capacity")], id.vars = c("date", "state.name"))

    # Add to plot
    #p <- p + geom_line(data = traj.CI.capacity, aes_string(x = "date", y = "value"),colour="black", linetype = "dashed", size=2, show.legend=c(colour=FALSE))
    p <- p + geom_line(data = traj.CI.capacity, aes_string(x = "date", y = "value"),colour="black", linetype = "dashed", size=2, show.legend=c(colour=FALSE))

  }

  ## ADD DATA
  if(!is.null(data.in)){
    p <- p + geom_point(data = data, aes_string(x = "date", y = "value"), size = 1, colour = "black")
  }


  ## FINAL THEMES AND EDITING
  p <- p + theme_bw() + theme(legend.position = "top", legend.box = "horizontal")
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "2 weeks" , date_labels = "%d-%b-%y")
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_text(size = 12, face = "bold"))
#  p <- p + ylab(paste0("Number  ", as.character(longnames[var.to.plot]))) + xlab(NULL)
  p <- p + ylab(NULL) + xlab(NULL)


  if(!is.null(ymax)){
    p <- p + scale_y_continuous(limits=c(0,ymax) , labels = scales::comma, breaks=seq(0,ymax,ymax/10))
  }
  else {  p <- p + scale_y_continuous(labels = scales::comma)}


  if(!is.null(use.title)) {

    if(scenario==2){
      p <- p + labs(title = sprintf("SCENARIO %s: R0 = %s%% of orig. R0, beginning %s", scenario.no, sd.redux*100, intervention_date))
    }
    else if(scenario==0) {
      p <- p + labs(title = "Maintain current level of social distancing")
    }
    else if(scenario==1) {
      p <- p + labs(title = "Counterfactual: no social distancing implemented")
    }
    else if(scenario==3){
      p <- p + labs(title = sprintf("Gradual social distancing beginning on %s", intervention_date))
    }

    p<-p+theme(plot.title = element_text(size = 12, hjust = 0.5, face="bold"))
  }

  p

}


########################################################################################
## SPECIFYING EPIDEMIC MODEL TO BE SIMULATED AND SCENARIOS
########################################################################################

correlated.param.SIM <- function(ABC.out.mat,iter,time.steps,startObservedData,scenario=NULL,intervention_date=NULL,sd.redux=NULL) {

  #TEST.out <- vector("list", 1)
  #for (i in 1) {

  print(nrow(ABC.out.mat))
  str(ABC.out.mat)

  TEST.out <- vector("list", nrow(ABC.out.mat))

  for (i in 1:nrow(ABC.out.mat)) {

    ### PARAMETER ESTIMATES FROM ABC

    #    ABC.out.mat <- as.numeric(ABC.out.mat)

    R0 <- ABC.out.mat[i,1]
    r <- ABC.out.mat[i,2]
    start_time <- round(ABC.out.mat[i,3])
    R0_redux <- ABC.out.mat[i,4]
    Delta <- ABC.out.mat[i,5]
    Alpha <- ABC.out.mat[i,6]
    Kappa <- ABC.out.mat[i,7]
    p_V <- ABC.out.mat[i,8]

    ## MODEL INPUTS REQUIRED HERE TO RECOMPILE THE MODEL WITH UPDATED BETA(T) FUNCTION
    d_IH <- 10   #days between illness onset and hospitalization
    d_IR <- 7    #days between illness onset and recovery (hospitalization not required)
    Br <- R0 * ( 1 / ( (r/ ((Alpha/d_IH) + ((1-Alpha)/d_IR)))  + (1-r)*d_IR ))

    ## Get R0_redux(t) from read-in file
    # R0_redux_t = read.csv(here("in", "R0_redux_t.csv"), sep=",",stringsAsFactors = FALSE)
    # Beta_t <- as.Date(R0_redux_t$Beta_t)
    # Beta_t_obs <- as.numeric(Beta_t - as.Date("2020-03-01"))
    # R0_redux <- R0_redux_t$R0_redux
    # Beta_y_obs <- Br*(1-R0_redux)

    if (scenario==0){
      ## NO SCENARIO: PROJECTION USING MODEL ESTIMATED PARAMETERS
      Beta_t<- Beta_t_obs
      Beta_y<- Beta_y_obs
    }

    else if (scenario==1){
      # ## SCENARIO1: SOCIAL DISTANCING NEVER HAPPENED
      Beta_t<- c(0, (start_time+11), (start_time+25), 180, 260, 500) #c(0, 79, 80, 260, 270, 500)
      Beta_y<- c(Br, Br, Br, Br, Br, Br)
    }

    else if (scenario==2){
      # ## SCENARIO2: END SOCIAL DISTANCING ON SPECIFIC DATE
      #intervention_date <- as.Date("2020-05-01")
      #intervention_date <- as.Date(intervention_date)
      #R0_redux_scenario = 1-sd.redux
      #intervention_date_numeric <- as.numeric(intervention_date - as.Date("2020-03-01"))
      #Beta_t<- c(0, (start_time+11), (start_time+25), (start_time+intervention_date_numeric-1), (start_time+intervention_date_numeric+5), 175, 200, 500) #c(0, 79, 80, 260, 270, 500)
      #Beta_y<- c(Br, Br, Br*R0_redux, Br*R0_redux, Br*R0_redux_scenario, Br*R0_redux_scenario, Br*R0_redux_scenario, Br*R0_redux_scenario)

      observed_redux_date <- as.Date("2020-03-12")
      observed_redux_numeric <- as.numeric(observed_redux_date - as.Date("2020-03-01"))

      intervention_date <- as.Date(intervention_date)
      intervention_date_numeric <- as.numeric(intervention_date - as.Date("2020-03-01"))

      Beta_t<- c(
        0,                              #1               #2              #3
        (start_time+observed_redux_numeric),       #4
        (start_time+observed_redux_numeric+15),
        (start_time+intervention_date_numeric),
        (start_time+intervention_date_numeric+3),
        (start_time+intervention_date_numeric+500))         #5

      Beta_y<- c(
        Br, #1
        Br, #2
        (Br*R0_redux), #3
        (Br*R0_redux), #4
        (Br*sd.redux), #5
        (Br*sd.redux)
        )

      }

    else if (scenario==3){
      # ## SCENARIO3: GRADUAL EASING BEGINNING MAY 1ST: REDUCE SD BY 10% EVERY 14 DAYS
      intervention_date <- as.Date(intervention_date)
      intervention_date_numeric <- as.numeric(intervention_date - as.Date("2020-03-01"))
      date_to_start_easing <- start_time+intervention_date_numeric
      Beta_t<- c(
        0,                              #1
        (start_time+11),                #2
        (start_time+25),                #3
        (date_to_start_easing-1),       #4
        (date_to_start_easing),         #5
        (date_to_start_easing + 14),    #6
        (date_to_start_easing + 28),    #7
        (date_to_start_easing + 42),    #8
        (date_to_start_easing + 56),    #9
        (500))                          #10

      Beta_y<- c(
        Br, #1
        Br*R0_redux, #2
        Br*R0_redux, #3
        Br*R0_redux, #4
        Br*0.6,      #5
        Br*0.7,      #6
        Br*0.8,      #7
        Br*0.9,      #8
        Br*1.0,      #9
        Br*1.0)      #10
    }

    else if (scenario==4){
      # ## SCENARIO4: INTERVENTION DATE = DATE SOCIAL DISTANCING BEGINS
      intervention_date <- as.Date(intervention_date)
      intervention_date_numeric <- as.numeric(intervention_date - as.Date("2020-03-01"))
      date_to_start_easing <- as.numeric(as.Date("2020-04-25")-as.Date("2020-03-01"))

      Beta_t<- c(
        0,                              #1
        (start_time),                #2
        (start_time+1),                #3
        (start_time+intervention_date_numeric),       #4
        (start_time+intervention_date_numeric+15),
        (start_time+date_to_start_easing),
        (start_time+date_to_start_easing+1),
        (start_time+date_to_start_easing+500))         #5

      Beta_y<- c(
        Br, #1
        Br, #2
        Br,
        Br,
        (Br*R0_redux), #5
        (Br*R0_redux), #6
        (Br*R0_redux*1), #7
        (Br*R0_redux*1)) #8

    }

    ## COMPILE
    x <- seihqdr_generator(Beta_t=Beta_t, Beta_y=Beta_y, S_ini=1e7,E_ini=10,r=r,Delta = Delta, Alpha=Alpha, Kappa=Kappa, p_QV=p_V)

    ## SIMULATE
    TEST<-as.data.frame(plyr::rdply(iter, x$run(0:time.steps),.id="iter"))

    ## BIND INCLUDING OFFSETING OBSERVED DATA BY START DATE
    TEST.out[[i]] <- cbind(data.frame(par.id = i, date = startObservedData-start_time+TEST$step), TEST)
  }

  ## ADD TO DATAFRAME OVER ALL PARAMETER VALUES
  TEST.out <- do.call(rbind, TEST.out)

  return(TEST.out)

}


########################################################################################
## GETTING MODEL OUTPUT + SUMMARY STATISTICS FUNCTION
########################################################################################

model.output.to.plot.SIM <- function(ABC.out.mat, par.vec.length, iter, time.steps, vars.to.plot, init.date.data="2020-03-01",all=NULL,scenario.selection,intervention_date=NULL,sd.redux=NULL) {

  if(scenario.selection==2 || scenario.selection==3){
    if(is.null(intervention_date) || is.null(sd.redux) )
    {stop("Need to specify intervention conditions")
    }
  }

  library(data.table)

  ## MODEL OUTPUT TO PLOT
  #TEST.out <- correlated.param(ABC.out.mat[1:par.vec.length,],iter=iter,time.steps=time.steps, startObservedData = 0, scenario=scenario.selection)
  # if (par.vec.length==1){ABC.to.run = ABC.out.mat}
  # else if (par.vec.length>1) {ABC.to.run = ABC.out.mat[1:par.vec.length,]}
  TEST.out <- correlated.param.SIM(ABC.out.mat[1:par.vec.length,],iter=iter,time.steps=time.steps, startObservedData = 0, scenario=scenario.selection, intervention_date=intervention_date,sd.redux=sd.redux)

  if(all){
    traj <- dplyr::mutate(TEST.out, Itot=I+A, CFRobs=D/Idetectcum, CFRactual=D/Itotcum )
  }
  traj <-  dplyr::select(traj,c(1:4,vars.to.plot))


  ## TO SAVE MEMORY
  rm(TEST.out)

  print("Starting CI calc")

  ### MELTING AND APPLYING SUMMARY STAT FUNCTIONS
  df.traj <- reshape2::melt(traj, measure.vars = c(5:ncol(traj)), variable.name = "state.name")

  library(data.table)
  df.traj_dt <- as.data.table(df.traj)

  traj.CI <- df.traj_dt[, list(
    N=.N,
    mean = mean(value),
    low_95 = quantile(value, c(.025)),
    up_95 = quantile(value, c(.975)),
    median = quantile(value, c(.5)),
    up_50 = quantile(value,.75),
    low_50 = quantile(value,.25)),
    by = c("date", "state.name")]
  traj.CI <- as.data.frame(traj.CI)

  ## TO ALIGN DATES: MODEL
  init.date = init.date.data #"2020-01-03"
  init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
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

sum.stats.SIMTEST <- function(data,include.R=TRUE){

  no_obs <- nrow(data)

  # Which values of variables to consider
  I.trust.n <- c(10:no_obs)  # The first 9 days of illness cases are unreliable/unavailable
  H.trust.n <- c(17:no_obs)  # The first 16 days of hospitalizations are unreliable/unavailable
  V.trust.n <- c(19:no_obs)  # The first 18 days of ventilation are unreliable/unavailable
  D.trust.n <- c(18:no_obs)  # The first 17 days of mortality are unreliable/unavailable
  Hnew.trust.n <- c(19:no_obs) # The first 18 days of new hospitalizations are unreliable/unavailable
  Dnew.trust.n <- c(28:no_obs) # The first 28 days of new deaths are unreliable/unavailable
  R.trust.n <- c(0:35)

  ss.I <- data$Idetectcum[I.trust.n]
  ss.H <- data$Htotcum[H.trust.n]
  ss.V <- data$Vcum[V.trust.n]
  ss.D <- data$D[D.trust.n]
  ss.Hnew <- data$H_new[Hnew.trust.n]
  ss.Dnew <- data$D_new[Dnew.trust.n]
  ss.R <- data$R[R.trust.n]

  # Which variables to consider

  if (include.R == TRUE){summarystats = c(ss.I, ss.H, ss.V, ss.D, ss.Hnew, ss.Dnew, ss.R) }
  else if (include.R == FALSE)
    {summarystats = c(ss.I, ss.H, ss.V, ss.D, ss.Hnew, ss.Dnew) }


  return(summarystats)
}

# ## SUMMARY STATISTICS COMPUTED ON DATA
# summarydata <- sum.stats(data)
# #summarydata

###################################################################################################
## SIMULATION MODEL FUNCTION TO COMPUTE FOR ABC ALGORITHM
## A function implementing the model to be simulated
## It must take as arguments a vector of model parameter values par
## and it must return a vector of summary statistics
###################################################################################################

model.1sim.stats.no.R <- function(par){

  ################################################
  #no_obs <- 100
  ################################################

  R0 <- par[1]
  r <- par[2]
  start_time <- par[3]
  R0_redux1 <- par[4]
  Delta <- par[5]
  Alpha <- par[6]
  Kappa <- par[7]
  p_V <- par[8]

  #intervention_date <- "2020-03-15"

  ### EPIDEMIC MODEL INPUTS
  d_IH <- 10   #days between illness onset and hospitalization
  d_IR <- 7    #days between illness onset and recovery (hospitalization not required)
  Br <- R0 * ( 1 / ( (r/ ((Alpha/d_IH) + ((1-Alpha)/d_IR)))  + (1-r)*d_IR ))

  ### R(t) FUNCTION
  ## Data time step 0 is March 1, 2020

  intervention_date <- as.Date(intervention_date)
  intervention_date_numeric <- as.numeric(intervention_date - as.Date("2020-03-01"))
  date_to_start_easing <- start_time+intervention_date_numeric

  Beta_t<- c(
    0,                              #1
    (start_time),                #2
    (start_time+1),                #3
    (start_time+intervention_date_numeric),       #4
    (start_time+intervention_date_numeric+15),
    (start_time+intervention_date_numeric+45),
    (start_time+intervention_date_numeric+46),
    (start_time+intervention_date_numeric+500))         #5

  Beta_y<- c(
    Br, #1
    Br, #2
    Br,
    Br,
    (Br*R0_redux), #5
    (Br*R0_redux), #6
    (Br*R0_redux), #7
    (Br*R0_redux)) #8

  ### GENERATE SIMULATION
  x <- seihqdr_generator(Beta_t=Beta_t, Beta_y=Beta_y, S_ini=1e7, E_ini=10, r=r, Delta=Delta, Alpha=Alpha, Kappa=Kappa, p_QV=p_V)
  st <- start_time
  one_sim <- as.data.frame(x$run(0:(st+no_obs))[(st+1):(st+no_obs),])

  ### SUMMARY STATISTICS COMPUTED ON MODEL OUTPUT:
  summarymodel <- sum.stats.SIMTEST(one_sim,include.R=FALSE)

  return(summarymodel)
}

model.1sim.stats.yes.R <- function(par){

  ################################################
  #no_obs <- 100
  ################################################

  R0 <- par[1]
  r <- par[2]
  start_time <- par[3]
  R0_redux1 <- par[4]
  Delta <- par[5]
  Alpha <- par[6]
  Kappa <- par[7]
  p_V <- par[8]

  #intervention_date <- "2020-03-15"

  ### EPIDEMIC MODEL INPUTS
  d_IH <- 10   #days between illness onset and hospitalization
  d_IR <- 7    #days between illness onset and recovery (hospitalization not required)
  Br <- R0 * ( 1 / ( (r/ ((Alpha/d_IH) + ((1-Alpha)/d_IR)))  + (1-r)*d_IR ))

  ### R(t) FUNCTION
  ## Data time step 0 is March 1, 2020

  intervention_date <- as.Date(intervention_date)
  intervention_date_numeric <- as.numeric(intervention_date - as.Date("2020-03-01"))
  date_to_start_easing <- start_time+intervention_date_numeric

  #
  # Beta_t<- c(
  #   0,                              #1
  #   (start_time),                #2
  #   (start_time+1),                #3
  #   (start_time+intervention_date_numeric),       #4
  #   (start_time+intervention_date_numeric+1),
  #   (start_time+intervention_date_numeric+500))         #5
  #
  # Beta_y<- c(
  #   Br, #1
  #   Br, #2
  #   Br,
  #   Br,
  #   (Br*R0_redux), #4
  #   (Br*R0_redux)) #5

  Beta_t<- c(
    0,                              #1
    (start_time),                #2
    (start_time+1),                #3
    (start_time+intervention_date_numeric),       #4
    (start_time+intervention_date_numeric+15),
    (start_time+intervention_date_numeric+45),
    (start_time+intervention_date_numeric+46),
    (start_time+intervention_date_numeric+500))         #5

  Beta_y<- c(
    Br, #1
    Br, #2
    Br,
    Br,
    (Br*R0_redux), #5
    (Br*R0_redux), #6
    (Br*R0_redux), #7
    (Br*R0_redux)) #8

  ### GENERATE SIMULATION
  x <- seihqdr_generator(Beta_t=Beta_t, Beta_y=Beta_y, S_ini=1e7, E_ini=10, r=r, Delta=Delta, Alpha=Alpha, Kappa=Kappa, p_QV=p_V)
  st <- start_time
  one_sim <- as.data.frame(x$run(0:(st+no_obs))[(st+1):(st+no_obs),])

  ### SUMMARY STATISTICS COMPUTED ON MODEL OUTPUT:
  summarymodel <- sum.stats.SIMTEST(one_sim,include.R=TRUE)

  return(summarymodel)
}


