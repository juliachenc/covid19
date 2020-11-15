
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

########################################################################################
## SUMMARY TABLE: ORIGINAL

summary.table <- function(traj.CI, ABC.out.mat) {
  #summary.table <- function(traj.CI, ABC.out.mat, par.vec.length, iter, time.steps, init.date.data="2020-03-01") {

  library(data.table)

  ## GET TODAY'S DATE
  CFR.obs<- traj.CI %>% filter(date==Sys.Date(),state.name=="CFRobs") %>% select(-c(date,N,mean,up_50,low_50)) %>% mutate_if(is.numeric, round, digits=4)
  CFR.actual<- traj.CI %>% filter(date==Sys.Date(),state.name=="CFRactual") %>% select(-c(date,N,mean,up_50,low_50)) %>% mutate_if(is.numeric, round, digits=4)
  CFR.today <- rbind(CFR.obs,CFR.actual)
  rownames(CFR.today) <- c("Case Fatality Rate", "Infection Fatality Rate")
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
  R0redux1.posterior.CI <- posterior.CI(ABC_out$param[,4])
  R0redux2.posterior.CI <- posterior.CI(ABC_out$param[,9])
  Alpha1.posterior.CI <- posterior.CI(ABC_out$param[,6])
  Kappa1.posterior.CI <- posterior.CI(ABC_out$param[,7])
  Delta1.posterior.CI <- posterior.CI(ABC_out$param[,5])
  Alpha2.posterior.CI <- posterior.CI(ABC_out$param[,11])
  Kappa2.posterior.CI <- posterior.CI(ABC_out$param[,12])
  Delta2.posterior.CI <- posterior.CI(ABC_out$param[,10])
  #Pr.D.given.I.CI <- posterior.CI(ABC_out$param[,6]*ABC_out$param[,7]*ABC_out$param[,5])

  #posterior.vars <- rbind(r.posterior.CI, R0.posterior.CI, R0redux.posterior.CI,Alpha.posterior.CI,Kappa.posterior.CI,Delta.posterior.CI,Pr.D.given.I.CI) %>% mutate_if(is.numeric, round, digits=4)
  posterior.vars <- rbind(r.posterior.CI, R0.posterior.CI, R0redux1.posterior.CI, R0redux2.posterior.CI, Alpha1.posterior.CI,Kappa1.posterior.CI,Delta1.posterior.CI, Alpha2.posterior.CI,Kappa2.posterior.CI,Delta2.posterior.CI) %>% mutate_if(is.numeric, round, digits=3)
  rownames(posterior.vars) <- c("r, fraction obs. illnesses", "R0 Initial", "R0 fraction reduction Mar12","R0 fraction reduction July5", "Pr(H|I)1", "Pr(Q|H)1", "Pr(D|Q)1", "Pr(H|I)2", "Pr(Q|H)2", "Pr(D|Q)2")

  all.posterior.vars <- join(posterior.vars,CFR.today,type="full") #rbind(CFR.today, posterior.vars)
  rownames(all.posterior.vars) <- c(rownames(posterior.vars),rownames(CFR.today))
  colnames(all.posterior.vars) <- c("Lower 95% CI", "Median", "Upper 95% CI")

  return(all.posterior.vars)

}

#summary.table(traj.CI=traj.0, ABC.out.mat=ABC.out.mat)


########################################################################################
## SUMMARY TABLE: WITH TWO DATES

summary.table.param.CFR.IFR <- function(traj.CI, ABC_out) {
  library(data.table)

  # GET QUANTILES FOR VARIABLE
  posterior.CI <- function(posterior.var){
    median = quantile(posterior.var, c(.5), na.rm=TRUE)
    low_95 = quantile(posterior.var, c(.025), na.rm=TRUE)
    mean = mean(posterior.var)
    up_95 = quantile(posterior.var, c(.975), na.rm=TRUE)
    posterior.CI <- as.data.frame(cbind(low_95,mean,up_95))
    return(posterior.CI)
  }

  # FORMAT AS "mean(low_95,up_95)"
  posterior.CI.FORMAT <- function(var.CI){
    var.CI <- as.data.frame(var.CI)
    low_95 <- round(var.CI$low_95,3)
    mean <- round(var.CI$mean,3)
    up_95 <- round(var.CI$up_95,3)
    var.95.CI <- paste0(mean, " (", low_95, ",", up_95,")")
    return(var.95.CI)
  }

  get.CFR.IFR.by.date <- function(traj.CI, CFR.or.IFR, date.in){
    if (CFR.or.IFR=="CFR"){
      state.name.in="CFRobs"
    }
    if (CFR.or.IFR=="IFR"){
      state.name.in="CFRactual"
    }
    posterior.CI <- traj.CI %>% filter(date==as.Date(date.in), state.name==state.name.in) %>% select(-c(state.name,date,N,median,up_50,low_50)) %>% mutate_if(is.numeric, round, digits=3)
    return(posterior.CI)
  }

  # GET CFR IFR
  CFR1.posterior.CI <- get.CFR.IFR.by.date(traj.CI=traj.CI, CFR.or.IFR = "CFR", date.in="2020-04-20")
  IFR1.posterior.CI <- get.CFR.IFR.by.date(traj.CI=traj.CI, CFR.or.IFR = "IFR", date.in="2020-04-20")
  CFR2.posterior.CI <- get.CFR.IFR.by.date(traj.CI=traj.CI, CFR.or.IFR = "CFR", date.in="2020-07-20")
  IFR2.posterior.CI <- get.CFR.IFR.by.date(traj.CI=traj.CI, CFR.or.IFR = "IFR", date.in="2020-07-20")

  # GET ESTIMATED PARAMETER CI
  R0.posterior.CI <- posterior.CI(ABC_out$param[,1])
  r.posterior.CI <- posterior.CI(ABC_out$param[,2])
  R0redux1.posterior.CI <- posterior.CI(ABC_out$param[,4])
  R0redux2.posterior.CI <- posterior.CI(ABC_out$param[,9])
  Alpha1.posterior.CI <- posterior.CI(ABC_out$param[,6])
  Kappa1.posterior.CI <- posterior.CI(ABC_out$param[,7])
  Delta1.posterior.CI <- posterior.CI(ABC_out$param[,5])
  Alpha2.posterior.CI <- posterior.CI(ABC_out$param[,11])
  Kappa2.posterior.CI <- posterior.CI(ABC_out$param[,12])
  Delta2.posterior.CI <- posterior.CI(ABC_out$param[,10])

  # PUT ALL APR.20 AND JUL.20 PARAMETERS IN A TABLE
  probs.CFR.IFR.table <- matrix(data=NA, nrow=5, ncol=2)
  colnames(probs.CFR.IFR.table) <- c("Apr 20", "Jul 20")
  rownames(probs.CFR.IFR.table) <- c("Alpha_t", "Kappa_t", "Delta_t", "CFR", "IFR")
  probs.CFR.IFR.table[1,1] <- posterior.CI.FORMAT(Alpha1.posterior.CI)
  probs.CFR.IFR.table[1,2] <- posterior.CI.FORMAT(Alpha2.posterior.CI)
  probs.CFR.IFR.table[2,1] <- posterior.CI.FORMAT(Kappa1.posterior.CI)
  probs.CFR.IFR.table[2,2] <- posterior.CI.FORMAT(Kappa2.posterior.CI)
  probs.CFR.IFR.table[3,1] <- posterior.CI.FORMAT(Delta1.posterior.CI)
  probs.CFR.IFR.table[3,2] <- posterior.CI.FORMAT(Delta2.posterior.CI)
  probs.CFR.IFR.table[4,1] <- posterior.CI.FORMAT(CFR1.posterior.CI)
  probs.CFR.IFR.table[4,2] <- posterior.CI.FORMAT(CFR2.posterior.CI)
  probs.CFR.IFR.table[5,1] <- posterior.CI.FORMAT(IFR1.posterior.CI)
  probs.CFR.IFR.table[5,2] <- posterior.CI.FORMAT(IFR2.posterior.CI)
  probs.CFR.IFR.table <- as.data.frame(probs.CFR.IFR.table)

  # PUT OTHER PARAMETERS IN A TABLE
  r.posterior.CI <- posterior.CI.FORMAT(r.posterior.CI)
  R0.posterior.CI <- posterior.CI.FORMAT(R0.posterior.CI)
  R0redux1.posterior.CI <- posterior.CI.FORMAT(R0redux1.posterior.CI)
  R0redux2.posterior.CI <- posterior.CI.FORMAT(R0redux2.posterior.CI)
  posterior.epi.vars <- as.data.frame(rbind(r.posterior.CI, R0.posterior.CI, R0redux1.posterior.CI, R0redux2.posterior.CI))
  rownames(posterior.epi.vars) <- c("r, fraction obs. illnesses", "R0 Initial", "R0 factor redux Mar.12","R0 factor redux Jul.5")
  colnames(posterior.epi.vars) <- "mean (95% CI)"

  return(list(probs.CFR.IFR.table,posterior.epi.vars))

}



########################################################################################
## PLOTTING ALL FACETED FUNCTION
########################################################################################

plot.model.data.all <- function(traj.CI, data.in, init.date.data, date.offset.4plot, time.steps.4plot, vars.to.plot) {

  ## Select only more recent dates
  init.date <- init.date.data
  init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
  startDatePlot <- init.date - date.offset.4plot #15
  endDatePlot <- startDatePlot + time.steps.4plot # - 40
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
  endDatePlot <- startDatePlot + time.steps.4plot #- 40  # the constant 40 because the traj are not aligned to start date
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

correlated.param.SIM <- function(ABC.out.mat,iter,time.steps) {

  TEST.out <- vector("list", nrow(ABC.out.mat))

  for (i in 1:nrow(ABC.out.mat)) {

    ### PARAMETER ESTIMATES FROM ABC

    R0 <- ABC.out.mat[i,1]
    r <- ABC.out.mat[i,2]
    start_time <- round(ABC.out.mat[i,3])
    R0_redux1 <- ABC.out.mat[i,4]
    Delta1 <- ABC.out.mat[i,5]
    Alpha1 <- ABC.out.mat[i,6]
    Kappa1 <- ABC.out.mat[i,7]
    p_V <- ABC.out.mat[i,8]
    R0_redux2 <- ABC.out.mat[i,9]
    Delta2 <- ABC.out.mat[i,10]
    Alpha2 <- ABC.out.mat[i,11]
    Kappa2 <- ABC.out.mat[i,12]

    ### BRING IN BETA_T ALPHA_T KAPPA_T DELTA_T FUNCTIONS
    fn_t_readin_code <- path(code.paper.dir, "fn_t_readin_code.R")
    source(fn_t_readin_code, local=TRUE)

    # length.B <- length(Beta_y)
    # Beta_y[length.B] <- Beta_y[length.B]*0.9
    # Beta_y[length.B] <- Beta_y[length.B-1]*0.9

    ## COMPILE
    x <- seihqdr_generator(Alpha_t=Alpha_t, Alpha_y=Alpha_y, Kappa_t=Kappa_t, Kappa_y=Kappa_y, Delta_t=Delta_t, Delta_y=Delta_y, Beta_t=Beta_t, Beta_y=Beta_y, S_ini=1e7, E_ini=10, r=r, p_QV=p_V)

    ## SIMULATE
    TEST<-as.data.frame(plyr::rdply(iter, x$run(0:time.steps),.id="iter"))

    ## BIND INCLUDING OFFSETING OBSERVED DATA BY START DATE
    TEST.out[[i]] <- cbind(data.frame(par.id = i, date = -start_time+TEST$step), TEST)
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


###################################################################################################
## SIMULATION MODEL FUNCTION TO COMPUTE FOR ABC ALGORITHM
## A function implementing the model to be simulated
## It must take as arguments a vector of model parameter values par
## and it must return a vector of summary statistics
###################################################################################################

model.1sim.stats.no.R <- function(par){

  R0 <- par[1]
  r <- par[2]
  start_time <- par[3]
  R0_redux1 <- par[4]
  Delta1 <- par[5]
  Alpha1 <- par[6]
  Kappa1 <- par[7]
  p_V <- par[8]
  R0_redux2 <- par[9]
  Delta2 <- par[10]
  Alpha2 <- par[11]
  Kappa2 <- par[12]

  ### BRING IN BETA_T ALPHA_T KAPPA_T DELTA_T FUNCTIONS
  fn_t_readin_code <- path(code.paper.dir, "fn_t_readin_code.R")
  source(fn_t_readin_code, local=TRUE)

  # length.B <- length(Beta_y)
  # Beta_y[length.B] <- Beta_y[length.B]*0.9
  # Beta_y[length.B] <- Beta_y[length.B-1]*0.9

  ### GENERATE SIMULATION
  x <- seihqdr_generator(Alpha_t=Alpha_t, Alpha_y=Alpha_y, Kappa_t=Kappa_t, Kappa_y=Kappa_y, Delta_t=Delta_t, Delta_y=Delta_y, Beta_t=Beta_t, Beta_y=Beta_y, S_ini=1e7, E_ini=10, r=r, p_QV=p_V)
  st <- start_time
  one_sim <- as.data.frame(x$run(0:(st+no_obs))[(st+1):(st+no_obs),])

  ### SUMMARY STATISTICS COMPUTED ON MODEL OUTPUT:
  summarymodel <- sum.stats.SIMTEST(one_sim,include.R=FALSE)

  return(summarymodel)
}


