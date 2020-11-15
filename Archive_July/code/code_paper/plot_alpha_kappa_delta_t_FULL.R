

#   TO DO:
# GET 50% INTERVAL FOR ALPHA, KAPPA, DELTA T1 T2
# CREATE DATA FRAME WITH R0(t) AND CI BASED ON POSTERIOR CI. PUT IT IN FORMAT FOR PLOTTING SINGLE
# WILL NEED TO ADJUST SINCE R0(t) DOESN'T HAVE A COLOR ASSOCIATED

posterior.CI <- function(ABC_out, ABC.num, round.by){
  posterior.var <- as.data.frame(ABC_out$param[,ABC.num])
  median = quantile(posterior.var, c(.5), na.rm=TRUE)
  low_95 = quantile(posterior.var, c(.025), na.rm=TRUE)
  #mean = mean(posterior.var)
  up_95 = quantile(posterior.var, c(.975), na.rm=TRUE)
  up_50 = quantile(posterior.var,.75,na.rm=TRUE)
  low_50 = quantile(posterior.var,.25,na.rm=TRUE)
  posterior.CI <- as.data.frame(cbind(low_95,low_50,median,up_50,up_95))
  posterior.CI <- round(posterior.CI, digits=round.by)
  return(posterior.CI)
}

Alpha1.CI <- posterior.CI(ABC_out=ABC_out, ABC.num=6, round.by=4) #posterior.CI(as.data.frame(ABC_out$param[,]))
Kappa1.CI <- posterior.CI(ABC_out=ABC_out, ABC.num=7, round.by=4)
Delta1.CI <- posterior.CI(ABC_out=ABC_out, ABC.num=5, round.by=4)

Alpha2.CI <- posterior.CI(ABC_out=ABC_out, ABC.num=11, round.by=4) #posterior.CI(as.data.frame(ABC_out$param[,]))
Kappa2.CI <- posterior.CI(ABC_out=ABC_out, ABC.num=12, round.by=4)
Delta2.CI <- posterior.CI(ABC_out=ABC_out, ABC.num=10, round.by=4)

########################################################################################
########################################################################################
## Alpha(t) Kappa(t) Delta(t) FUNCTIONS
########################################################################################
########################################################################################


startObservedData <- as.Date("2020-03-01")
intervention_date <- as.Date(intervention_date)
intervention_date_numeric <- as.numeric(intervention_date - as.Date("2020-03-01"))
todays_date_numeric = as.numeric(as.Date("2020-07-16") - as.Date("2020-03-01"))

Alpha_decrease_date <- as.numeric( as.Date("2020-06-01") - as.Date("2020-03-01"))
Alpha_stabilize_date <- as.numeric( as.Date("2020-07-01") - as.Date("2020-03-01"))
Final_plot_date <- as.numeric( as.Date("2020-07-20") - as.Date("2020-03-01") )

Alpha_t.time<-c(
  0,
  Alpha_decrease_date-1,
  Alpha_decrease_date,
  Alpha_stabilize_date,
  Final_plot_date
)
Alpha_y<-c(
  Alpha1, # 0,
  Alpha1, # Alpha_decrease_date-1,
  Alpha1, # Alpha_decrease_date,
  Alpha2, # Alpha_stabilize_date,
  Alpha2 # Alpha_stabilize_date + 400
)
Kappa_t <- Alpha_t.time
Kappa_y<-c(
  Kappa1, # 0,
  Kappa1, # Alpha_decrease_date-1,
  Kappa1, # Alpha_decrease_date,
  Kappa2, # Alpha_stabilize_date,
  Kappa2 # Alpha_stabilize_date + 400
)
Delta_t <- Alpha_t.time
Delta_y<-c(
  Delta1, # 0,
  Delta1, # Alpha_decrease_date-1,
  Delta1, # Alpha_decrease_date,
  Delta2, # Alpha_stabilize_date,
  Delta2 # Alpha_stabilize_date + 400
)

#################################
## Put Alpha(t) function in format for plots

Alpha_t <- rbind(
    Alpha1.CI, # 0,
    Alpha1.CI, # Alpha_decrease_date-1,
    Alpha1.CI, # Alpha_decrease_date,
    Alpha2.CI, # Alpha_stabilize_date,
    Alpha2.CI # Alpha_stabilize_date + 400
  )

Alpha_t$date <- Alpha_t.time + startObservedData
Alpha_t$state.name <- rep("Alpha_t",nrow(Alpha_t))
rownames(Alpha_t)<-1:nrow(Alpha_t)


# ## Quick plot of R0(t) median
# p <- ggplot(Alpha_t, aes(x=date, y=median)) +
#   geom_line() +
#   xlab("Date") + ylab("Alpha(t)") + #ylim(0,6) +
#   scale_x_date(date_breaks = "2 weeks",limit=c(as.Date("2020-03-01"),as.Date("2020-07-20"))) + ggtitle("R0(t), with model-estimated reduction trend")
# p

#################################
## Put Kappa(t) function in format for plots

Kappa_t <- rbind(
  Kappa1.CI, # 0,
  Kappa1.CI, # Alpha_decrease_date-1,
  Kappa1.CI, # Alpha_decrease_date,
  Kappa2.CI, # Alpha_stabilize_date,
  Kappa2.CI # Alpha_stabilize_date + 400
)

Kappa_t$date <- Alpha_t.time + startObservedData
Kappa_t$state.name <- rep("Kappa_t",nrow(Kappa_t))
rownames(Kappa_t)<-1:nrow(Kappa_t)


#################################
## Put Delta(t) function in format for plots

Delta_t <- rbind(
  Delta1.CI, # 0,
  Delta1.CI, # Alpha_decrease_date-1,
  Delta1.CI, # Alpha_decrease_date,
  Delta2.CI, # Alpha_stabilize_date,
  Delta2.CI # Alpha_stabilize_date + 400
)

Delta_t$date <- Alpha_t.time + startObservedData
Delta_t$state.name <- rep("Delta_t",nrow(Kappa_t))
rownames(Delta_t)<-1:nrow(Delta_t)



########################################################################################
########################################################################################
## PLOT TOGETHER WITH NEW ADDITIONS TO VARIABLES
########################################################################################
########################################################################################

plot.together.R0 <- function(traj.CI=traj.CI, data.in=data.in, init.date.data=NULL, date.offset.4plot=NULL, time.steps.4plot, vars.to.plot) {

  if(is.null(init.date.data)) {
    init.date.data <- "2020-03-01"}
  if(is.null(date.offset.4plot)){
    date.offset.4plot=0}

  ###########
  ### traj.CI
  ###########

  ## Filter only to variable of interest
  traj.CI <- traj.CI %>%  dplyr::filter(state.name %in% vars.to.plot)

  ## Select only more recent dates
  init.date <- init.date.data
  init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
  startDatePlot <- init.date - date.offset.4plot -1 #15
  endDatePlot <- startDatePlot + time.steps.4plot #- 40  # the constant 40 because the traj are not aligned to start date
  traj.CI <- traj.CI %>% dplyr::filter(date >= startDatePlot) %>% dplyr::filter(date < endDatePlot)

  ## Add title
  traj.CI$title <- chart.title

  ###########
  ### data.in
  ###########

  ## Data in -- plot only for the selected variable
  if(!is.null(data.in)){

    if(any(vars.to.plot %in% colnames(data.in))) {  # FIX LATER -- REMOVE TMP

      ## Filter only to variable of interest
      vars.to.extract <- vars.to.plot[vars.to.plot %in% colnames(data.in) ]

      data.in<- data.in %>% dplyr::select(vars.to.extract)

      ## ALIGN DATES: DATA
      no_obs <- nrow(data.in)
      step <- 0:(no_obs-1)
      date <- init.date + step
      data.date <- cbind(date,data.in)
      rownames(data.date) <- date
      #data.date$date <- NULL

      ## Select only more recent dates
      data.date <- data.date %>% dplyr::filter(date > startDatePlot)
      data <- reshape2::melt(data.date, measure.vars = c(2:ncol(data.date)), variable.name = "state.name")
    }

    else {data = NULL}
  }

  #####################
  ### colors and names
  #####################

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
                 "Recovered",
                 "R0(t)",
                 "Alpha(t)",
                 "Kappa(t)",
                 "Delta(t)"
                 )

  names(longnames) <-  c(
                         "S",
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
                         "R",
                         "R0_t",
                         "Alpha_t",
                         "Kappa_t",
                         "Delta_t"
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
    "deeppink4",
    "grey50",
    "mediumturquoise",
    "lightskyblue",
    "violetred1"
  )
  names(cols.list) <- names(longnames)
  color.this.var <- as.character(cols.list[vars.to.plot])

  ##################
  ### CREATE PLOT
  ##################

  p <- ggplot(data = traj.CI,
              aes(x = date,
                  y = median, ymin = low_95, ymax = up_95,
                  color = state.name,
                  fill = state.name,
                  group = state.name))

  p <- p +  geom_ribbon(data = traj.CI,
                        aes(x = date,
                            y = median, ymin = low_50, ymax = up_50,
                            color = state.name,
                            fill = state.name,
                            group = state.name),alpha = .5, inherit.aes=TRUE, color=FALSE)

  p <- p +  scale_fill_manual(values = c(color.this.var),labels = longnames) + scale_color_manual(values = c(color.this.var), labels = longnames)
  p <- p + geom_line() + geom_ribbon(alpha = 0.2, color = FALSE)

  # if(!is.null(data)){
  #   p <- p + geom_point(data = data,
  #                       aes(x = date, y = value,
  #                           color = state.name),
  #                       alpha = 0.7,
  #                       inherit.aes = FALSE)
  # }

  p <- p + theme_bw() + theme(legend.title = element_blank())
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "2 weeks" , date_labels = "%d-%b-%y")
  p <- p + scale_y_continuous(limits = c(0,0.75), breaks = seq(from = 0, to = 0.75, by = 0.05))
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_text(size = 12, face = "bold"))
  #  p <- p + ylab(paste0("Number  ", as.character(longnames[var.to.plot]))) + xlab(NULL)
  p <- p + ylab("Probability") + xlab(NULL)

  #p <- p + labs(title = title.input)
  #p<-p+theme(plot.title = element_text(size = 12, hjust = 0.5, face="bold"))
  p <- p + facet_grid(. ~ title)

  p

}

########################################


# traj.CI <- Alpha_t
# data.in <- NULL
# vars.to.plot <- "Alpha_t"
# chart.title <- "Delta(t) Over Time"
# time.steps.4plot <- 150
# Alpha_t_plot <- plot.together.R0(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
# Alpha_t_plot
#
# ggplotly(R0_t_plot)


traj.CI <- rbind(Alpha_t, Kappa_t, Delta_t)
vars.to.plot <- c("Alpha_t","Kappa_t","Delta_t")
data.in <- NULL
chart.title <- "Population-Average Probabilities of Severe Illness"
time.steps.4plot <- 145
Alpha_t_plot <- plot.together.R0(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
Alpha_t_plot

# pdf(file = path(code.dir, "integrated/figs/use_in_paper/Alpha_t_plot_TALL.pdf"), width=10, height =10)
# Alpha_t_plot
# dev.off()




























########################################################################################
########################################################################################
## PLOTTING SCENARIOS
########################################################################################
########################################################################################

###### OUTPUT SPECIFYING SCENARIO ALPHA KAPPA DELTA
weighted.avg.scenarios.overall <- weighted.avg.scenarios.overall


########################################################################################################################
########################################################################################################################


###### FUNCTIONS
posterior.CI.num <- function(posterior.var, round.by){
  median = quantile(posterior.var, c(.5), na.rm=TRUE)
  low_95 = quantile(posterior.var, c(.025), na.rm=TRUE)
  #mean = mean(posterior.var)
  up_95 = quantile(posterior.var, c(.975), na.rm=TRUE)
  up_50 = quantile(posterior.var,.75,na.rm=TRUE)
  low_50 = quantile(posterior.var,.25,na.rm=TRUE)
  posterior.CI <- as.data.frame(cbind(low_95,low_50,median,up_50,up_95))
  posterior.CI <- round(posterior.CI, digits=round.by)
  return(posterior.CI)
}

function.name <- function(AKD, scenario){
  if (AKD==0) stem <- "Alpha_t."
  if (AKD==1) stem <- "Kappa_t."
  if (AKD==2) stem <- "Delta_t."
  function.name.out <- paste0(stem,scenario)
}

###### GENERAL TO ALL ALPHA KAPPA DELTA _t's
startObservedData <- as.Date("2020-03-01")
intervention_date <- as.Date(intervention_date)
intervention_date_numeric <- as.numeric(intervention_date - as.Date("2020-03-01"))
todays_date_numeric = as.numeric(as.Date("2020-07-16") - as.Date("2020-03-01"))

Alpha_decrease_date <- as.numeric( as.Date("2020-06-01") - as.Date("2020-03-01"))
Alpha_stabilize_date <- as.numeric( as.Date("2020-07-01") - as.Date("2020-03-01"))
Final_plot_date <- as.numeric( as.Date("2020-07-20") - as.Date("2020-03-01") )

Alpha_t.time<-c(
  0,
  Alpha_decrease_date-1,
  Alpha_decrease_date,
  Alpha_stabilize_date,
  Final_plot_date
)

###### GET ALPHA KAPPA DELTA INPUTS AT EACH TIME POINT
scenario<-0
# Alpha1.CI.0 <- Alpha1.CI
# Alpha2.CI.0 <- Alpha2.CI
# Kappa1.CI.0 <- Kappa1.CI
# Kappa2.CI.0 <- Kappa2.CI
# Delta1.CI.0 <- Delta1.CI
# Delta2.CI.0 <- Delta2.CI
Alpha1.CI.0 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[3,1], round.by=4)
Alpha2.CI.0 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[3,4], round.by=4)
Kappa1.CI.0 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[3,2], round.by=4)
Kappa2.CI.0<- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[3,5], round.by=4)
Delta1.CI.0 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[3,3], round.by=4)
Delta2.CI.0 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[3,6], round.by=4)


scenario<-1
Alpha1.CI.1 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[scenario,1], round.by=4)
Alpha2.CI.1 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[scenario,4], round.by=4)
Kappa1.CI.1 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[scenario,2], round.by=4)
Kappa2.CI.1<- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[scenario,5], round.by=4)
Delta1.CI.1 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[scenario,3], round.by=4)
Delta2.CI.1 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[scenario,6], round.by=4)

scenario<-2
Alpha1.CI.2 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[scenario,1], round.by=4)
Alpha2.CI.2 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[scenario,4], round.by=4)
Kappa1.CI.2 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[scenario,2], round.by=4)
Kappa2.CI.2<- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[scenario,5], round.by=4)
Delta1.CI.2 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[scenario,3], round.by=4)
Delta2.CI.2 <- posterior.CI.num(posterior.var=weighted.avg.scenarios.overall[scenario,6], round.by=4)

##### SPECIFY INPUTS TO GET FULL FUNCTIONS


Alpha.Kappa.Delta.4PLOT.together <- vector("list", length <- 3)

for (scenario.idx in 1:3){
  scenario <- scenario.idx-1

if (scenario==0){
  Alpha1.CI.IN <- Alpha1.CI.0
  Alpha2.CI.IN <- Alpha2.CI.0
  Kappa1.CI.IN <- Kappa1.CI.0
  Kappa2.CI.IN <- Kappa2.CI.0
  Delta1.CI.IN <- Delta1.CI.0
  Delta2.CI.IN <- Delta2.CI.0
}
  if (scenario==1){
    Alpha1.CI.IN <- Alpha1.CI.1
    Alpha2.CI.IN <- Alpha2.CI.1
    Kappa1.CI.IN <- Kappa1.CI.1
    Kappa2.CI.IN <- Kappa2.CI.1
    Delta1.CI.IN <- Delta1.CI.1
    Delta2.CI.IN <- Delta2.CI.1
  }
  if (scenario==2){
    Alpha1.CI.IN <- Alpha1.CI.2
    Alpha2.CI.IN <- Alpha2.CI.2
    Kappa1.CI.IN <- Kappa1.CI.2
    Kappa2.CI.IN <- Kappa2.CI.2
    Delta1.CI.IN <- Delta1.CI.2
    Delta2.CI.IN <- Delta2.CI.2
  }
  ###
  Alpha_t <- rbind(
    Alpha1.CI.IN, # 0,
    Alpha1.CI.IN, # Alpha_decrease_date-1,
    Alpha1.CI.IN, # Alpha_decrease_date,
    Alpha2.CI.IN, # Alpha_stabilize_date,
    Alpha2.CI.IN # Alpha_stabilize_date + 400
  )

  Alpha_t$date <- Alpha_t.time + startObservedData
  function.name.out.Alpha <- function.name(AKD=0, scenario=scenario)
  Alpha_t$state.name <- rep(function.name.out.Alpha,nrow(Alpha_t))
  rownames(Alpha_t)<-1:nrow(Alpha_t)
  function.name.out.Alpha <- Alpha_t

  ###
  Kappa_t <- rbind(
    Kappa1.CI.IN, # 0,
    Kappa1.CI.IN, # Alpha_decrease_date-1,
    Kappa1.CI.IN, # Alpha_decrease_date,
    Kappa2.CI.IN, # Alpha_stabilize_date,
    Kappa2.CI.IN # Alpha_stabilize_date + 400
  )

  Kappa_t$date <- Alpha_t.time + startObservedData
  function.name.out.Kappa <- function.name(AKD=1, scenario=scenario)
  Kappa_t$state.name <- rep(function.name.out.Kappa,nrow(Kappa_t))
  rownames(Kappa_t)<-1:nrow(Kappa_t)
  function.name.out.Kappa <- Kappa_t

  ###
  Delta_t <- rbind(
    Delta1.CI.IN, # 0,
    Delta1.CI.IN, # Alpha_decrease_date-1,
    Delta1.CI.IN, # Alpha_decrease_date,
    Delta2.CI.IN, # Alpha_stabilize_date,
    Delta2.CI.IN # Alpha_stabilize_date + 400
  )

  Delta_t$date <- Alpha_t.time + startObservedData
  function.name.out.Delta <- function.name(AKD=2, scenario=scenario)
  Delta_t$state.name <- rep(function.name.out.Delta,nrow(Delta_t))
  rownames(Delta_t)<-1:nrow(Delta_t)
  function.name.out.Delta <- Delta_t

  Alpha.Kappa.Delta.4PLOT <- rbind(function.name.out.Alpha, function.name.out.Kappa, function.name.out.Delta)
  Alpha.Kappa.Delta.4PLOT.together[[scenario.idx]] <- Alpha.Kappa.Delta.4PLOT
}


###### DEFINE PLOT FUNCTION
plot.together.AKD <- function(traj.CI=traj.CI, data.in=data.in, init.date.data=NULL, date.offset.4plot=NULL, time.steps.4plot, vars.to.plot) {

  if(is.null(init.date.data)) {
    init.date.data <- "2020-03-01"}
  if(is.null(date.offset.4plot)){
    date.offset.4plot=0}

  ###########
  ### traj.CI
  ###########

  ## Filter only to variable of interest
  traj.CI <- traj.CI %>%  dplyr::filter(state.name %in% vars.to.plot)

  ## Select only more recent dates
  init.date <- init.date.data
  init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
  startDatePlot <- init.date - date.offset.4plot -1 #15
  endDatePlot <- startDatePlot + time.steps.4plot #- 40  # the constant 40 because the traj are not aligned to start date
  traj.CI <- traj.CI %>% dplyr::filter(date >= startDatePlot) %>% dplyr::filter(date < endDatePlot)

  ## Add title
  traj.CI$title <- chart.title

  #####################
  ### colors and names
  #####################

  longnames <- c("Alpha(t) Protect=Observed",
                 "Kappa(t) Protect=Observed",
                 "Delta(t) Protect=Observed",
                 "Alpha(t) Protect=High",
                 "Kappa(t) Protect=High",
                 "Delta(t) Protect=High",
                 "Alpha(t) Protect=Moderate",
                 "Kappa(t) Protect=Moderate",
                 "Delta(t) Protect=Moderate"
  )

  names(longnames) <-  c(
    "Alpha_t.0",
    "Kappa_t.0",
    "Delta_t.0",
    "Alpha_t.1",
    "Kappa_t.1",
    "Delta_t.1",
    "Alpha_t.2",
    "Kappa_t.2",
    "Delta_t.2"
  )

  ## Colors

  cols.list <- c(
    ## SCENARIOS
    "aquamarine4",
    "aquamarine3",
    "aquamarine2",

    "deepskyblue4",
    "deepskyblue",
    "darkslategray2",

    "deeppink4",
    "deeppink",
    "darkorange3"
  )
  names(cols.list) <- names(longnames)
  color.this.var <- as.character(cols.list[vars.to.plot])

  ##################
  ### CREATE PLOT
  ##################

  p <- ggplot(data = traj.CI,
              aes(x = date,
                  y = median, ymin = low_95, ymax = up_95,
                  color = state.name,
                  fill = state.name,
                  group = state.name))

  p <- p +  geom_ribbon(data = traj.CI,
                        aes(x = date,
                            y = median, ymin = low_50, ymax = up_50,
                            color = state.name,
                            fill = state.name,
                            group = state.name),alpha = .5, inherit.aes=TRUE, color=FALSE)

  p <- p +  scale_fill_manual(values = c(color.this.var),labels = longnames) + scale_color_manual(values = c(color.this.var), labels = longnames)
  p <- p + geom_line() + geom_ribbon(alpha = 0.2, color = FALSE)

  # if(!is.null(data)){
  #   p <- p + geom_point(data = data,
  #                       aes(x = date, y = value,
  #                           color = state.name),
  #                       alpha = 0.7,
  #                       inherit.aes = FALSE)
  # }

  p <- p + theme_bw() + theme(legend.title = element_blank())
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "2 weeks" , date_labels = "%d-%b-%y")
  p <- p + scale_y_continuous(limits = c(0,0.7), breaks = seq(from = 0, to = 0.7, by = 0.05))
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_text(size = 12, face = "bold"))
  #  p <- p + ylab(paste0("Number  ", as.character(longnames[var.to.plot]))) + xlab(NULL)
  p <- p + ylab("Probability") + xlab(NULL)

  #p <- p + labs(title = title.input)
  #p<-p+theme(plot.title = element_text(size = 12, hjust = 0.5, face="bold"))
  p <- p + facet_grid(. ~ title)

  p

}


###### GET OUTPUT

# traj.CI <- Alpha.Kappa.Delta.4PLOT.together[[1]]
# vars.to.plot <- c("Alpha_t.0","Kappa_t.0","Delta_t.0")
# data.in <- NULL
# chart.title <- "Probabilities of Severe Illness"
# time.steps.4plot <- 145
# AKD_t_plot <- plot.together.AKD(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
# AKD_t_plot
#
#
# traj.CI <- Alpha.Kappa.Delta.4PLOT.together[[2]]
# vars.to.plot <- c("Alpha_t.1","Kappa_t.1","Delta_t.1")
# data.in <- NULL
# chart.title <- "Probabilities of Severe Illness"
# time.steps.4plot <- 145
# AKD_t_plot <- plot.together.AKD(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
# AKD_t_plot
#
# traj.CI <- Alpha.Kappa.Delta.4PLOT.together[[3]]
# vars.to.plot <- c("Alpha_t.2","Kappa_t.2","Delta_t.2")
# data.in <- NULL
# chart.title <- "Probabilities of Severe Illness"
# time.steps.4plot <- 145
# AKD_t_plot <- plot.together.AKD(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
# AKD_t_plot


traj.CI <- rbind(Alpha.Kappa.Delta.4PLOT.together[[1]], Alpha.Kappa.Delta.4PLOT.together[[2]], Alpha.Kappa.Delta.4PLOT.together[[3]])
vars.to.plot <- c("Alpha_t.0","Kappa_t.0","Delta_t.0","Alpha_t.1","Kappa_t.1","Delta_t.1", "Alpha_t.2","Kappa_t.2","Delta_t.2")
data.in <- NULL
chart.title <- "Probabilities of Severe Illness: Scenarios"
time.steps.4plot <- 145
AKD_t_plot <- plot.together.AKD(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
AKD_t_plot

# pdf(file = path(code.dir, "integrated/figs/use_in_paper/AKD_t_plot_MEDIANS.pdf"), width=10, height =10)
# AKD_t_plot
# dev.off()


