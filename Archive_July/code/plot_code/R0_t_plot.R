
#   TO DO:
# GET 50% INTERVAL
# CREATE DATA FRAME WITH R0(t) AND CI BASED ON POSTERIOR CI. PUT IT IN FORMAT FOR PLOTTING SINGLE
# WILL NEED TO ADJUST SINCE R0(t) DOESN'T HAVE A COLOR ASSOCIATED

posterior.CI <- function(posterior.var){
  median = quantile(posterior.var, c(.5), na.rm=TRUE)
  low_95 = quantile(posterior.var, c(.025), na.rm=TRUE)
  #mean = mean(posterior.var)
  up_95 = quantile(posterior.var, c(.975), na.rm=TRUE)
  up_50 = quantile(posterior.var,.75,na.rm=TRUE)
  low_50 = quantile(posterior.var,.25,na.rm=TRUE)
  posterior.CI <- as.data.frame(cbind(low_95,low_50,median,up_50,up_95))
  return(posterior.CI)
}


out_R0 <- as.data.frame(cbind(ABC_out$param[,1],ABC_out$weights))
out_R0redux1<- as.data.frame(cbind(ABC_out$param[,4],ABC_out$weights))
out_R0redux2<- as.data.frame(cbind(ABC_out$param[,9],ABC_out$weights))

R0_x_redux1 <- out_R0*out_R0redux1
R0_x_redux2 <- out_R0*out_R0redux2

# GET QUANTILES FOR VARIABLE
R0.posterior.CI <- round(posterior.CI(out_R0),digits=4)
R0redux1.posterior.CI <- round(posterior.CI(R0_x_redux1),digits=4)
R0redux2.posterior.CI <- round(posterior.CI(R0_x_redux2),digits=4)

out_t0 <- as.data.frame(cbind(ABC_out$param[,3],ABC_out$weights))
out_t0.posterior.CI <- round(posterior.CI(out_t0),digits=4)

# R0redux1.posterior.CI <- round(posterior.CI(out_R0redux1),digits=4)
# R0redux2.posterior.CI <- round(posterior.CI(out_R0redux2),digits=4)

out_r <- as.data.frame(cbind(ABC_out$param[,2],ABC_out$weights))
out_r.posterior.CI <- round(posterior.CI(out_r),digits=4)
out_r.posterior.CI

########################################################################################
########################################################################################
## R0 PLOT
########################################################################################
########################################################################################

startObservedData <- as.Date("2020-03-01")
initDatePlot <- startObservedData-start_time

intervention_date <- as.Date(intervention_date)
intervention_date_numeric <- as.numeric(intervention_date - as.Date("2020-03-01"))
#todays_date_numeric = as.numeric(Sys.Date() - as.Date("2020-03-01"))
todays_date_numeric = as.numeric(as.Date("2020-07-20") - as.Date("2020-03-01"))

# Beta_t = Beta_t.fn
# Beta_y = Beta_y.fn


Beta_t<- c(
  0,                              #1
  (start_time),                #2
  (start_time+1),                #3
  (start_time+intervention_date_numeric),   #2020-03-12    #4
  (start_time+intervention_date_numeric+15),#2020-03-27    #5
  (start_time+intervention_date_numeric+46),#2020-04-27    #6
  (start_time+intervention_date_numeric+114),#2020-07-04   #7
  (start_time+todays_date_numeric))#DATE FAR IN FUTURE   #9

# Beta_y<- c(
#   Br, #1
#   Br, #2
#   Br, #3
#   Br, #4
#   (Br*R0_redux1), #5
#   (Br*R0_redux1), #6
#   (Br*R0_redux2), #7
#   (Br*R0_redux2*.9), #8
#   (Br*R0_redux2)*.9) #9

# R0_t <- c(
#     R0, #1
#     R0, #2
#     R0, #3
#     R0, #4
#     (R0*R0_redux1), #5
#     (R0*R0_redux1), #6
#     (R0*R0_redux2), #7
#     (R0*R0_redux2*.9), #8
#     (R0*R0_redux2*.9)) #9

#################################
## R0(t) function

R0_t <- rbind(
  R0.posterior.CI, #1
  R0.posterior.CI, #2
  R0.posterior.CI, #3
  R0.posterior.CI, #4
  (R0redux1.posterior.CI), #5
  (R0redux1.posterior.CI), #6
  (R0redux2.posterior.CI), #7
  (R0redux2.posterior.CI)) #8

#################################
## Put R0(t) function in format for plots

rownames(R0_t)<-1:nrow(R0_t)
R0_t$date <- Beta_t + initDatePlot
R0_t$state.name <- rep("R0_t",nrow(R0_t))

## Quick plot of R0(t) median
p <- ggplot(R0_t, aes(x=date, y=median)) +
  geom_line() +
  xlab("Date") + ylab("R0(t)") + ylim(0,6) +
  scale_x_date(date_breaks = "2 weeks",limit=c(as.Date("2020-03-01"),as.Date("2020-07-20"))) + ggtitle("R0(t), with model-estimated reduction trend")
p

###########################
## Plot of R0(t) with CI

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
                 "R0(t)")

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
                         "R",
                         "R0_t"
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
    "grey50"
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
  p <- p + scale_y_continuous(limits = c(0,5), breaks = seq(from = 0, to = 5, by = 0.5))
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_text(size = 12, face = "bold"))
  #  p <- p + ylab(paste0("Number  ", as.character(longnames[var.to.plot]))) + xlab(NULL)
  p <- p + ylab("R0(t)") + xlab(NULL)

  #p <- p + labs(title = title.input)
  #p<-p+theme(plot.title = element_text(size = 12, hjust = 0.5, face="bold"))
  p <- p + facet_grid(. ~ title)

  p

}

traj.CI <- R0_t
data.in <- NULL
vars.to.plot <- "R0_t"
chart.title <- "R0(t) Over Time"
time.steps.4plot <- 145
R0_t_plot <- plot.together.R0(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
R0_t_plot

###########################
## OUTPUT TO PRINT TO FILE

# pdf(file = path(code.dir, "integrated/figs/use_in_paper/R0_t_plot.pdf"), width=10, height =10)
# R0_t_plot
# dev.off()


########################################################################################
########################################################################################
## R0 PLOT SCENARIOS
########################################################################################
########################################################################################

### SCENARIO ORIGINAL
R0_t <- rbind(
  R0.posterior.CI, #1
  R0.posterior.CI, #2
  R0.posterior.CI, #3
  R0.posterior.CI, #4
  (R0redux1.posterior.CI), #5
  (R0redux1.posterior.CI), #6
  (R0redux2.posterior.CI), #7
  (R0redux2.posterior.CI)) #8

rownames(R0_t)<-1:nrow(R0_t)
R0_t$date <- Beta_t + initDatePlot
R0_t$state.name <- rep("R0_t",nrow(R0_t))

### SCENARIO MODERATE
R0_t.NOTHING <- rbind(
  R0.posterior.CI, #1
  R0.posterior.CI, #2
  R0.posterior.CI, #3
  R0.posterior.CI, #4
  (R0.posterior.CI), #5
  (R0.posterior.CI), #6
  (R0.posterior.CI), #7
  (R0.posterior.CI))
rownames(R0_t.NOTHING)<-1:nrow(R0_t.NOTHING)
R0_t.NOTHING$date <- Beta_t + initDatePlot
R0_t.NOTHING$state.name <- rep("R0_t.NOTHING",nrow(R0_t.NOTHING))


### SCENARIO MODERATE
Rt_begins_to_drop_date <- as.numeric( as.Date("2020-03-12") - as.Date("2020-03-01"))
Rt_stabilize_date <- as.numeric( as.Date("2020-04-27") - as.Date("2020-03-01"))

R0_t.MODERATE.time <- c(
      0,
      Rt_begins_to_drop_date,
      Rt_stabilize_date,
      as.numeric( as.Date("2020-07-20") - as.Date("2020-03-01"))
    )
R0_t.MODERATE.time <- R0_t.MODERATE.time + as.Date("2020-03-01")

R0_t.MODERATE <- rbind(
  R0.posterior.CI, #3
  R0.posterior.CI, #4
  (R0redux2.posterior.CI), #5
  (R0redux2.posterior.CI) #6
)
rownames(R0_t.MODERATE)<-1:nrow(R0_t.MODERATE)
R0_t.MODERATE$date <- R0_t.MODERATE.time
R0_t.MODERATE$state.name <- rep("R0_t.MODERATE",nrow(R0_t.MODERATE))


#####################################################################################
plot.together.R0.scenarios <- function(traj.CI=traj.CI, data.in=data.in, init.date.data=NULL, date.offset.4plot=NULL, time.steps.4plot, vars.to.plot) {

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

  longnames <- c("R0(t) NPI=Observed",
                 "R0(t) NPI=Moderate",
                 "R0(t) NPI=None")

  names(longnames) <-  c("R0_t",
                         "R0_t.MODERATE",
                         "R0_t.NOTHING"
  )

  ## Colors

  cols.list <- c("deeppink1",
    "cornflowerblue",
    "antiquewhite4"
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
  p <- p + scale_y_continuous(limits = c(0,5), breaks = seq(from = 0, to = 5, by = 0.5))
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_text(size = 12, face = "bold"))
  #  p <- p + ylab(paste0("Number  ", as.character(longnames[var.to.plot]))) + xlab(NULL)
  p <- p + ylab("R0(t)") + xlab(NULL)

  #p <- p + labs(title = title.input)
  #p<-p+theme(plot.title = element_text(size = 12, hjust = 0.5, face="bold"))
  p <- p + facet_grid(. ~ title)

  p

}



### PLOT ALL THREE SCENARIOS
traj.CI <- rbind(R0_t.NOTHING, R0_t.MODERATE, R0_t)
vars.to.plot <- c("R0_t.NOTHING","R0_t.MODERATE","R0_t")
data.in <- NULL
chart.title <- "R0(t): Scenarios"
time.steps.4plot <- 145
R0_t_scenarios_plot <- plot.together.R0.scenarios(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
R0_t_scenarios_plot

# pdf(file = path(code.dir, "integrated/figs/use_in_paper/R0_t_plot_SCENARIOS.pdf"), width=10, height =10)
# R0_t_scenarios_plot
# dev.off()




