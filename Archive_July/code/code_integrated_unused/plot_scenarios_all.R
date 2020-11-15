
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

scenario.vars <- c(
  "I_detect_new",
  "Htot",
  "Htotcum",
  "D"
)

plot.model.data.all.TEST <- function(traj.CI, data.in, init.date.data, date.offset.4plot, time.steps.4plot, vars.to.plot) {

  #print(str(traj.CI$scenario))

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

  #p <- ggplot(transform(traj.CI.area, state.name = factor(state.name, levels=vars.to.plot), scenario = factor(scenario, levels= c("NPI:Nothing | Protect:100%","NPI:Moderate | Protect:100%") )    ))
    p <- ggplot(transform(traj.CI.area, state.name = factor(state.name, levels=vars.to.plot) )    )

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

  #  p <- p + facet_wrap(~state.name, labeller = labeller(state.name = longnames), scales = "free_y")
  p <- p + facet_grid(cols= vars(state.name), rows= vars(scenario), labeller = labeller(state.name = longnames), scales = "free_y")
  #p <- p + facet_grid(state.name ~ scenario, labeller = labeller(state.name = longnames), scales = "free_y")

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
## TESTING OUTPUT
########################################################################################

## 0 SCENARIO 0: R0 DO NOTHING (NO INTERVENTIONS: PROJECTION USING MODEL ESTIMATED PARAMETERS)
## 1 SCENARIO 1: R0 DO NOTHING + SHIELD SPECIFIC POPULATIONS [Trump/Atlas Scenario]
## 2 SCENARIO 2: STAGE 3 SELF-ADAPTIVE R0
## 3 SCENARIO 3A: STAGE 3 SELF-ADAPTIVE R0 + SHEILD SPECIFIC POPULATIONS
## 4 SCENARIO 3B: STAGE 3 SELF-ADAPTIVE R0 + SHEILD SPECIFIC POPULATIONS at 50%
## 5 SCENARIO 6: OBSERVED TREND
## 6 SCENARIO 7A: OBSERVED TREND Rt + SHIELD SPECIFIC POPULATIONS
## 7 SCENARIO 7B: OBSERVED TREND Rt + SHIELD SPECIFIC POPULATIONS at 50%

## 8 SCENARIO 4: CONTINUE TOTAL LOCKDOWN 1 MONTH LONGER
## 9 SCENARIO 5: CONTINUE TOTAL LOCKDOWN 1 MONTH LONGER + SHIELD SPECIFIC POPULATIONS


ABC.out.mat <- ABC_out$param[1:1000,]
par.vec.length <- 100
iter <- 10
time.steps <- 300
vars.to.plot <- vars.plus.R
scenario = 1

Alpha.filter <- Alpha.filter.prior.mean
Kappa.filter <- Kappa.filter.prior.mean
Delta.filter <- Delta.filter.prior.mean

traj.SHIELD2 <- model.output.to.plot.subpop.scenarios(ABC.out.mat=ABC.out.mat, par.vec.length=par.vec.length, iter=iter, time.steps=time.steps, vars.to.plot = vars.to.plot, scenario = scenario,
                                                      Alpha.filter=Alpha.filter,Kappa.filter=Kappa.filter,Delta.filter=Delta.filter)


traj.SHIELD$scenario <- rep("NPI:Nothing | Protect:100%", times=nrow(traj.SHIELD))
traj.SHIELD2$scenario <- rep("NPI:Moderate | Protect:100%", times=nrow(traj.SHIELD))

traj.COMBINE <- rbind(traj.SHIELD,traj.SHIELD2)

#p <- ggplot(transform(traj.CI.area, state.name = factor(state.name, levels=vars.to.plot), scenario = factor(scenario, levels= c("NPI:Nothing | Protect:100%","NPI:Moderate | Protect:100%") )    ))
traj.COMBINE$scenario <- factor(traj.COMBINE$scenario, levels =  c("NPI:Nothing | Protect:100%","NPI:Moderate | Protect:100%"))


init.date.data="2020-03-01"
plot.thru.curr.date = as.numeric(Sys.Date() - as.Date(init.date.data))
time.steps.4.plot = 160 #175 #plot.thru.curr.date
data.in <- data.in.tmp
vars.to.plot <- scenario.vars #vars.plus.R

plot.SHIELD.TEST <-
  plot.model.data.all.TEST(traj.CI = traj.COMBINE, data.in = data.in, init.date.data = "2020-03-01", date.offset.4plot = 15, time.steps.4plot=time.steps.4.plot,
                           vars.to.plot=vars.to.plot)
plot.SHIELD.TEST




