


# Manual ylim so I can fix height across charts

plot.model.single.SCENARIO <- function(traj.CI, data.in, time.steps.4plot=NULL, ymax=NULL, plot.capacity=NULL, var.to.plot=NULL, intervention_date=NULL) {

  ## Filter only to variable of interest
  traj.CI <- traj.CI %>%  dplyr::filter(state.name==var.to.plot)

  date.offset.4plot <- 0

  ## Select only more recent dates
  init.date <- "2020-03-01"
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
  # p <- p + theme_bw() + theme(legend.position = "top", legend.box = "horizontal")
  #
  # p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "1 month" , date_labels = "%d-%b-%y")
  #
  # p <- p + theme(axis.text.x = element_text(angle = 90),
  # #   p <- p + theme(axis.text.x = element_blank(),
  # # #               strip.text.x = element_text(size = 12, face = "bold"))
  #                strip.text.x = element_blank() )
  #
  #   p <- p + ylab(NULL) + xlab(NULL)



  p <- p + theme_bw() + theme(legend.position = "top", legend.box = "horizontal")
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "1 month" , date_labels = "%d-%b-%y")
  p <- p + theme(axis.text.x = element_blank(), #element_text(angle = 90),
                 strip.text.x = element_blank() ) #element_text(size = 12, face = "bold"))
  #  p <- p + ylab(paste0("Number  ", as.character(longnames[var.to.plot]))) + xlab(NULL)
  p <- p + ylab(NULL) + xlab(NULL)



  if(!is.null(ymax)){
    p <- p + scale_y_continuous(limits=c(0,ymax) , labels = scales::comma, breaks=seq(0,ymax,ymax/5))
  }
  else {  p <- p + scale_y_continuous(labels = scales::comma)}

  p

}

plot.model.single.SCENARIO.XAxis <- function(traj.CI, data.in, time.steps.4plot=NULL, ymax=NULL, plot.capacity=NULL, var.to.plot=NULL, intervention_date=NULL) {

  date.offset.4plot <- 0
  ## Filter only to variable of interest
  traj.CI <- traj.CI %>%  dplyr::filter(state.name==var.to.plot)

  ## Select only more recent dates
  init.date <- "2020-03-01"
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
  # p <- p + theme_bw() + theme(legend.position = "top", legend.box = "horizontal")
  #
  # p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "1 month" , date_labels = "%d-%b-%y")
  #
  # p <- p + theme(axis.text.x = element_text(angle = 90),
  # #   p <- p + theme(axis.text.x = element_blank(),
  # # #               strip.text.x = element_text(size = 12, face = "bold"))
  #                strip.text.x = element_blank() )
  #
  #   p <- p + ylab(NULL) + xlab(NULL)



  p <- p + theme_bw() + theme(legend.position = "top", legend.box = "horizontal")
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "1 month" , date_labels = "%d-%b-%y")
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_blank() ) #element_text(size = 12, face = "bold"))
  #  p <- p + ylab(paste0("Number  ", as.character(longnames[var.to.plot]))) + xlab(NULL)
  p <- p + ylab(NULL) + xlab(NULL)



  if(!is.null(ymax)){
    p <- p + scale_y_continuous(limits=c(0,ymax) , labels = scales::comma, breaks=seq(0,ymax,ymax/5))
  }
  else {  p <- p + scale_y_continuous(labels = scales::comma)}


  p

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

#
# Alpha.filter <- Alpha.filter.prior.mean
# Kappa.filter <- Kappa.filter.prior.mean
# Delta.filter <- Delta.filter.prior.mean
#
# traj.SHIELD2 <- model.output.to.plot.subpop.scenarios(ABC.out.mat=ABC.out.mat, par.vec.length=par.vec.length, iter=iter, time.steps=time.steps, vars.to.plot = vars.to.plot, scenario = scenario,
#                                                      Alpha.filter=Alpha.filter,Kappa.filter=Kappa.filter,Delta.filter=Delta.filter)
#
# traj.SHIELD$scenario <- rep("NPI:Nothing | Protect:100%", times=nrow(traj.SHIELD))
# traj.SHIELD2$scenario <- rep("NPI:Moderate | Protect:100%", times=nrow(traj.SHIELD))
#
# traj.COMBINE <- rbind(traj.SHIELD,traj.SHIELD2)
#
# #p <- ggplot(transform(traj.CI.area, state.name = factor(state.name, levels=vars.to.plot), scenario = factor(scenario, levels= c("NPI:Nothing | Protect:100%","NPI:Moderate | Protect:100%") )    ))
# traj.COMBINE$scenario <- factor(traj.COMBINE$scenario, levels =  c("NPI:Nothing | Protect:100%","NPI:Moderate | Protect:100%"))



scenario.list <- list(traj.S1, traj.S2,traj.S3,traj.S4,traj.S5,traj.S6,traj.S7,traj.S8)
plot.single.I <- vector("list", length(scenario.list))
plot.single.H <- vector("list", length(scenario.list))
plot.single.D <- vector("list", length(scenario.list))

time.steps.4plot <- 150 #175 #plot.thru.curr.date
vars.to.plot <- vars.plus.R
plot.capacity <- TRUE

for (i in 1:length(scenario.list)){

  traj.scenario <- scenario.list[[i]]

  var.to.plot <- "I_detect_new"
  ymax=20000
  plot.capacity=NULL
  plot.single.I[[i]] <- plot.model.single.SCENARIO(traj.CI=traj.scenario, data.in=data.in.tmp,time.steps.4plot=time.steps.4plot, ymax=ymax, plot.capacity=plot.capacity, var.to.plot=var.to.plot,intervention_date=intervention_date)

  var.to.plot <- "Htot"
  ymax=24000
  plot.capacity=TRUE
  plot.single.H[[i]] <- plot.model.single.SCENARIO(traj.CI=traj.scenario, data.in=data.in.tmp,time.steps.4plot=time.steps.4plot, ymax=ymax, plot.capacity=plot.capacity, var.to.plot=var.to.plot,intervention_date=intervention_date)


  var.to.plot <- "D"
  ymax=8000
  plot.capacity=NULL
  plot.single.D[[i]] <- plot.model.single.SCENARIO.XAxis(traj.CI=traj.scenario, data.in=data.in.tmp,time.steps.4plot=time.steps.4plot, ymax=ymax, plot.capacity=plot.capacity, var.to.plot=var.to.plot,intervention_date=intervention_date)

}

for (i in 1:2){
  traj.scenario <- scenario.list[[i]]
  var.to.plot <- "D"
  ymax=35000
  plot.capacity=NULL
  plot.single.D[[i]] <- plot.model.single.SCENARIO.XAxis(traj.CI=traj.scenario, data.in=data.in.tmp,time.steps.4plot=time.steps.4plot, ymax=ymax, plot.capacity=plot.capacity, var.to.plot=var.to.plot,intervention_date=intervention_date)
}

combined.plot <- ggpubr::ggarrange( plot.single.I[[1]], plot.single.I[[2]], plot.single.I[[3]], plot.single.I[[4]], plot.single.I[[5]], plot.single.I[[6]], plot.single.I[[7]], plot.single.I[[8]],
           plot.single.H[[1]], plot.single.H[[2]], plot.single.H[[3]],   plot.single.H[[4]], plot.single.H[[5]],  plot.single.H[[6]],  plot.single.H[[7]], plot.single.H[[8]],
           plot.single.D[[1]], plot.single.D[[2]], plot.single.D[[3]],   plot.single.D[[4]], plot.single.D[[5]],  plot.single.D[[6]],  plot.single.D[[7]], plot.single.D[[8]],
           nrow=3, ncol=8 , common.legend = TRUE, legend = "bottom")
combined.plot

##
## ggpubr::ggexport(combined.plot, filename = path(code.dir, "integrated/figs/use_in_paper/scenarios8_integrated.pdf"), width=20,height=12)
##

# ## COMBINED PLOT WITHOUT 50%
# combined.plot <- ggpubr::ggarrange( plot.single.I[[1]], plot.single.I[[2]], plot.single.I[[3]], plot.single.I[[4]],  plot.single.I[[6]], plot.single.I[[7]],
#                             plot.single.H[[1]], plot.single.H[[2]], plot.single.H[[3]],   plot.single.H[[4]],  plot.single.H[[6]],  plot.single.H[[7]],
#                             plot.single.D[[1]], plot.single.D[[2]], plot.single.D[[3]],   plot.single.D[[4]],   plot.single.D[[6]],  plot.single.D[[7]],
#                             nrow=3, ncol=6 , common.legend = TRUE, legend = "bottom")
# combined.plot

# ggexport(combined.plot, filename = "combined_plot_Alpha2_RiskModelCalculated.pdf", width=20,height=12)
# ggexport(combined.plot.observed_Alpha_t, filename = "combined_plot_Alpha2_Observed_Alpha_t.pdf", width=20,height=12)

