
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



########################################################################################
## PLOTTING MULTIPLE TOGETHER FUNCTION
########################################################################################

date.offset.4plot=0
time.steps.4plot= as.numeric(as.Date("2020-07-18") - as.Date("2020-03-01"))
traj.CI <- traj.0
data.in <- data.in.tmp


vars.to.plot <- c("I","Itot")
chart.title <- "Current Observed and Estimated Total Illnesses"
curr.illness.variables <- plot.together(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
curr.illness.variables
pdf(file = path(fig.dir, "curr.illness.variables.pdf"), width=10, height =10)
curr.illness.variables
dev.off()

vars.to.plot <- c("Idetectcum","Itotcum")
chart.title <- "Cumulative Observed and Estimated Total Illnesses"
cum.illness.variables <- plot.together(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
cum.illness.variables
pdf(file = path(fig.dir, "cum.illness.variables.pdf"), width=10, height =10)
cum.illness.variables
dev.off()

vars.to.plot <- c("Htot","Q", "V","D_new")
chart.title <- "Healthcare Variables"
curr.healthcare.variables.50 <- plot.together.50(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
curr.healthcare.variables.50
pdf(file = path(fig.dir, "curr.healthcare.variables.50.pdf"), width=10, height =10)
curr.healthcare.variables.50
dev.off()

traj.CI <- traj.scenario0
vars.to.plot <- c("Htot","Q", "V","D_new")
chart.title <- "Healthcare Variables"
curr.healthcare.variables <- plot.together(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
curr.healthcare.variables
pdf(file = path(fig.dir, "curr.healthcare.variables.pdf"), width=10, height =10)
curr.healthcare.variables
dev.off()

vars.to.plot <- c("H_new", "D_new")
chart.title <- "Daily Hospitalizations and Deaths"
new.healthcare.variables <- plot.together(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
new.healthcare.variables
pdf(file = path(fig.dir, "new.healthcare.variables.pdf"), width=10, height =10)
new.healthcare.variables
dev.off()

vars.to.plot <- c("Htotcum","Qcum", "Vcum", "D")
chart.title <- "Cumulative Hospitalizations and Deaths"
cum.healthcare.variables.50 <- plot.together.50(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
cum.healthcare.variables.50
pdf(file = path(fig.dir, "cum.healthcare.variables.50.pdf"), width=10, height =10)
cum.healthcare.variables.50
dev.off()

vars.to.plot <- c("Htotcum","Qcum", "Vcum", "D")
chart.title <- "Cumulative Hospitalizations and Deaths"
cum.healthcare.variables <- plot.together(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
cum.healthcare.variables
pdf(file = path(fig.dir, "cum.healthcare.variables.50.pdf"), width=10, height =10)
cum.healthcare.variables
dev.off()


plot.together <- function(traj.CI=traj.CI, data.in=data.in, init.date.data=NULL, date.offset.4plot=NULL, time.steps.4plot, vars.to.plot) {

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
  startDatePlot <- init.date - date.offset.4plot #15
  endDatePlot <- startDatePlot + time.steps.4plot #- 40  # the constant 40 because the traj are not aligned to start date
  traj.CI <- traj.CI %>% dplyr::filter(date >= startDatePlot) %>% dplyr::filter(date < endDatePlot)

  ## Add title
  traj.CI$title <- chart.title

  ###########
  ### data.in
  ###########

  ## Data in -- plot only for the selected variable
  if(is.null(data.in)){data=NULL}
  else{

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
  color.this.var <- as.character(cols.list[vars.to.plot])

  ##################
  ### CREATE PLOT
  ##################

  p <- ggplot(data = traj.CI,
              aes(x = date,
                  y = mean, ymin = low_95, ymax = up_95,
                  color = state.name,
                  fill = state.name,
                  group = state.name))

  p <- p +  geom_ribbon(data = traj.CI,
                        aes(x = date,
                            y = mean, ymin = low_50, ymax = up_50,
                            color = state.name,
                            fill = state.name,
                            group = state.name),alpha = .5, inherit.aes=TRUE, color=FALSE)

  p <- p +  scale_fill_manual(values = c(color.this.var),labels = longnames) + scale_color_manual(values = c(color.this.var), labels = longnames)
  p <- p + geom_line() + geom_ribbon(alpha = 0.2, color = FALSE)

  if(!is.null(data)){
    p <- p + geom_point(data = data,
                      aes(x = date, y = value,
                          color = state.name),
                      alpha = 0.7,
                      inherit.aes = FALSE)
  }

  p <- p + theme_bw() + theme(legend.title = element_blank())
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "2 weeks" , date_labels = "%d-%b-%y")
  p <- p + scale_y_continuous(labels = scales::comma)
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_text(size = 12, face = "bold"))
  #  p <- p + ylab(paste0("Number  ", as.character(longnames[var.to.plot]))) + xlab(NULL)
  p <- p + ylab("Number in compartment") + xlab(NULL)

  #p <- p + labs(title = title.input)
  #p<-p+theme(plot.title = element_text(size = 12, hjust = 0.5, face="bold"))
  p <- p + facet_grid(. ~ title)

  p

}

plot.together.50 <- function(traj.CI=traj.CI, data.in=data.in, init.date.data=NULL, date.offset.4plot=NULL, time.steps.4plot, vars.to.plot) {

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
  if(is.null(data.in)){data=NULL}
  else{

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
  color.this.var <- as.character(cols.list[vars.to.plot])

  ##################
  ### CREATE PLOT
  ##################

  p <- ggplot(data = traj.CI,
              aes(x = date,
                  y = mean, ymin = low_50, ymax = up_50,
                  color = state.name,
                  fill = state.name,
                  group = state.name))

  # p <- p +  geom_ribbon(data = traj.CI,
  #                       aes(x = date,
  #                           y = mean, ymin = low_50, ymax = up_50,
  #                           color = state.name,
  #                           fill = state.name,
  #                           group = state.name),alpha = .5, inherit.aes=TRUE, color=FALSE)

  p <- p +  scale_fill_manual(values = c(color.this.var),labels = longnames) + scale_color_manual(values = c(color.this.var), labels = longnames)
  p <- p + geom_line() + geom_ribbon(alpha = 0.5, color = FALSE)

  if(!is.null(data)){
    p <- p + geom_point(data = data,
                        aes(x = date, y = value,
                            color = state.name),
                        alpha = 0.7,
                        inherit.aes = FALSE)
  }

  p <- p + theme_bw() + theme(legend.title = element_blank())
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "2 weeks" , date_labels = "%d-%b-%y")
  p <- p + scale_y_continuous(labels = scales::comma)
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_text(size = 12, face = "bold"))
  #  p <- p + ylab(paste0("Number  ", as.character(longnames[var.to.plot]))) + xlab(NULL)
  p <- p + ylab("Number in compartment") + xlab(NULL)

  #p <- p + labs(title = title.input)
  #p<-p+theme(plot.title = element_text(size = 12, hjust = 0.5, face="bold"))
  p <- p + facet_grid(. ~ title)

  p

}

################################################################################################################################
################################################################################################################################
################################################################################################################################





################################################################################################################################
################################################################################################################################
################################################################################################################################

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


