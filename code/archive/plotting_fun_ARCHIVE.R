

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
## PLOTTING MULTIPLE TOGETHER
########################################################################################
## USED IN:
## plot.param.t
## CFR.IFR.plots
plot.together <- function(traj.CI=traj.CI, data.in=data.in, init.date.data=NULL, endDatePlot=endDatePlot, vars.to.plot, y.lab.in, y.max.in, chart.title) {
  
  if(is.null(init.date.data)) {
    init.date.data <- "2020-03-01"}
  
  ###########
  ### traj.CI
  ###########
  
  ## Filter only to variable of interest
  traj.CI <- traj.CI %>%  dplyr::filter(state.name %in% vars.to.plot)
  
  ## Select only more recent dates
  init.date <- init.date.data
  init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
  startDatePlot <- init.date #- date.offset.4plot -1 #15
  endDatePlot <- as.Date(endDatePlot) #startDatePlot + time.steps.4plot #- 40  # the constant 40 because the traj are not aligned to start date
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
                 "Delta(t)",
                 "r(t)",
                 "CFR",
                 "IFR"
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
    "Rt",
    "Alpha_t",
    "Kappa_t",
    "Delta_t",
    "r_t",
    "CFR",
    "IFR"
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
    "violetred1",
    "grey50",
    "grey50",
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
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "1 month" , date_labels = "%d-%b-%y")
  p <- p + scale_y_continuous(limits = c(0,y.max.in), breaks = seq(from = 0, to = y.max.in, by = y.max.in/10))
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_text(size = 12, face = "bold"))
  #  p <- p + ylab(paste0("Number  ", as.character(longnames[var.to.plot]))) + xlab(NULL)
  #p <- p + ylab("Probability") + xlab(NULL)
  p <- p + ylab(y.lab.in) + xlab(NULL)
  #p <- p + labs(title = title.input)
  #p<-p+theme(plot.title = element_text(size = 12, hjust = 0.5, face="bold"))
  p <- p + facet_grid(. ~ title)
  
  p
  
  
  
}


########################################################################################
## PLOTTING R(t), r(t), Alpha(t), Kappa(t), Delta(t)
########################################################################################
plot.param.t <- function(ABC_out=ABC_out, endDatePlot=endDatePlot){
  
  ABC.par <- ABC_out$param
  
  #################################
  ## Confidence intervals
  posterior.CI <- function(posterior.var, round.by=4){
    median = quantile(posterior.var, c(.5), na.rm=TRUE)
    low_95 = quantile(posterior.var, c(.025), na.rm=TRUE)
    low_50 = quantile(posterior.var, c(.25), na.rm=TRUE)
    mean = mean(posterior.var)
    up_50 = quantile(posterior.var, c(.75), na.rm=TRUE)
    up_95 = quantile(posterior.var, c(.975), na.rm=TRUE)
    posterior.CI <- as.data.frame(cbind(low_95,low_50,median,up_50,up_95))
    posterior.CI <- round(posterior.CI, digits=round.by)
    return(posterior.CI)
  }
  
  #################################
  ## Put fn(t) in format for plots
  format.4.plot <- function(fn_t, fn_y_chr, fn.posterior.CI, fn.name){
    fn_y <- as.data.frame(matrix(nrow=length(fn_t), ncol=ncol(fn.posterior.CI) ))
    for (i in 1:length(fn_t)){
      fn_y[i,] = get(fn_y_chr[i])
    }
    colnames(fn_y) <- colnames(fn.posterior.CI)
    
    fn_plot <- as.vector(fn_y)
    rownames(fn_plot) <- 1:length(fn_t)
    fn_plot$date <- fn_t
    fn_plot$state.name <- rep(fn.name, length(fn_t))
    
    return(fn_plot)
  }
  
  #################################
  # GET VARIABLES AND APPLY CI
  ABC.par.CI <- apply(ABC_out$param, MARGIN=2, FUN=posterior.CI)
  
  #############################################
  ## Alpha(t) Kappa(t) Delta(t)
  
  Alpha1.CI <- ABC.par.CI[[6]]
  Alpha2.CI <- ABC.par.CI[[11]]
  Kappa1.CI <- ABC.par.CI[[7]]
  Kappa2.CI <- ABC.par.CI[[12]]
  Delta1.CI <- ABC.par.CI[[5]]
  Delta2.CI <- ABC.par.CI[[10]]
  
  # GET ORDER OF VALUES
  start_time = round(mean(ABC.par[,3]))
  alpha_t_readin_path <- path(data.dir, "alpha_t_readin.csv")
  alpha_t_readin = as.data.frame(read.csv(alpha_t_readin_path, sep=",",stringsAsFactors = FALSE))
  Alpha_t_dates <- as.Date(alpha_t_readin$Alpha_t)
  Alpha_t_dates[1] <- Alpha_t_dates[1]-start_time
  Alpha.t <- Alpha_t_dates
  Alpha.t[length(Alpha.t)] <- Sys.Date()
  
  # ALPHA
  Alpha.chr <- alpha_t_readin$Alpha_y
  assign("Alpha1",Alpha1.CI)
  assign("Alpha2", Alpha2.CI)
  Alpha_plot <- format.4.plot(fn_t = Alpha.t, fn_y_chr = Alpha.chr, fn.posterior.CI=Alpha1.CI, fn.name="Alpha_t" )
  
  # KAPPA
  Kappa.chr <- alpha_t_readin$Kappa_y
  assign("Kappa1",Kappa1.CI)
  assign("Kappa2", Kappa2.CI)
  Kappa_plot <- format.4.plot(fn_t = Alpha.t, fn_y_chr = Kappa.chr, fn.posterior.CI=Kappa1.CI, fn.name="Kappa_t" )
  
  # DELTA
  Delta.chr <- alpha_t_readin$Delta_y
  assign("Delta1",Delta1.CI)
  assign("Delta2", Delta2.CI)
  Delta_plot <- format.4.plot(fn_t = Alpha.t, fn_y_chr = Delta.chr, fn.posterior.CI=Delta1.CI, fn.name="Delta_t" )
  
  # PLOTTING AKD
  traj.CI <- rbind(Alpha_plot, Kappa_plot, Delta_plot)
  vars.to.plot <- c("Alpha_t","Kappa_t","Delta_t")
  data.in <- NULL
  y.max.in <- .8
  y.lab.in <- "Probability"
  chart.title <- "Population-Average Probabilities of Severe Illness"
  AKD_t_plot <- plot.together(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)
  #AKD_t_plot
  
  #############################################
  ## R(t)
  
  # GET ORDER OF VALUES
  out_R0 <- ABC.par[,1]
  out_R0redux1<- ABC.par[,4]
  out_R0redux2<- ABC.par[,9]
  R0_x_redux1 <- out_R0*out_R0redux1
  R0_x_redux2 <- out_R0*out_R0redux2
  R0_x_redux3 <- R0_x_redux1
  
  # GET QUANTILES FOR VARIABLE
  R0.CI <- posterior.CI(out_R0,4)
  R0.redux1.CI <- posterior.CI(R0_x_redux1,4)
  R0.redux2.CI <- posterior.CI(R0_x_redux2,4)
  R0.redux3.CI <- posterior.CI(R0_x_redux3,4)
  
  # GET ORDER OF VALUES
  fn_t_readin_path <- path(data.dir, "fn_t_readin.csv")
  fn_t_readin = as.data.frame(read.csv(fn_t_readin_path, sep=",",stringsAsFactors = FALSE))
  Beta_t_dates <- as.Date(fn_t_readin$Beta_t)
  Beta_t_dates[1] <- Beta_t_dates[1]-start_time
  Rt.t <- Beta_t_dates
  Rt.t[length(Rt.t)] <- Sys.Date()
  
  Rt.chr <- fn_t_readin$Beta_y
  assign("mu.0",R0.CI)
  assign("mu.1", R0.redux1.CI)
  assign("mu.2", R0.redux2.CI)
  assign("mu.3",R0.redux2.CI)
  
  # PUT IN FORMAT FOR PLOTTING
  Rt_plot <- format.4.plot(fn_t = Rt.t, fn_y_chr = Rt.chr, fn.posterior.CI=R0.CI, fn.name="Rt" )
  
  # PLOTTING R(t)
  traj.CI <- Rt_plot
  vars.to.plot <- "Rt"
  data.in <- NULL
  y.max.in <- 4
  y.lab.in <- "R(t)"
  chart.title <- "Time-varying Reproductive Number R(t)"
  R_t_plot <- plot.together(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)
  #R_t_plot
  
  
  #############################################
  ## r(t)
  
  # GET ORDER OF VALUES
  r1.CI <- ABC.par.CI[[2]]
  r2.CI <- ABC.par.CI[[13]]
  
  # GET ORDER OF VALUES
  fn_t_readin_path <- path(data.dir, "fn_t_readin.csv")
  fn_t_readin = as.data.frame(read.csv(fn_t_readin_path, sep=",",stringsAsFactors = FALSE))
  r_t_dates <- as.Date(fn_t_readin$r_t)
  r_t_dates[1] <- r_t_dates[1]
  r_t_dates <- na.omit(r_t_dates)
  r.t <- r_t_dates
  r.t[length(r.t)] <- Sys.Date()
  
  r.chr <- fn_t_readin$r_y
  assign("r1",r1.CI)
  assign("r2", r2.CI)
  
  # PUT IN FORMAT FOR PLOTTING
  r_plot <- format.4.plot(fn_t = r.t, fn_y_chr = r.chr, fn.posterior.CI=r1.CI, fn.name="r_t" )
  
  # PLOTTING r(t)
  traj.CI <- r_plot
  vars.to.plot <- "r_t"
  data.in <- NULL
  y.max.in <- 1
  y.lab.in <- "r(t)"
  chart.title <- "Fraction of observed infections r(t)"
  r_t_plot <- plot.together(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)
  #r_t_plot
  
  
  #############################################
  ## PUT TOGETHER
  #AKD_t_plot + R_t_plot + r_t_plot
  
  plot.out <- vector(mode="list", length=3)
  plot.out[[1]] <- R_t_plot
  plot.out[[2]] <- r_t_plot
  plot.out[[3]] <- AKD_t_plot
  
  return(plot.out)
  
}


########################################################################################
## PLOTTING CFR(t), IFR(t)
########################################################################################
CFR.IFR.plots <- function(traj.CI, date.in){
  
  CFR.posterior.CI <- get.CFR.IFR.by.date(traj.CI=traj.CI, CFR.or.IFR = "CFR", date.in=date.in)
  IFR.posterior.CI <- get.CFR.IFR.by.date(traj.CI=traj.CI, CFR.or.IFR = "IFR", date.in=date.in)
  
  #############################################
  ## CFR(t)
  
  CFR_plot <- CFR.posterior.CI
  CFR_plot$state.name <- rep("CFR", by=nrow(CFR_plot))
  
  # PLOTTING CFR and IFR
  traj.CI <- CFR_plot
  vars.to.plot <- "CFR"
  data.in <- NULL
  y.max.in <- round(max(CFR_plot$up_95) + 0.01, 2)
  y.lab.in <- "CFR(t)"
  chart.title <- "Time-varying Case Fatality Rate CFR(t)"
  CFR_t_plot <- plot.together(traj.CI=traj.CI, data.in=data.in, endDatePlot = endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)
  
  #############################################
  ## IFR(t)
  
  IFR_plot <- IFR.posterior.CI
  IFR_plot$state.name <- rep("IFR", by=nrow(IFR_plot))
  
  # PLOTTING CFR and IFR
  traj.CI <- IFR_plot
  vars.to.plot <- "IFR"
  data.in <- NULL
  y.max.in <- round(max(IFR_plot$up_95) + 0.001, 3)
  y.lab.in <- "IFR(t)"
  chart.title <- "Time-varying Infection Fatality Rate IFR(t)"
  IFR_t_plot <- plot.together(traj.CI=traj.CI, data.in=data.in, endDatePlot = endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)
  
  plot.out <- vector(mode="list", length=2)
  plot.out[[1]] <- CFR_t_plot
  plot.out[[2]] <- IFR_t_plot
  
  return(plot.out)
  
}


########################################################################################
## PLOTTING COMPARTMENTAL VARIABLES
########################################################################################

plot.compartments <- function(traj.CI.in){
  
  vars.to.plot <- c("I","Itot")
  chart.title <- "Current Observed and Estimated Total Illnesses"
  traj.CI <- traj.CI.in
  y.max.in <- 200000
  y.lab.in <- "Number infected"
  curr.illness.variables <- plot.together(traj.CI=traj.CI, data.in=NULL, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)
  curr.illness.variables
  
  
  vars.to.plot <- c("Htot","Q", "V","D_new")
  traj.CI <- traj.CI.in
  y.max.in <- 4000
  y.lab.in <- "Number in compartment"
  chart.title <- "Healthcare Variables"
  curr.healthcare.variables <- plot.together(traj.CI=traj.CI, data.in=NULL, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)
  
  plot.out <- vector(mode="list", length=2)
  plot.out[[1]] <- curr.illness.variables
  plot.out[[2]] <- curr.healthcare.variables
  
  return(plot.out)
}







