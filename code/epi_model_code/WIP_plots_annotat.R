
########################################################################################
## PLOT TOGETHER -- ADDING IN CAPACITY
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
## FUNCTION TO PLOT SCENARIO CI -- example for how to add annotations
########################################################################################

#plot <- plot.SCENARIOS(traj.CI=traj.CI, endDatePlot=endDatePlot, startDatePlot=startDatePlot, vars.to.plot=vars.to.plot, longnames=longnames, filter.scenarios=filter.scenarios)

## ADD SCENARIO ID

plot.SCENARIOS.CAMPUS <- function(traj.CI, endDatePlot, startDatePlot, vars.to.plot, longnames, filter.scenarios) {
  
  #longnames <- c("Inf. high risk", "Inf. medium risk", "Inf. low risk", "Number of Tests")
  names(longnames) <- vars.to.plot #c("I_on","I_off","I_saf","Test")
  
  ## Filter only to variables of interest
  traj.CI <- traj.CI %>%  dplyr::filter(state.name %in% vars.to.plot) 
  
  if (!is.null(filter.scenarios)){
    traj.CI <- traj.CI %>% dplyr::filter(scenario.name %in% levels(traj.CI$scenario.name)[filter.scenarios])
  }
  
  ## Select only more recent dates
  init.date <- as.Date(startDatePlot)
  endDatePlot <- as.Date(endDatePlot) #startDatePlot + time.steps.4plot #- 40  # the constant 40 because the traj are not aligned to start date
  traj.CI <- traj.CI %>% dplyr::filter(date >= startDatePlot) %>% dplyr::filter(date < endDatePlot)
  
  traj.CI.peak <- traj.CI %>% group_by(state.name,scenario.name) %>% 
    summarize(max.median.full = max(median, na.rm = TRUE),
              max.median = round(max(median, na.rm = TRUE)),
              max.median.day = match(max.median.full, median) + as.Date(init.date.data)) %>% 
    ungroup() %>% as.data.frame()
  
  ## PLOTTING
  traj.CI.line <- reshape2::melt(traj.CI[c("date","scenario.name", "state.name", "median")], id.vars = c("date", "scenario.name", "state.name"))
  traj.CI.area <- reshape2::melt(traj.CI[c("date","scenario.name", "state.name", "low_95", "low_50", "up_50", "up_95")], id.vars = c("date", "scenario.name", "state.name"))
  traj.CI.area$type <- sapply(traj.CI.area$variable, function(x) {str_split(x, "_")[[1]][1]})
  traj.CI.area$CI <- sapply(traj.CI.area$variable, function(x) {str_split(x, "_")[[1]][2]})
  traj.CI.area$variable <- NULL
  traj.CI.area <- reshape2::dcast(traj.CI.area, "date+scenario.name+state.name+CI~type")
  
  p <- ggplot(transform(traj.CI.area, state.name = factor(state.name, levels=vars.to.plot)))
  
  #####################
  ### colors and names
  #####################
  
  
  
  ## Colors
  
  # cols.list <- c(
  #   "salmon",
  #   "sandybrown",
  #   "navajowhite3",
  #   "olivedrab4",
  #   "olivedrab2",
  #   "mediumseagreen",
  #   "mediumaquamarine",
  #   "mediumturquoise",
  #   "cyan2",
  #   "lightskyblue",
  #   "steelblue2",
  #   "mediumpurple",
  #   "mediumorchid",
  #   "plum1",
  #   "violetred1",
  #   "deeppink4",
  #   "grey50",
  #   "mediumturquoise",
  #   "lightskyblue",
  #   "violetred1",
  #   "grey50",
  #   "grey50",
  #   "grey50"
  # )
  
  # names(cols.list) <- names(longnames)
  # color.this.var <- as.character(cols.list[vars.to.plot])
  
  # ## CAPACITY DATA FRAME
  # capacity.vals <- as.data.frame(matrix(NA, nrow=length(levels(traj.CI$state.name)), ncol=2))
  # capacity.vals[,1] <- levels(traj.CI$state.name)
  # rownames(capacity.vals) <- levels(traj.CI$state.name)
  # capacity.vals["Q_on",2] <- 750 
  # capacity.vals["Q_off",2] <- 750
  # capacity.vals["Q_saf",2] <-750
  # colnames(capacity.vals) <- c("state.name","capacity")
  
  ## PLOT OPTIONS
  #  p <- p + facet_grid(state.name ~ scenario.id, labeller=labeller(state.name=longnames, scenario.id=longnames.scenarios), scales='free')
  p <- p + facet_grid(state.name ~ scenario.name, labeller=labeller(state.name=longnames), scales='free')
  p <- p + geom_ribbon(data = traj.CI.area, aes_string(x = "date", ymin = "low", ymax = "up", alpha = "CI", fill = "state.name"),show.legend = c(fill=FALSE))
  p <- p + geom_line(data = traj.CI.line, aes_string(x = "date", y = "value", linetype = "variable", colour = "state.name"), size = 1, show.legend = c(colour=FALSE))
  
  p <- p + scale_alpha_manual("Percentile", values = c("95" = 0.20, "50" = 0.50), labels = c("95" = "95th", "50" = "50th"))
  p <- p + scale_linetype("Stats")
  p <- p + guides(linetype = guide_legend(order = 1))
  
  
  # ## ADD CAPACITY
  # capacity.vals <- capacity.vals %>% filter(state.name %in% vars.to.plot)
  # p <- p + geom_hline(data= capacity.vals, aes(yintercept=capacity),linetype = "dashed")
  
  ## ADD MAX POINTER
  #capacity.vals <- capacity.vals %>% filter(state.name %in% vars.to.plot)
  p <- p + geom_hline(data = traj.CI.peak, aes(yintercept = max.median),linetype = "dashed") 
  p <- p +  geom_text(data=traj.CI.peak, aes( as.Date(init.date.data) + 50, 1.2*max.median, label = paste0("Median: ", max.median, " Date: ", max.median.day )   ), size=3)
  p <- p + geom_vline(data = traj.CI.peak, aes(xintercept = max.median.day), linetype = "dashed", colour = "grey20")
  
  ## Finish plot 
  p <- p + theme_bw() + theme(legend.position = "top", legend.box = "horizontal")
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot+2,endDatePlot)), date_breaks = "1 week" , date_labels = "%d-%b-%y")
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_text(size = 10, face = "bold"),
                 strip.text.y = element_text(size=10, face="bold"))
  p <- p + ylab("Numbers in Compartments") + xlab(NULL)
  p <- p + scale_y_continuous(labels = scales::comma)
  p <- p + theme(strip.background = element_rect(colour="black", fill="white", 
                                                 size=1, linetype="solid"))
  p
}


########################################################################################
## PLOTTING MULTIPLE TOGETHER
########################################################################################
## USED IN:
## plot.param.t
## CFR.IFR.plots
plot.together.capacity <- function(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot, y.lab.in, y.max.in, chart.title, plot.capacity, plot.annotations) {
  
  ###########
  ### traj.CI
  ###########
  
  ## Filter only to variable of interest
  traj.CI <- traj.CI %>%  dplyr::filter(state.name %in% vars.to.plot)
  
  ## Select only more recent dates
  init.date <- as.Date("2020-03-01")
  startDatePlot <- init.date #
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
      data.processed <- reshape2::melt(data.date, measure.vars = c(2:ncol(data.date)), variable.name = "state.name")
    }
    
    else {data.processed = NULL}
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
  
  if(!is.null(data.in)){
    p <- p + geom_point(data = data.processed,
                        aes(x = date, y = value,
                            color = state.name),
                        alpha = 0.7,
                        inherit.aes = FALSE)
  }
  
  ##################
  ## ADD CAPACITY
  if (!is.null(plot.capacity)){
    ##################
    ### CREATE CAPACITY DATA FRAME
    capacity.vals <- as.data.frame(matrix(NA, nrow=length(levels(traj.CI$state.name)), ncol=2))
    capacity.vals[,1] <- levels(traj.CI$state.name)
    rownames(capacity.vals) <- levels(traj.CI$state.name)
    capacity.vals["Htot",2] <- 4000 
    capacity.vals["Q",2] <- 2245
    capacity.vals["V",2] <-1000
    colnames(capacity.vals) <- c("state.name","capacity")
    ##################
    ### ADD CAPACITY LINES
    capacity.vals <- capacity.vals %>% filter(state.name %in% vars.to.plot)
    p <- p + geom_hline(data = capacity.vals, aes(yintercept=capacity),linetype = "dashed", colour="azure4")
  }
  
  #################
  ## ADD DATE ANNOTATIONS
  if (!is.null(plot.annotations)){
    ######### Create data frame with annotations
    traj.CI.date <- as.data.frame(matrix(NA, nrow=5, ncol=3))
    colnames(traj.CI.date) <- c("date","date.label","y.place")
    traj.CI.date$date <- c(as.Date("2020-03-19"),as.Date("2020-05-08"),as.Date("2020-06-12"),as.Date("2020-07-01"),as.Date("2020-08-18"))
    traj.CI.date$date.label <- c("Stage I", "Stage II", "Stage III", "Modifications", "School Year")
    traj.CI.date$y.place <- c(1:5)
    ######### Add data frame with annotations
    p <- p + geom_vline(data=traj.CI.date, aes(xintercept=as.Date(date)), linetype="dashed",colour="azure4", size=.35) + 
      # annotate("text", label = traj.CI.date$date.label, x = traj.CI.date$date, y = (y.max.in/2)+(y.max.in/20)*traj.CI.date$y.place, size = 3.5, colour = "black")
      annotate("text", label = traj.CI.date$date.label, x = traj.CI.date$date, y = (y.max.in)-(y.max.in/25)*traj.CI.date$y.place, size = 3.5, colour = "black")
  }
  
  ##################
  ## FINISH PLOT
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
  Alpha.t[length(Alpha.t)] <- endDatePlot-1
  
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
  plot.capacity <- NULL
  plot.annotations <- TRUE
  y.max.in <- 0.8
  y.lab.in <- "Probability"
  chart.title <- "Population-Average Probabilities of Severe Illness"
  AKD_t_plot <- plot.together.capacity(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title, plot.capacity=plot.capacity, plot.annotations=plot.annotations)
  AKD_t_plot
  
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
  Rt.t[length(Rt.t)] <- endDatePlot-1
  
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
  y.lab.in <- "Probability"
  plot.capacity <- NULL
  plot.annotations <- TRUE
  R_t_plot <- plot.together.capacity(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title, plot.capacity=plot.capacity, plot.annotations=plot.annotations)
  R_t_plot
  
  
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
  r.t[length(r.t)] <- endDatePlot-1
  
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
  plot.capacity <- NULL
  plot.annotations <- TRUE
  r_t_plot <- plot.together.capacity(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title, plot.capacity=plot.capacity, plot.annotations=plot.annotations)
  r_t_plot
  
  #r_t_plot <- plot.together(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)
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

plots.out <- plot.param.t(ABC_out=ABC_out, endDatePlot=endDatePlot)
plots.out[[2]]

########################################################################################
## PLOTTING CFR(t), IFR(t)
########################################################################################
CFR.IFR.plots <- function(traj.CI, date.in, endDatePlot){
  
  endDatePlot <- endDatePlot + 1
  
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
  plot.capacity <- NULL
  plot.annotations <- TRUE
  CFR_t_plot <- plot.together.capacity(traj.CI=traj.CI, data.in=data.in, endDatePlot = endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title, plot.capacity=plot.capacity, plot.annotations=plot.annotations)
  
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
  IFR_t_plot <- plot.together.capacity(traj.CI=traj.CI, data.in=data.in, endDatePlot = endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title, plot.capacity=plot.capacity, plot.annotations=plot.annotations)
  
  plot.out <- vector(mode="list", length=2)
  plot.out[[1]] <- CFR_t_plot
  plot.out[[2]] <- IFR_t_plot
  
  return(plot.out)
  
}

CFR.IFR.plots.out <- CFR.IFR.plots(traj.CI=traj.0, date.in=date.in)
CFR.IFR.plots.out[[1]]
CFR.IFR.plots.out[[2]]

########################################################################################
## PLOTTING COMPARTMENTAL VARIABLES TOGETHER
########################################################################################

## HEALTHCARE VARIABLES
traj.CI <- traj.0
data.in <- NULL
y.max.in <- 2500
y.lab.in <- "Current Census in Compartment"
vars.to.plot<-c("V", "D_new","Htot")
endDatePlot <- as.Date("2020-10-15")
chart.title <- "Census in Hospital and New Deaths"
data.in <- latest_data
plot.capacity <- NULL
healthcare_plot <- plot.together.capacity(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title, plot.capacity=plot.capacity, plot.annotations=plot.annotations)
healthcare_plot

## NEW INFECTIONS 
traj.CI <- traj.0
data.in <- NULL
y.max.in <- 200000
y.lab.in <- "Current Infections"
vars.to.plot<-c("Itot","I")
endDatePlot <- as.Date("2020-10-15")
chart.title <- "Current Infections Observed and Unobserved"
data.in <- NULL
plot.capacity <- NULL
infections_plot <- plot.together.capacity(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title, plot.capacity=plot.capacity, plot.annotations=plot.annotations)
infections_plot


plot.out.compartmental <- function(traj.CI, endDatePlot){
  ## HEALTHCARE VARIABLES
  traj.CI <- traj.0
  data.in <- NULL
  y.max.in <- traj.CI %>% filter(state.name=="Htot") %>% select(up_95) %>% max() %>% round(digits=-2) + 500 
  y.lab.in <- "Current Census in Compartment"
  vars.to.plot<-c("V", "D_new","Htot")
  endDatePlot <- as.Date("2020-10-15")
  chart.title <- "Census in Hospital and New Deaths"
  data.in <- latest_data
  plot.capacity <- NULL
  healthcare_plot <- plot.together.capacity(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title, plot.capacity=plot.capacity, plot.annotations=plot.annotations)
  #healthcare_plot
  
  ## NEW INFECTIONS 
  traj.CI <- traj.0
  data.in <- NULL
  y.max.in <- traj.CI %>% filter(state.name=="Itot") %>% select(up_95) %>% max() %>% round(digits=-5) + 10000 
  y.lab.in <- "Current Infections"
  vars.to.plot<-c("Itot","I")
  endDatePlot <- as.Date("2020-10-15")
  chart.title <- "Current Infections Observed and Unobserved"
  data.in <- NULL
  plot.capacity <- NULL
  infections_plot <- plot.together.capacity(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title, plot.capacity=plot.capacity, plot.annotations=plot.annotations)
  #infections_plot
  
  plot.out <- vector(mode="list", length=2)
  plot.out[[1]] <- healthcare_plot
  plot.out[[2]] <- infections_plot
  
  return(plot.out)
  
  
}
  
plot.out.H.I <- plot.out.compartmental(traj.CI = traj.CI, endDatePlot = endDatePlot)
plot.out.H.I[[2]]

