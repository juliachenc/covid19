
#############################################
## PLOT CODE
## Creates plots for time varying parameters:
## R(t)
## r(t)
## Alpha(t), Kappa(t), Delta(t)

plot.together <- function(traj.CI=traj.CI, data.in=data.in, init.date.data=NULL, date.offset.4plot=NULL, time.steps.4plot, vars.to.plot, y.lab.in, y.max.in, chart.title) {

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
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "2 weeks" , date_labels = "%d-%b-%y")
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


plot.param.t <- function(ABC_out){

  ABC.par <- ABC_out$param

  #################################
  ## Plot fn with CI
  # plot.together <- function(traj.CI=traj.CI, data.in=data.in, init.date.data=NULL, date.offset.4plot=NULL, time.steps.4plot, vars.to.plot, y.lab.in, y.max.in, chart.title) {
  #
  #   if(is.null(init.date.data)) {
  #     init.date.data <- "2020-03-01"}
  #   if(is.null(date.offset.4plot)){
  #     date.offset.4plot=0}
  #
  #   ###########
  #   ### traj.CI
  #   ###########
  #
  #   ## Filter only to variable of interest
  #   traj.CI <- traj.CI %>%  dplyr::filter(state.name %in% vars.to.plot)
  #
  #   ## Select only more recent dates
  #   init.date <- init.date.data
  #   init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
  #   startDatePlot <- init.date - date.offset.4plot -1 #15
  #   endDatePlot <- startDatePlot + time.steps.4plot #- 40  # the constant 40 because the traj are not aligned to start date
  #   traj.CI <- traj.CI %>% dplyr::filter(date >= startDatePlot) %>% dplyr::filter(date < endDatePlot)
  #
  #   ## Add title
  #   traj.CI$title <- chart.title
  #
  #   ###########
  #   ### data.in
  #   ###########
  #
  #   ## Data in -- plot only for the selected variable
  #   if(!is.null(data.in)){
  #
  #     if(any(vars.to.plot %in% colnames(data.in))) {  # FIX LATER -- REMOVE TMP
  #
  #       ## Filter only to variable of interest
  #       vars.to.extract <- vars.to.plot[vars.to.plot %in% colnames(data.in) ]
  #
  #       data.in<- data.in %>% dplyr::select(vars.to.extract)
  #
  #       ## ALIGN DATES: DATA
  #       no_obs <- nrow(data.in)
  #       step <- 0:(no_obs-1)
  #       date <- init.date + step
  #       data.date <- cbind(date,data.in)
  #       rownames(data.date) <- date
  #       #data.date$date <- NULL
  #
  #       ## Select only more recent dates
  #       data.date <- data.date %>% dplyr::filter(date > startDatePlot)
  #       data <- reshape2::melt(data.date, measure.vars = c(2:ncol(data.date)), variable.name = "state.name")
  #     }
  #
  #     else {data = NULL}
  #   }
  #
  #   #####################
  #   ### colors and names
  #   #####################
  #
  #   longnames <- c("Susceptible",
  #                  "New Obs. Infected",
  #                  "Current Obs. Infected",
  #                  "Cum. Obs. Infected",
  #                  "Current Tot. Infected",
  #                  "Cum. Tot. Infected",
  #                  "New in Hospital",
  #                  "Current in Hospital",
  #                  "Cum. in Hospital",
  #                  "Current in ICU",
  #                  "Cum. in ICU",
  #                  "Current Ventilation",
  #                  "Cum. Ventilation",
  #                  "New Deaths",
  #                  "Cum. Deaths",
  #                  "Recovered",
  #                  "R0(t)",
  #                  "Alpha(t)",
  #                  "Kappa(t)",
  #                  "Delta(t)",
  #                  "r(t)",
  #                  "CFR",
  #                  "IFR"
  #   )
  #
  #   names(longnames) <-  c(
  #     "S",
  #     "I_detect_new",
  #     "I",
  #     "Idetectcum",
  #     "Itot",
  #     "Itotcum",
  #     "H_new",
  #     "Htot",
  #     "Htotcum",
  #     "Q",
  #     "Qcum",
  #     "V",
  #     "Vcum",
  #     "D_new",
  #     "D",
  #     "R",
  #     "Rt",
  #     "Alpha_t",
  #     "Kappa_t",
  #     "Delta_t",
  #     "r_t",
  #     "CFR",
  #     "IFR"
  #   )
  #
  #   ## Colors
  #
  #   cols.list <- c(
  #     "salmon",
  #     "sandybrown",
  #     "navajowhite3",
  #     "olivedrab4",
  #     "olivedrab2",
  #     "mediumseagreen",
  #     "mediumaquamarine",
  #     "mediumturquoise",
  #     "cyan2",
  #     "lightskyblue",
  #     "steelblue2",
  #     "mediumpurple",
  #     "mediumorchid",
  #     "plum1",
  #     "violetred1",
  #     "deeppink4",
  #     "grey50",
  #     "mediumturquoise",
  #     "lightskyblue",
  #     "violetred1",
  #     "grey50",
  #     "grey50",
  #     "grey50"
  #   )
  #   names(cols.list) <- names(longnames)
  #   color.this.var <- as.character(cols.list[vars.to.plot])
  #
  #   ##################
  #   ### CREATE PLOT
  #   ##################
  #
  #   p <- ggplot(data = traj.CI,
  #               aes(x = date,
  #                   y = median, ymin = low_95, ymax = up_95,
  #                   color = state.name,
  #                   fill = state.name,
  #                   group = state.name))
  #
  #   p <- p +  geom_ribbon(data = traj.CI,
  #                         aes(x = date,
  #                             y = median, ymin = low_50, ymax = up_50,
  #                             color = state.name,
  #                             fill = state.name,
  #                             group = state.name),alpha = .5, inherit.aes=TRUE, color=FALSE)
  #
  #   p <- p +  scale_fill_manual(values = c(color.this.var),labels = longnames) + scale_color_manual(values = c(color.this.var), labels = longnames)
  #   p <- p + geom_line() + geom_ribbon(alpha = 0.2, color = FALSE)
  #
  #   # if(!is.null(data)){
  #   #   p <- p + geom_point(data = data,
  #   #                       aes(x = date, y = value,
  #   #                           color = state.name),
  #   #                       alpha = 0.7,
  #   #                       inherit.aes = FALSE)
  #   # }
  #
  #   p <- p + theme_bw() + theme(legend.title = element_blank())
  #   p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "2 weeks" , date_labels = "%d-%b-%y")
  #   p <- p + scale_y_continuous(limits = c(0,y.max.in), breaks = seq(from = 0, to = y.max.in, by = y.max.in/10))
  #   p <- p + theme(axis.text.x = element_text(angle = 90),
  #                  strip.text.x = element_text(size = 12, face = "bold"))
  #   #  p <- p + ylab(paste0("Number  ", as.character(longnames[var.to.plot]))) + xlab(NULL)
  #   #p <- p + ylab("Probability") + xlab(NULL)
  #   p <- p + ylab(y.lab.in) + xlab(NULL)
  #   #p <- p + labs(title = title.input)
  #   #p<-p+theme(plot.title = element_text(size = 12, hjust = 0.5, face="bold"))
  #   p <- p + facet_grid(. ~ title)
  #
  #   p
  #
  # }

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
  time.steps.4plot <- 300
  AKD_t_plot <- plot.together(traj.CI=traj.CI, data.in=data.in, date.offset.4plot=start_time, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)
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
  time.steps.4plot <- 300
  R_t_plot <- plot.together(traj.CI=traj.CI, data.in=data.in, date.offset.4plot=start_time, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)
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
  r_t_dates[1] <- r_t_dates[1]-start_time
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
  time.steps.4plot <- 300
  r_t_plot <- plot.together(traj.CI=traj.CI, data.in=data.in, date.offset.4plot=start_time, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)
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

