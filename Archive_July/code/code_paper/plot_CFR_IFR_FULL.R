

CFR.IFR.plots <- function(traj.CI, date.in, y.max.CFR, y.max.IFR){

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
  y.max.in <- y.max.CFR
  y.lab.in <- "CFR(t)"
  chart.title <- "Time-varying Case Fatality Rate CFR(t)"
  time.steps.4plot <- 300
  start_time <- 45 # Hard coding for now
  CFR_t_plot <- plot.together(traj.CI=traj.CI, data.in=data.in, date.offset.4plot=start_time, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)

  #############################################
  ## IFR(t)

  IFR_plot <- IFR.posterior.CI
  IFR_plot$state.name <- rep("IFR", by=nrow(IFR_plot))

  # PLOTTING CFR and IFR
  traj.CI <- IFR_plot
  vars.to.plot <- "IFR"
  data.in <- NULL
  y.max.in <- y.max.IFR
  y.lab.in <- "IFR(t)"
  chart.title <- "Time-varying Infection Fatality Rate IFR(t)"
  time.steps.4plot <- 300
  IFR_t_plot <- plot.together(traj.CI=traj.CI, data.in=data.in, date.offset.4plot=start_time, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title)

  plot.out <- vector(mode="list", length=2)
  plot.out[[1]] <- CFR_t_plot
  plot.out[[2]] <- IFR_t_plot

  return(plot.out)

}
