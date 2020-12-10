########################################################################################
## PLOTTING R(t), r(t), Alpha(t), Kappa(t), Delta(t)
########################################################################################

plot.scenarios.in <- plot.SCENARIOS.input(ABC_out=ABC_out, endDatePlot=endDatePlot)
plot.scenarios.in[[1]]
plot.scenarios.in[[2]]

plot.SCENARIOS.input <- function(ABC_out=ABC_out, endDatePlot=endDatePlot){
  
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
  
  ## Read in csv files with Beta_t
  start_time = 45
  fn_t_readin_path <- path(data.dir, "fn_t_readin.csv")
  fn_t_readin = as.data.frame(read.csv(fn_t_readin_path, sep=",",stringsAsFactors = FALSE))
  
  ## Get Beta_t
  Beta_t_dates <- as.Date(fn_t_readin$Beta_t)
  Beta_t_dates[1] <- Beta_t_dates[1]-start_time
  Beta_t <- round(as.numeric(Beta_t_dates - as.Date("2020-03-01")) + start_time)
  Rt.t <- Beta_t_dates
  Rt.t[length(Rt.t)] <- endDatePlot
  
  Rt_scenarios_plot <- vector(mode="list", length=3)
  
  for (NPI.idx in 1:3){
    NPI.scenario = NPI.idx
    NPI.name <- NPI.scenarios.mat[NPI.idx, "NPI.name"]
    Beta_y_vals <- NPI.scenarios.mat[NPI.idx, "Beta_y_vals"]
    
    Beta_y_vals <- as.character(NPI.scenarios.mat[NPI.scenario,"Beta_y_vals"])
    
    Rt.chr <- fn_t_readin[,Beta_y_vals]
    assign("mu.0",R0.CI)
    assign("mu.1", R0.redux1.CI)
    assign("mu.2", R0.redux2.CI)
    
    # PUT IN FORMAT FOR PLOTTING
    Rt_plot <- format.4.plot(fn_t = Rt.t, fn_y_chr = Rt.chr, fn.posterior.CI=R0.CI, fn.name=NPI.name )
    
    Rt_scenarios_plot[[NPI.idx]] <- Rt_plot
  }
  Rt_scenarios_plot <- do.call(rbind, Rt_scenarios_plot)
  
  # PLOTTING R(t) SCENARIOS
  traj.CI <- Rt_scenarios_plot
  vars.to.plot <- c("NPI.None","NPI.Obs","NPI.Mod")
  data.in <- NULL
  y.max.in <- 4
  y.lab.in <- "R(t)"
  chart.title <- "R(t): Scenarios"
  y.lab.in <- "R(t)"
  plot.capacity <- NULL
  plot.annotations <- TRUE
  R_t_scenarios_plot <- plot.together.capacity(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title, plot.capacity=plot.capacity, plot.annotations=plot.annotations)
  #R_t_scenarios_plot   
    
  
  
  
  #############################################
  #############################################
  ## Alpha(t) Kappa(t) Delta(t)
  
  #################################
  # GET VARIABLES AND APPLY CI
  ABC.par.CI <- apply(ABC_out$param, MARGIN=2, FUN=posterior.CI)
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
  
  AKD_SCENARIO_plot <- vector(mode="list", length=3)
  
  for (protect.idx in 1:3){
    
    protect.scenario = protect.idx
    protect.name <- protect.scenarios.mat[protect.idx, "protect.name"]
    
    Alpha1 <- weighted.avg.scenarios[protect.scenario,"Alpha.t1"]
    Alpha2 <- weighted.avg.scenarios[protect.scenario,"Alpha.t2"]
    
    Kappa1 <- weighted.avg.scenarios[protect.scenario,"Kappa.t1"]
    Kappa2 <- weighted.avg.scenarios[protect.scenario,"Kappa.t2"]
    
    Delta1 <- weighted.avg.scenarios[protect.scenario,"Delta.t1"]
    Delta2 <- weighted.avg.scenarios[protect.scenario,"Delta.t2"]
    
    ## Alpha_t
    Alpha_y_chr <- alpha_t_readin$Alpha_y
    assign("Alpha1",Alpha1)
    assign("Alpha2", Alpha2)
    Alpha_SCENARIO_plot <- format.4.plot(fn_t = Alpha.t, fn_y_chr = Alpha_y_chr, fn.posterior.CI=Alpha1.CI, fn.name= paste0("Alpha.",protect.name ))
    
    ## Kappa_t
    Kappa_y_chr <- alpha_t_readin$Kappa_y
    assign("Kappa1",Kappa1)
    assign("Kappa2", Kappa2)
    Kappa_SCENARIO_plot <- format.4.plot(fn_t = Alpha.t, fn_y_chr = Kappa_y_chr, fn.posterior.CI=Kappa1.CI, fn.name= paste0("Kappa.",protect.name ))
    
    ## Delta_t
    Delta_y_chr <- alpha_t_readin$Delta_y
    assign("Delta1",Delta1)
    assign("Delta2", Delta2)
    Delta_SCENARIO_plot <- format.4.plot(fn_t = Alpha.t, fn_y_chr = Delta_y_chr, fn.posterior.CI=Delta1.CI, fn.name= paste0("Delta.",protect.name ))
    
    AKD_SCENARIO_plot[[protect.idx]] <- rbind(Alpha_SCENARIO_plot, Kappa_SCENARIO_plot, Delta_SCENARIO_plot)
  }
  
  AKD_SCENARIO_plot <- do.call(rbind,AKD_SCENARIO_plot)
  
  # PLOTTING AKD
  traj.CI <- AKD_SCENARIO_plot
  vars.to.plot <- unique(AKD_SCENARIO_plot$state.name)
  data.in <- NULL
  plot.capacity <- NULL
  plot.annotations <- TRUE
  y.max.in <- 0.8
  y.lab.in <- "Probability"
  chart.title <- "Probabilities of Severe Illness: Scenarios"
  AKD_SCENARIOS_plot <- plot.together.capacity(traj.CI=traj.CI, data.in=data.in, endDatePlot=endDatePlot, vars.to.plot = vars.to.plot, y.lab.in=y.lab.in, y.max.in=y.max.in, chart.title=chart.title, plot.capacity=plot.capacity, plot.annotations=plot.annotations)
  #AKD_SCENARIOS_plot
  
  scenarios.plots.out <- vector(mode="list",length=2)
  scenarios.plots.out[[1]] <- R_t_scenarios_plot
  scenarios.plots.out[[2]] <- AKD_SCENARIOS_plot
  
  return(scenarios.plots.out)
  
}

