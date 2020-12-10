

########################################################################################
## Get Alpha, Kappa, Delta with populations protected
########################################################################################

weighted.avg.protect.SCENARIOS <- function(X.mat, freq.PREV.q, freq.LAC.obs.age, logit.SEIR.est, psi.mat, percent.to.remove, factor.to.remove){
  
  n.dates <- ncol(freq.LAC.obs.age)
  
  #percent.to.remove <- c(1,0.5) # Remove 100%
  #factor.to.remove <- 4  # Age.65.
  weighted.avg <- vector("list", n.dates)
  weighted.avg.scenarios <- vector("list",length(percent.to.remove))
  weighted.avg.scenarios.overall <- NULL
  
  for (remove in 1:length(percent.to.remove)){
    for (t in 1:n.dates){
      percent.remove <- percent.to.remove[remove]
      time = t
      name.date <- colnames(freq.LAC.obs.age)[t]
      
      freq.I <- freq.ILL(X.mat, freq.PREV.q, freq.LAC.obs.age, time = time)
      
      Pr.H.I.q <- get.Pr.q(model=1, time=time, logit.SEIR.est, X.mat, freq.q.IN = freq.I[[2]],  psi.mat)
      freq.H <- get.population.freq(prob.q.vec.IN = Pr.H.I.q, freq.q.IN = freq.I[[1]], X.mat)
      
      Pr.Q.H.q <- get.Pr.q(model=2, time=time, logit.SEIR.est, X.mat, freq.q.IN = freq.H[[2]],  psi.mat)
      freq.Q <- get.population.freq(prob.q.vec.IN = Pr.Q.H.q, freq.q.IN = freq.H[[1]], X.mat)
      
      Pr.D.Q.q <- get.Pr.q(model=3, time=time, logit.SEIR.est, X.mat, freq.q.IN = freq.Q[[2]],  psi.mat)
      freq.D <- get.population.freq(prob.q.vec.IN = Pr.D.Q.q, freq.q.IN = freq.Q[[1]], X.mat)
      
      ##
      freq.I.filter <- freq.I[[1]]*(1-percent.remove*X.mat[,factor.to.remove])
      weighted.avg.H.filter <- t(Pr.H.I.q) %*% freq.I.filter
      
      freq.H.filter <- (get.population.freq(Pr.H.I.q, freq.I.filter, X.mat))[[1]]
      weighted.avg.Q.filter <- t(Pr.Q.H.q) %*% freq.H.filter
      
      freq.Q.filter <- (get.population.freq(Pr.Q.H.q, freq.H.filter, X.mat))[[1]]
      weighted.avg.D.filter <- t(Pr.D.Q.q) %*% freq.Q.filter
      
      weighted.avg.t <- cbind( weighted.avg.H.filter, weighted.avg.Q.filter, weighted.avg.D.filter )
      colnames(weighted.avg.t) <- c("Alpha.","Kappa.","Delta.")
      colnames(weighted.avg.t) <- paste0( colnames(weighted.avg.t), name.date)
      weighted.avg[[t]] <- weighted.avg.t
      ##
    }
    weighted.avg.scenarios[[remove]] <- do.call(cbind, weighted.avg)
  }
  weighted.avg.scenarios.overall <- do.call(rbind,weighted.avg.scenarios)
  #weighted.avg.scenarios.overall <- as.data.frame(weighted.avg.scenarios)
  rownames(weighted.avg.scenarios.overall) <- paste0("Protect.",percent.to.remove*100)
  
  return(weighted.avg.scenarios.overall)
  
}


########################################################################################
## Plot scenario input policies to run: NPI in [[1]], Protect in [[2]] 
########################################################################################

plot.SCENARIOS.input <- function(ABC_out=ABC_out, NPI.scenarios.mat=NPI.scenarios.mat, protect.scenarios.mat=protect.scenarios.mat, weighted.avg.scenarios=weighted.avg.scenarios, endDatePlot=endDatePlot){
  
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

########################################################################################
## SPECIFYING EPIDEMIC MODEL TO BE SIMULATED LOOPING OVER SCENARIOS
## NOTE: NPI scenarios are called in from data "fn_readin.csv"
## NOTE: Protect scenarios are called in from weighted.avg.protect.mat
########################################################################################

correlated.param.SCENARIOS <- function(ABC.out.mat,iter,time.steps,weighted.avg.scenarios,protect.scenarios.mat, NPI.scenarios.mat) {
  
  ##########################################################################################
  ## Read in csv files with Beta_t, Alpha_t, Kappa_t, Delta_t
  
  start_time = 45
  
  fn_t_readin_path <- path(data.dir, "fn_t_readin.csv")
  fn_t_readin = as.data.frame(read.csv(fn_t_readin_path, sep=",",stringsAsFactors = FALSE))
  
  ## Get r_t
  r_t_dates <- as.Date(fn_t_readin$r_t)
  r_t_dates <- na.omit(r_t_dates)
  r_t_dates[1] <- r_t_dates[1]-start_time
  r_t <- round(as.numeric(r_t_dates - as.Date("2020-03-01")) + start_time)
  
  ## Get Beta_t
  start_time <- 45
  Beta_t_dates <- as.Date(fn_t_readin$Beta_t)
  Beta_t_dates[1] <- Beta_t_dates[1]-start_time
  Beta_t <- round(as.numeric(Beta_t_dates - as.Date("2020-03-01")) + start_time)
  
  ## Get Alpha_t
  alpha_t_readin_path <- path(data.dir, "alpha_t_readin.csv")
  alpha_t_readin = as.data.frame(read.csv(alpha_t_readin_path, sep=",",stringsAsFactors = FALSE))
  
  Alpha_t_dates <- as.Date(alpha_t_readin$Alpha_t)
  Alpha_t_dates[1] <- Alpha_t_dates[1]-start_time
  Alpha_t <- round(as.numeric(Alpha_t_dates - as.Date("2020-03-01")) + start_time)
  Kappa_t <- Alpha_t
  Delta_t <- Alpha_t
  
  ##########################################################################################
  ## INITIALIZE SCENARIOS.out
  SCENARIOS.out <- vector("list", 9)
  scenario.idx <- 1
  
  
  for (NPI.idx in 1:3){
    NPI.scenario = NPI.idx
    NPI.name <- NPI.scenarios.mat[NPI.idx, "NPI.name"]
    Beta_y_vals <- NPI.scenarios.mat[NPI.idx, "Beta_y_vals"]
    
    for (protect.idx in 1:3){
      
      print(paste0("Starting scenario ", scenario.idx))
      
      protect.scenario = protect.idx
      protect.name <- protect.scenarios.mat[protect.idx, "protect.name"]
      
      TEST.out <- vector("list", nrow(ABC.out.mat))
      
      for (idx in 1:nrow(ABC.out.mat)){
        
        R0 <- ABC.out.mat[idx,1]
        r1 <- ABC.out.mat[idx,2]
        start_time <- round(ABC.out.mat[idx,3])
        R0_redux1 <- ABC.out.mat[idx,4]
        Delta1 <- ABC.out.mat[idx,5]
        Alpha1 <- ABC.out.mat[idx,6]
        Kappa1 <- ABC.out.mat[idx,7]
        p_V <- ABC.out.mat[idx,8]
        R0_redux2 <- ABC.out.mat[idx,9]
        Delta2 <- ABC.out.mat[idx,10]
        Alpha2 <- ABC.out.mat[idx,11]
        Kappa2 <- ABC.out.mat[idx,12]
        r2 <- ABC.out.mat[idx,13]
        
        ##########################################################################################
        ## Alpha Kappa Delta t1 and t2 if not using the "Protect.0" scenario
        
        #if (protect.name!="Protect.0"){
          
          Alpha1 <- weighted.avg.scenarios[protect.scenario,"Alpha.t1"]
          Alpha2 <- weighted.avg.scenarios[protect.scenario,"Alpha.t2"]
          
          Kappa1 <- weighted.avg.scenarios[protect.scenario,"Kappa.t1"]
          Kappa2 <- weighted.avg.scenarios[protect.scenario,"Kappa.t2"]
          
          Delta1 <- weighted.avg.scenarios[protect.scenario,"Delta.t1"]
          Delta2 <- weighted.avg.scenarios[protect.scenario,"Delta.t2"]
          
        #}
        
        ################################
        ## r_y
        r_y_chr <- fn_t_readin$r_y
        assign("r1",r1)
        assign("r2", r1)
        
        r_y <- as.vector(length(r_t))
        for (z in 1:length(r_t)){
          r_y[z] = get(r_y_chr[z])
        }
        
        
        ## Alpha_t
        
        Alpha_y_chr <- alpha_t_readin$Alpha_y
        assign("Alpha1",Alpha1)
        assign("Alpha2", Alpha2)
        
        Alpha_y <- as.vector(length(Alpha_t))
        for (z in 1:length(Alpha_t)){
          Alpha_y[z] = get(Alpha_y_chr[z])
        }
        
        ## Kappa_t
        
        Kappa_y_chr <- alpha_t_readin$Kappa_y
        assign("Kappa1",Kappa1)
        assign("Kappa2", Kappa2)
        
        Kappa_y <- as.vector(length(Alpha_t))
        for (z in 1:length(Alpha_t)){
          Kappa_y[z] = get(Kappa_y_chr[z])
        }
        
        ## Delta_t
        
        Delta_y_chr <- alpha_t_readin$Delta_y
        assign("Delta1",Delta1)
        assign("Delta2", Delta2)
        
        Delta_y <- as.vector(length(Alpha_t))
        for (z in 1:length(Alpha_t)){
          Delta_y[z] = get(Delta_y_chr[z])
        }
        
        ##########################################################################################
        ## Beta_y
        
        Beta_y_vals <- as.character(NPI.scenarios.mat[NPI.scenario,"Beta_y_vals"])
        
        mu_y_chr <- fn_t_readin[,Beta_y_vals]
        assign("mu.0",1)
        assign("mu.1", R0_redux1)
        assign("mu.2", R0_redux2)
        
        mu_y <- as.vector(length(Beta_t))
        for (z in 1:length(Beta_t)){
          mu_y[z] = get(mu_y_chr[z])
        }
        R0_y <- R0*mu_y
        
        ## Get Beta_y as a function of R0, R0_redux, r, and Alpha
        
        Br.function <- function(R0.in, r.in, Alpha.in){
          d_IH <- 10   #days between illness onset and hospitalization
          d_IR <- 7    #days between illness onset and recovery (hospitalization not required)
          Br <- R0.in * ( 1 / ( (r.in/ ((Alpha.in/d_IH) + ((1-Alpha.in)/d_IR)))  + (1-r.in)*d_IR ))
          return(Br)
        }
        
        Beta_y<- c(
          Br.function(R0.in<-R0_y[1], r.in<-r1, Alpha.in<-Alpha1) ,
          Br.function(R0.in<-R0_y[2], r.in<-r1, Alpha.in<-Alpha1),
          Br.function(R0.in<-R0_y[3], r.in<-r1, Alpha.in<-Alpha1),
          Br.function(R0.in<-R0_y[4], r.in<-r1, Alpha.in<-Alpha1),
          Br.function(R0.in<-R0_y[5], r.in<-r1, Alpha.in<-Alpha1),
          Br.function(R0.in<-R0_y[6], r.in<-r2, Alpha.in<-Alpha2),
          Br.function(R0.in<-R0_y[7], r.in<-r2, Alpha.in<-Alpha2),
          Br.function(R0.in<-R0_y[8], r.in<-r2, Alpha.in<-Alpha2),
          Br.function(R0.in<-R0_y[9], r.in<-r2, Alpha.in<-Alpha2),
          Br.function(R0.in<-R0_y[10], r.in<-r2, Alpha.in<-Alpha2),
          Br.function(R0.in<-R0_y[11], r.in<-r2, Alpha.in<-Alpha2),
          Br.function(R0.in<-R0_y[12], r.in<-r2, Alpha.in<-Alpha2),
          Br.function(R0.in<-R0_y[13], r.in<-r2, Alpha.in<-Alpha2)
        )
        
        ##########################################################################################
        ## RUN THE MODEL
        
        ## COMPILE 
        x <- seihqdr_generator(Alpha_t=Alpha_t, Alpha_y=Alpha_y, Kappa_t=Kappa_t, Kappa_y=Kappa_y, Delta_t=Delta_t, Delta_y=Delta_y, Beta_t=Beta_t, Beta_y=Beta_y, r_t=r_t, r_y=r_y, S_ini=1e7, E_ini=10, p_QV=p_V)
        
        ## SIMULATE
        TEST<-as.data.frame(plyr::rdply(iter, x$run(0:time.steps),.id="iter"))
        
        ## BIND INCLUDING OFFSETING OBSERVED DATA BY START DATE
        TEST.out[[idx]] <- cbind(data.frame(par.id = idx, date = -start_time+TEST$step), TEST)
        
        ## BIND INCLUDING OFFSETING OBSERVED DATA BY START DATE
        TEST.out[[idx]] <- cbind(data.frame(scenario.id = paste0(scenario.idx, "_", NPI.name, "_", protect.name ) , protect.id = protect.name, NPI.id = NPI.name, par.id = idx, date = -start_time+TEST$step), TEST)
        #TEST.out[[idx]] <- cbind(data.frame(scenario.id = scenario.idx, protect.id = protect.name, NPI.id = NPI.name, par.id = idx, date = -start_time+TEST$step), TEST)
        
      }  # end over idx
      
      ## Add to a dataframe over all idx
      TEST.out <- do.call(rbind, TEST.out)
      
      ## Get CI for scenario
      SCENARIO.CI <- get.CI.SCENARIOS(TEST.out = TEST.out)
      
      ## Put TEST.out dataframe into SCENARIOS.out
      # SCENARIOS.out[[scenario.idx]] <- TEST.out#SCENARIO.CI
      SCENARIOS.out[[scenario.idx]] <- SCENARIO.CI
      
      
      ##
      scenario.idx <- scenario.idx + 1 
      
      rm(TEST.out)
      
    } # end over y NPI.scenario
  } # end over x protect.scenario protect.idx
  
  # Output
  all.scenarios <- do.call(rbind,SCENARIOS.out)
  return(all.scenarios)
}


########################################################################################
## FUNCTION TO GET CI FOR EACH SCENARIO (embedded in correlated.param.SCENARIOS)
########################################################################################

get.CI.SCENARIOS <- function(TEST.out) {
  
  library(data.table)
  init.date.data="2020-03-01"
  
  ### Add CFR and IFR to the list (EXTRA STEP NOW THAT THIS IS BEING USED ALSO FOR summary_table)
  traj <- dplyr::mutate(TEST.out, Itot=I+A, CFRobs=(D/Idetectcum), CFRactual=(D/(Itotcum)) )
  traj <-  dplyr::select(traj,c(1:7,CFRobs,CFRactual,vars.plus.R))
  ###
  
  ## TO SAVE MEMORY
  rm(TEST.out)
  
  print("Starting CI calc")
  
  ### MELTING AND APPLYING SUMMARY STAT FUNCTIONS
  df.traj <- reshape2::melt(traj, measure.vars = c(8:ncol(traj)), variable.name = "state.name")
  df.traj_dt <- as.data.table(df.traj)
  
  traj.CI <- df.traj_dt[, list(
    N=.N,
    mean = mean(value),
    median = quantile(value, c(.5),na.rm=TRUE),
    low_95 = quantile(value, c(.025),na.rm=TRUE),
    up_95 = quantile(value, c(.975),na.rm=TRUE),
    up_50 = quantile(value,.75,na.rm=TRUE),
    low_50 = quantile(value,.25,na.rm=TRUE)),
    by = c("date", "state.name","scenario.id","protect.id","NPI.id")]
  traj.CI <- as.data.frame(traj.CI)
  
  ## TO ALIGN DATES: MODEL
  init.date = init.date.data #"2020-01-03"
  init.date <- as.Date(init.date)
  traj.CI[["date"]] <- traj.CI[["date"]] + init.date
  
  return(traj.CI)
  
}


########################################################################################
## FUNCTION TO GET CI FOR EACH SCENARIO (embedded in correlated.param.SCENARIOS)
########################################################################################

plot.SCENARIOS <- function(traj.CI, data.in, endDatePlot, vars.to.plot, filter.scenarios) {
  
  ## Filter only to variables of interest
  traj.CI <- traj.CI %>%  dplyr::filter(state.name %in% vars.to.plot) 
  
  if (!is.null(filter.scenarios)){
    traj.CI <- traj.CI %>% dplyr::filter(scenario.id %in% levels(traj.CI$scenario.id)[filter.scenarios])
  }
  
  ## Select only more recent dates
  init.date <- as.Date("2020-03-01")
  startDatePlot <- init.date #- date.offset.4plot -1 #15
  endDatePlot <- as.Date(endDatePlot) #startDatePlot + time.steps.4plot #- 40  # the constant 40 because the traj are not aligned to start date
  traj.CI <- traj.CI %>% dplyr::filter(date >= startDatePlot) %>% dplyr::filter(date < endDatePlot)
  
  if(!is.null(data.in)){
    ## ALIGN DATES: DATA
    no_obs <- nrow(data.in)
    step <- 0:(no_obs-1)
    date <- init.date + step
    data.date <- cbind(date,data.in)
    rownames(data.date) <- step
    
    ## Select only more recent dates
    data.date <- data.date %>% dplyr::filter(date > startDatePlot) %>% dplyr::filter(date < endDatePlot)
    data <- reshape2::melt(data.date, measure.vars = c(2:ncol(data.date)), variable.name = "state.name")
  }
  
  ## PLOTTING
  traj.CI.line <- reshape2::melt(traj.CI[c("date","scenario.id",  "protect.id", "NPI.id", "state.name", "median")], id.vars = c("date", "scenario.id",  "protect.id", "NPI.id","state.name"))
  traj.CI.area <- reshape2::melt(traj.CI[c("date","scenario.id",  "protect.id", "NPI.id", "state.name", "low_95", "low_50", "up_50", "up_95")], id.vars = c("date", "scenario.id",  "protect.id", "NPI.id","state.name"))
  traj.CI.area$type <- sapply(traj.CI.area$variable, function(x) {str_split(x, "_")[[1]][1]})
  traj.CI.area$CI <- sapply(traj.CI.area$variable, function(x) {str_split(x, "_")[[1]][2]})
  traj.CI.area$variable <- NULL
  traj.CI.area <- reshape2::dcast(traj.CI.area, "date+scenario.id+state.name+CI~type")
  
  p <- ggplot(transform(traj.CI.area, state.name = factor(state.name, levels=vars.to.plot)))
  
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
    "CFRobs",
    "CFRactual"
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
  
  ## CAPACITY DATA FRAME
  capacity.vals <- as.data.frame(matrix(NA, nrow=length(levels(traj.CI$state.name)), ncol=2))
  capacity.vals[,1] <- levels(traj.CI$state.name)
  rownames(capacity.vals) <- levels(traj.CI$state.name)
  capacity.vals["Htot",2] <- 4000 
  capacity.vals["Q",2] <- 2245
  capacity.vals["V",2] <-1000
  colnames(capacity.vals) <- c("state.name","capacity")
  
  ## PLOT OPTIONS
  #  p <- p + facet_grid(state.name ~ scenario.id, labeller=labeller(state.name=longnames, scenario.id=longnames.scenarios), scales='free')
  p <- p + facet_grid(state.name ~ scenario.id, labeller=labeller(state.name=longnames), scales='free')
  p <- p + geom_ribbon(data = traj.CI.area, aes_string(x = "date", ymin = "low", ymax = "up", alpha = "CI", fill = "state.name"),show.legend = c(fill=FALSE))
  p <- p + geom_line(data = traj.CI.line, aes_string(x = "date", y = "value", linetype = "variable", colour = "state.name"), size = 1, show.legend = c(colour=FALSE))
  
  p <- p + scale_alpha_manual("Percentile", values = c("95" = 0.20, "50" = 0.50), labels = c("95" = "95th", "50" = "50th"))
  p <- p + scale_linetype("Stats")
  p <- p + guides(linetype = guide_legend(order = 1))
  
  
  ## ADD CAPACITY
  capacity.vals <- capacity.vals %>% filter(state.name %in% vars.to.plot)
  p <- p + geom_hline(data= capacity.vals, aes(yintercept=capacity),linetype = "dashed")
  
  ## ADD DATA
  if(!is.null(data.in)){
    p <- p + geom_point(data = data, aes_string(x = "date", y = "value"), size = .5, colour = "black")
  }
  
  p <- p + theme_bw() + theme(legend.position = "top", legend.box = "horizontal")
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "1 month" , date_labels = "%d-%b-%y")
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_text(size = 8, face = "bold"))
  p <- p + ylab("Numbers in Compartments") + xlab(NULL)
  p <- p + scale_y_continuous(labels = scales::comma)
  p <- p + theme(strip.background = element_rect(colour="black", fill="white", 
                                                 size=1, linetype="solid"))
  p
}


