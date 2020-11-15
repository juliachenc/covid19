
########################################################################################
## SUMMARY TABLES: WITH MULTIPLE DATES

summary.table.param.CFR.IFR <- function(traj.CI, ABC_out, date.in, use.mean.select, round.by.in=4) {
  library(data.table)
  ABC_param <- ABC_out$param

  # GET ESTIMATED PARAMETER CI
  ABC.par.CI <- apply(ABC_param, MARGIN=2, FUN=posterior.CI)
  Alpha1.CI <- ABC.par.CI[[6]]
  Alpha2.CI <- ABC.par.CI[[11]]
  Kappa1.CI <- ABC.par.CI[[7]]
  Kappa2.CI <- ABC.par.CI[[12]]
  Delta1.CI <- ABC.par.CI[[5]]
  Delta2.CI <- ABC.par.CI[[10]]
  r1.CI <- ABC.par.CI[[2]]
  r2.CI <- ABC.par.CI[[13]]
  # R(t)
  out_R0 <- ABC_param[,1]
  out_R0redux1<- ABC_param[,4]
  out_R0redux2<- ABC_param[,9]
  R0_x_redux1 <- out_R0*out_R0redux1
  R0_x_redux2 <- out_R0*out_R0redux2
  R0.CI <- ABC.par.CI[[1]]
  R0.redux1.CI <- posterior.CI(R0_x_redux1)
  R0.redux2.CI <- posterior.CI(R0_x_redux2)

  # EPI PARAMETERS TABLE
  r1.posterior.CI <- posterior.CI.FORMAT(r1.CI, use.mean=use.mean.select)
  r2.posterior.CI <- posterior.CI.FORMAT(r2.CI, use.mean=use.mean.select)
  R0.posterior.CI <- posterior.CI.FORMAT(R0.CI, use.mean=use.mean.select )
  R0redux1.posterior.CI <- posterior.CI.FORMAT(R0.redux1.CI, use.mean=use.mean.select)
  R0redux2.posterior.CI <- posterior.CI.FORMAT(R0.redux2.CI, use.mean=use.mean.select)
  posterior.epi.vars <- as.data.frame(rbind(R0.posterior.CI, R0redux1.posterior.CI, R0redux2.posterior.CI, r1.posterior.CI, r2.posterior.CI))
  rownames(posterior.epi.vars) <- c("R0", "R(t) 2020-03-27","R(t) 2020-05-15", "r(t) 2020-04-15","r(t) 2020-08-15")
  if (use.mean.select==1){
    colnames(posterior.epi.vars) <- "mean (95% CI)"
  }
  if (use.mean.select==0){
    colnames(posterior.epi.vars) <- "median (95% CI)"
  }

  # PROBABILITY OF SEVERE ILLNESS TABLE
  probs.table <- as.data.frame(matrix(data=NA, nrow=2, ncol=3))
  rownames(probs.table) <- c("2020-05-01", "2020-06-01")
  colnames(probs.table) <- c("Alpha_t", "Kappa_t", "Delta_t")
  probs.table[1,1] <- posterior.CI.FORMAT(Alpha1.CI,use.mean=use.mean.select)
  probs.table[2,1] <- posterior.CI.FORMAT(Alpha2.CI,use.mean=use.mean.select)
  probs.table[1,2] <- posterior.CI.FORMAT(Kappa1.CI,use.mean=use.mean.select)
  probs.table[2,2] <- posterior.CI.FORMAT(Kappa2.CI,use.mean=use.mean.select)
  probs.table[1,3] <- posterior.CI.FORMAT(Delta1.CI,use.mean=use.mean.select)
  probs.table[2,3] <- posterior.CI.FORMAT(Delta2.CI,use.mean=use.mean.select)
  if (use.mean.select==1){
    colnames(probs.table) <- c(paste0("mean (95% CI)", c(" Alpha_t"," Kappa_t"," Delta_t") ))
  }
  if (use.mean.select==0){
    colnames(probs.table) <- c(paste0("median (95% CI)", c(" Alpha_t"," Kappa_t"," Delta_t") ))
  }

  # CFR IFR TABLE
  CFR.posterior.CI <- get.CFR.IFR.by.date(traj.CI=traj.CI, CFR.or.IFR = "CFR", date.in=date.in, round.by=round.by.in)
  IFR.posterior.CI <- get.CFR.IFR.by.date(traj.CI=traj.CI, CFR.or.IFR = "IFR", date.in=date.in, round.by=round.by.in)

  CFR.format <- var.format(CFR.posterior.CI, use.mean=use.mean.select)
  rownames(CFR.format) <- date.in
  colnames(CFR.format) <- "CFR"
  IFR.format <- var.format(IFR.posterior.CI, use.mean=use.mean.select)
  rownames(IFR.format) <- date.in
  colnames(IFR.format) <- "IFR"
  CFR.IFR.table <- cbind(CFR.format,IFR.format)
  if (use.mean.select==1){
    colnames(CFR.IFR.table) <- c("CFR mean (95% CI)", "IFR mean (95% CI)")
  }
  if (use.mean.select==0){
    colnames(CFR.IFR.table) <- c("CFR median (95% CI)", "IFR median (95% CI)")
  }


  return(list(posterior.epi.vars, probs.table, CFR.IFR.table))

}
