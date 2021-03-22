
# Week2
# r0
prior.R0 = c("normal", week_par_mean[1,1], week_par_sd[1,1])
# r
prior.r = c("normal", week_par_mean[1,2], week_par_sd[1,2])
#st
#prior.st = c("unif", week_par_mean[1,3]-1,week_par_mean[1,3]-1)
# R0.redux
prior.R0.redux = c("unif", week_par_mean[1,3]-0.05,week_par_mean[1,3]+0.05)
# Delta
prior.Delta = c("normal", week_par_mean[1,4], week_par_sd[1,4])
# Alpha 
prior.Alpha = c("normal", week_par_mean[1,5], week_par_sd[1,5])
# Kappa
prior.Kappa = c("normal", week_par_mean[1,6], week_par_sd[1,6])
# p_V
prior.p_V = c("normal", week_par_mean[1,7], week_par_sd[1,7])

prior.par <- list(
  prior.R0,
  prior.r,
  #prior.st,
  prior.R0.redux,
  prior.Delta,
  prior.Alpha,
  prior.Kappa,
  prior.p_V)