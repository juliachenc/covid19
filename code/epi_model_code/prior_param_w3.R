
# Week2
# r0
prior.R0 = c("normal", week_par_mean[2,1], week_par_sd[2,1])
# r
prior.r = c("normal", week_par_mean[2,2], week_par_sd[2,2])
#st
prior.st = c("unif", week_par_mean[2,3]-1,week_par_mean[2,3]-1)
# R0.redux
prior.R0.redux = c("unif", week_par_mean[2,4]-0.05,week_par_mean[2,4]+0.05)
# Delta
prior.Delta = c("normal", week_par_mean[2,5], week_par_sd[2,5])
# Alpha 
prior.Alpha = c("normal", week_par_mean[2,6], week_par_sd[2,6])
# Kappa
prior.Kappa = c("normal", week_par_mean[2,7], week_par_sd[2,7])
# p_V
prior.p_V = c("normal", week_par_mean[2,8], week_par_sd[2,8])

prior.par <- list(
  prior.R0,
  prior.r,
  prior.st,
  prior.R0.redux,
  prior.Delta,
  prior.Alpha,
  prior.Kappa,
  prior.p_V)