
if(i == week.no-1){
  prior.R0 = c("normal", week_par_mean[i,1], week_par_sd[i,1])
  # r
  prior.r = c("normal", week_par_mean[i,2], week_par_sd[i,2])
  # Delta
  prior.Delta = c("normal", week_par_mean[i,3], week_par_sd[i,3])
  # Alpha 
  prior.Alpha = c("normal", week_par_mean[i,4], week_par_sd[i,4])
  # Kappa
  prior.Kappa = c("normal", week_par_mean[i,5], week_par_sd[i,5])
  # p_V
  prior.p_V = c("normal", week_par_mean[i,6], week_par_sd[i,6]) 
  }

# print(paste0("i=  ", i))
# print(paste0("prior.R0=   ", prior.R0))

prior.par <- list(
  prior.R0,
  prior.r,
  prior.Delta,
  prior.Alpha,
  prior.Kappa,
  prior.p_V)

