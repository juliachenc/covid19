## Prior parameter distributions

#prior.st <- c("unif",44,46)

R0 <- 3.65
prior.R0 <- c("normal",R0,.1)  #.03)

# R0_redux1 <- .21
# R0_redux2 <- .33
# R0_redux3 <- .575 #.6 #65
# R0_redux4 <- .43125
# R0_redux5 <- .299
#
# prior.R0.redux1 <- c("unif", R0_redux1 - 0.05, R0_redux1 + 0.05)
# prior.R0.redux2 <- c("unif", R0_redux2 - 0.05, R0_redux2 + 0.05)
# prior.R0.redux3 <- c("unif", R0_redux3 - 0.05, R0_redux3 + 0.05)
# prior.R0.redux4 <- c("unif", R0_redux4 - 0.05, R0_redux4 + 0.05)
# prior.R0.redux5 <- c("unif", R0_redux5 - 0.05, R0_redux5 + 0.05)


# prior.r1 <- c("unif",0.1, 0.75) #c("unif", 0.13, 0.16)
# prior.r2 <- c("unif",0.1, 0.75) #c("unif",0.12, 0.14) #0.30

prior.r <- c("unif",0.03, 0.75)


## @JULIA FOUND AN ERROR 4.5.21: These had been reduced down to very low values -- I returned them to their previous values
Alpha <- 0.14#.155
Kappa <- 0.6 #.65
Delta <- .56
p_V <- 0.27

stdev <- 0.01

# prior.Delta <- c("unif", Delta - stdev, Delta + stdev)
# prior.Alpha <- c("unif", Alpha - stdev, Alpha + stdev)
# prior.Kappa <- c("unif", Kappa - stdev, Kappa + stdev)
# prior.p_V <- c("unif", p_V - stdev, p_V + stdev)

prior.Delta <- c("normal",Delta, stdev)#.001) #0.01)
prior.Alpha <- c("normal",Alpha, stdev)#.002) #0.003)
prior.Kappa <- c("normal",Kappa, stdev)#.002) # 0.03)
prior.p_V <- c("normal",p_V, stdev) # c("unif", p_V-0.1, p_V+0.5 ) #c("normal",p_V, 0.08)

prior.par <- list(
  prior.R0,
  prior.r,
  # prior.st,
  # prior.R0.redux1,
  prior.Delta,
  prior.Alpha,
  prior.Kappa,
  prior.p_V)

## TIME-VARYING FUNCTIONS ARE READ IN VIA:
## fn_t_readin_path <- path(data.dir, "fn_t_readin.csv")
