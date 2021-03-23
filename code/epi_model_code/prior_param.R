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

prior.r1 <- c("unif",0.03, 0.35)
prior.r2 <- c("unif", 0.3, 0.65) #c("unif",0.1, 0.85)


Alpha <- .05 #.1 #0.14#.155
Kappa <- .05 #.1 #0.6 #.65
Delta <- .05 #.1 #.56
p_V <- .05 #.1 # 0.27 #.28

stdev <- 0.05

prior.Delta <- c("unif", Delta - stdev, Delta + stdev)
prior.Alpha <- c("unif", Alpha - stdev, Alpha + stdev)
prior.Kappa <- c("unif", Kappa - stdev, Kappa + stdev)
prior.p_V <- c("unif", p_V - stdev, p_V + stdev)

# prior.Delta1 <- c("normal",Delta1, stdev)#.001) #0.01)
# prior.Alpha1 <- c("normal",Alpha1, stdev)#.002) #0.003)
# prior.Kappa1 <- c("normal",Kappa1, stdev)#.002) # 0.03)
# prior.p_V <- c("normal",p_V, stdev) # c("unif", p_V-0.1, p_V+0.5 ) #c("normal",p_V, 0.08)

# Alpha2 <- 0.05 #.06
# Kappa2 <- .55 #.25
# Delta2 <- .52 #.75
#
# # (julia) add Delta3
# Delta3 <- .56
# prior.Delta3 <- c("normal",Delta3, 0.7)
# prior.Delta2 <- c("normal",Delta2, stdev)#.001)
# prior.Alpha2 <- c("normal",Alpha2, stdev)#.001)
# prior.Kappa2 <- c("normal",Kappa2, stdev)#.002)

prior.par <- list(
  prior.R0,
  prior.r1,
  # prior.st,
  # prior.R0.redux1,
  prior.Delta,
  prior.Alpha,
  prior.Kappa,
  prior.p_V)

## TIME-VARYING FUNCTIONS ARE READ IN VIA:
## fn_t_readin_path <- path(data.dir, "fn_t_readin.csv")
