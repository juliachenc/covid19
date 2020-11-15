## Prior parameter distributions

prior.st <- c("unif",44,46)

R0 <- 3.65
prior.R0 <- c("normal",R0,.03)

R0_redux1 <- .21 
R0_redux2 <- .33

prior.R0.redux1 <- c("unif", R0_redux1 - 0.05, R0_redux1 + 0.05)
prior.R0.redux2 <- c("unif", R0_redux2 - 0.05, R0_redux2 + 0.05)

# prior.r1 <- c("unif", 0.1, 0.17)
# prior.r2 <- prior.r1

prior.r1 <- c("unif",0.1, 0.16) #c("unif", 0.13, 0.16)
prior.r2 <- c("unif",0.1, 0.16) #c("unif",0.12, 0.14) #0.30

prior.r1 <- c("unif",0.1, 0.75) #c("unif", 0.13, 0.16)
prior.r2 <- c("unif",0.1, 0.75) #c("unif",0.12, 0.14) #0.30


Alpha1 <- 0.14#.155 
Kappa1 <- 0.6 #.65
Delta1 <- .56

Alpha2 <- 0.05 #.06
Kappa2 <- .58 #.25
Delta2 <- .52 #.75

stdev <- 0.005
prior.Delta1 <- c("normal",Delta1, stdev)#.001) #0.01)
prior.Alpha1 <- c("normal",Alpha1, stdev)#.002) #0.003)
prior.Kappa1 <- c("normal",Kappa1, stdev)#.002) # 0.03)

prior.Delta2 <- c("normal",Delta2, stdev)#.001)
prior.Alpha2 <- c("normal",Alpha2, stdev)#.001)
prior.Kappa2 <- c("normal",Kappa2, stdev)#.002)

p_V <- 0.25 #.28 
prior.p_V <- c("normal",p_V, stdev) # c("unif", p_V-0.1, p_V+0.5 ) #c("normal",p_V, 0.08)

prior.par <- list(
  prior.R0,
  prior.r1,
  prior.st,
  prior.R0.redux1,
  prior.Delta1,
  prior.Alpha1,
  prior.Kappa1,
  prior.p_V,
  prior.R0.redux2,
  prior.Delta2,
  prior.Alpha2,
  prior.Kappa2,
  prior.r2)

## TIME-VARYING FUNCTIONS ARE READ IN VIA:
## fn_t_readin_path <- path(data.dir, "fn_t_readin.csv")
