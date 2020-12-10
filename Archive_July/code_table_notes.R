
# The model estimated $Pr(Death|Illness,Race_i)$ is calculated based on the prevalence of the risk factors in each race/ethnicity group in L.A. County. The observed $Pr(Death|Illness,Race_i)$ is calculated as the number of deaths over the number of illnesses for each race in the official L.A. County statistics.
#
# Complex dynamics lead to exposure to COVID-19 and resulting illness. We have assumed a random-mixed model of transmission dynamics in L.A. and so do not account for these dynamics in our model. Because the probability $Pr(Death|Illness,Race_i)$ is by definition conditional upon an individual already having contracted COVID-19 illness, it does not depend on these factors leading to exposure to the virus. We illustrate here the Relative Risk of $Pr(Death | Illness)$ for each race/ethnicity group to that for whites in order to make clear the difference in expected vs. observed mortality rate between race/ethnicity groups given the same level of illness or exposure pattern. Some insights this analysis by RR provides:
#
# * The observed $RR_{White}(Death|Illness)$ is slightly lower for the Latinx population than calculated by our model, meaning slightly fewer deaths than expected based on the prevalence of risk factors alone.
# * The observed $RR_{White}(Death|Illness)$ is slightly higher for the black population than calculated by our model, meaning slightly more deaths than expected based on the prevalence of risk factors alone.
# * The observed $RR_{White}(Death|Illness)$ is higher for the Asian population than calculated by our model, meaning more deaths (by around 33%) than expected based on the prevalence of risk factors alone.
#
# Lastly, it is important to note that the observed distribution of deaths by race for L.A. County includes cases from nursing homes (SNFs), whereas the model projections are based on the general population. Because SNF deaths represent a significant portion of overall deaths, removing SNF numbers from the observed distribution may lead to a different observed $RR_{White}(Death|Illness)$ than that shown in the table below.


|       | Description                                                       | Value                                   |
  |-------|-------------------------------------------------------------------|-----------------------------------------|
  | R0    | Basic reproductive number                                         | Estimated                               |
  | Alpha | probability infected (I) requires  hospitalization (vs. recovers) | Estimated                               |
  | Kappa | probability hospitalized (H)  requires ICU (vs. recovers)         | Estimated                               |
  | Delta | probability ICU (Q) patient dies                                  | Estimated                               |
  | p_V   | probability ventilation (V) required  given ICU                   | Estimated                               |
  | beta  | transmission rate                                                 |  Analytically derived from model and R0 |
  | d_EI  | days between exposure and infectivity  (incubation period)        | 5 days                                  |
  | d_IH  | days between symptom onset and  hospitalization (if required)     | 10 days                                 |
  | d_IR  | days between symptom onset and  recovery (if not hospitalized)    | 7 days                                  |
  | d_HQ  | days between hospitalization and  ICU (if required)               | 1 day                                   |
  | d_QR  | days between hospitalization and  recovery (if ICU not required)  | 12 days                                 |
  | d_QD  | days between ICU and fatality                                     | 8 days                                  |
  | d_QR  | days between ICU and recovery                                     | 7 days                                  |
  | Alpha | probability infected (I) requires  hospitalization (vs. recovers) | Estimated                               |
  | N     | Total population size                                             |                                         |
  | S     | Susceptible population                                            |                                         |
  | E     | Exposed not yet infectious                                        |                                         |
  | A     | Infected, unobserved                                              |                                         |
  | I     | Infected, observed                                                |                                         |
  | H     | In Hospital                                                       |                                         |
  | Q     | In ICU                                                            |                                         |
  | V     | On ventilator                                                     |                                         |
  | D     | Dead                                                              |                                         |
  | R     | Recovered/removed                                                 |                                         |





risk_table <- as.datatable(formattable(data_subset, align="r",
                                       align = c(rep("c", 9)),
                                       list(
                                         `age`= color_tile('yellow', 'darkorange'),
                                         `BMI`= color_tile('lightblue', 'pink'),
                                         `smoking`= color_tile('grey', 'transparent'),
                                         `comorbidity`= color_tile('darkgrey', 'transparent'),
                                         `riskprofile`= color_tile('red', 'green'),
                                         `LA County`= color_bar('cornflowerblue', fun = unit.scale),
                                         `Pr.H`= color_bar('violet', fun = unit.scale),
                                         `Pr.Q`= color_bar('violet', fun = unit.scale),
                                         `Pr.D`= color_bar('violet', fun = unit.scale)
                                       )), rownames = FALSE, colnames = c('Population Prevalence in L.A.County' = 'LA County', 'Age' = 'age', 'BMI status' = 'BMI', 'Smoking status' = 'smoking', 'Existing comorbidities' = 'comorbidity', 'Risk Group' = 'riskprofile', 'Pr(Hospital|Illness)' = 'Pr.H', 'Pr(ICU|Hospital)' = 'Pr.Q', 'Pr(Death|ICU)' = 'Pr.D' ),
                           #filter = 'bottom',
                           options = list(pageLength = 10,
                                          autoWidth = TRUE)
                           # order = list(list(9, 'desc'),list(8,'desc'),list(7,'desc')))
                           #order = list(list(6, 'desc'), list(7, 'desc'), list(8, 'desc')))
                           #order(data2$"LA County",decreasing=TRUE))
)


prevalence[, "Improvement"] = prevalence[, "Improvement"] / 100
formattable(prevalence,
            align = c("l",rep("r", NCOL(prevalence) - 1)),
            list(`Indicator Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                 `Average` = color_bar("#FA614B"),
                 `Improvement` = percent))

summary_table2 <- t(summary_table2)
summary_table2 <- as.data.frame(summary_table2)

as.datatable(formattable(summary_table2,
            align = rep("c", ncol(summary_table2)),
            list(`Case Fatality Rate: Observed` = percent,
                 `Case Fatality Rate: True, Estimated` = percent,
                 `r, fraction obs. illnesses` = percent,
                 `R0 fraction reduction` = percent,
                 `Pr(Hospital|Illness)` = percent,
                 `Pr(ICU|Hospital)` = percent,
                 `Pr(Death|ICU)` = percent)
            )
)

summary_table2 <- t(summary_table2)
formattable(summary_table2,
            list(`Case Fatality Rate: Observed` = percent,
                 `Case Fatality Rate: True, Estimated` = percent,
                 `r, fraction obs. illnesses` = percent,
                 `R0 fraction reduction` = percent,
                 `Pr(Hospital|Illness)` = percent,
                 `Pr(ICU|Hospital)` = percent)
)






rownames(table.ex)= c("A","B","C","D","E","F","G","H","I","J")

formattable(table.ex,
            list(`V1` = percent))

            )











########################################################################################
########################################################################################
########################################################################################

```{r}

############################################################################################################
############################################################################################################
### CFR model with estimated SNF removed
############################################################################################################
############################################################################################################



summary.table <- function(ABC.out.mat, par.vec.length, iter, time.steps, init.date.data="2020-03-01") {

  library(data.table)

  D.percent.races <- as.data.frame(race_model_D)

  D.obs.white.percent <- race_model_D$White

  ## MODEL OUTPUT TO PLOT

  par.vec.length <- 100
  iter = 30
  time.steps = 500

  TEST.out <- correlated.param.SIM(ABC.out.mat[1:par.vec.length,],iter=iter,time.steps=time.steps, startObservedData = 0, scenario=4, intervention_date="2020-03-12",sd.redux=NULL)




  traj <- dplyr::mutate(TEST.out, CFRobs=(D/Idetectcum), CFRactual=(D/(Itotcum)))
  traj <-  dplyr::select(traj,c(1:4,CFRobs,CFRactual))

  ## TO SAVE MEMORY
  rm(TEST.out)

  print("Starting CI calc")

  ### MELTING AND APPLYING SUMMARY STAT FUNCTIONS
  df.traj <- reshape2::melt(traj, measure.vars = c(5:ncol(traj)), variable.name = "state.name")

  library(data.table)
  df.traj_dt <- as.data.table(df.traj)

  traj.CI <- df.traj_dt[, list(
    N=.N,
    median = quantile(value, c(.5), na.rm=TRUE),
    low_95 = quantile(value, c(.025), na.rm=TRUE),
    #low_50 = quantile(value,.25, na.rm=TRUE),
    #up_50 = quantile(value,.75, na.rm=TRUE),
    up_95 = quantile(value, c(.975), na.rm=TRUE)),
    by = c("date", "state.name")]
  traj.CI <- as.data.frame(traj.CI)

  ## TO ALIGN DATES: MODEL
  init.date = init.date.data #"2020-01-03"
  init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
  traj.CI[["date"]] <- traj.CI[["date"]] + init.date

  ## GET TODAY'S DATE
  CFR.today<- traj.CI %>% filter(date==Sys.Date()) %>% select(-c(date,N)) %>% mutate_if(is.numeric, round, digits=4)
  #rownames(CFR.today) <- c("Case Fatality Rate: Observed", "Case Fatality Rate: True, Estimated")
  rownames(CFR.today) <- c("Case Fatality Rate", "Infection Fatality Rate")
  CFR.today <- select(CFR.today, -c(1))

  # GET QUANTILES FOR VARIABLE

  posterior.CI <- function(posterior.var){
    median = quantile(posterior.var, c(.5), na.rm=TRUE)
    low_95 = quantile(posterior.var, c(.025), na.rm=TRUE)
    #mean = mean(posterior.var)
    up_95 = quantile(posterior.var, c(.975), na.rm=TRUE)
    posterior.CI <- as.data.frame(cbind(low_95,median,up_95))
    return(posterior.CI)
  }

  R0.posterior.CI <- round(posterior.CI(ABC_out$param[,1]),digits=2)
  r.posterior.CI <- posterior.CI(ABC_out$param[,2])
  R0redux.posterior.CI <- posterior.CI(ABC_out$param[,4])
  Alpha.posterior.CI <- posterior.CI(ABC_out$param[,6])
  Kappa.posterior.CI <- posterior.CI(ABC_out$param[,7])
  Delta.posterior.CI <- posterior.CI(ABC_out$param[,5])
  #Pr.D.given.I.CI <- posterior.CI(ABC_out$param[,6]*ABC_out$param[,7]*ABC_out$param[,5])

  #posterior.vars <- rbind(r.posterior.CI, R0.posterior.CI, R0redux.posterior.CI,Alpha.posterior.CI,Kappa.posterior.CI,Delta.posterior.CI,Pr.D.given.I.CI) %>% mutate_if(is.numeric, round, digits=4)
  posterior.vars <- rbind(r.posterior.CI, R0.posterior.CI, R0redux.posterior.CI,Alpha.posterior.CI,Kappa.posterior.CI,Delta.posterior.CI) %>% mutate_if(is.numeric, round, digits=4)
  rownames(posterior.vars) <- c("r, fraction obs. illnesses", "R0", "R0 fraction reduction", "Pr(Hospital|Illness)", "Pr(ICU|Hospital)", "Pr(Death|ICU)")

  all.posterior.vars <- rbind(CFR.today, posterior.vars)
  colnames(all.posterior.vars) <- c("Median", "Lower 95% CI", "Upper 95% CI")

  return(all.posterior.vars)

}

summary_table <- summary.table(ABC.out.mat=ABC.out.mat, par.vec.length=par.vec.length, iter=iter, time.steps=time.steps, init.date.data="2020-03-01")


```
########################################################################################
########################################################################################
########################################################################################



```{r}
############################################################################################################
############################################################################################################
### CFR *observed* with and without estimated SNF removed
############################################################################################################
############################################################################################################


### I by race with SNF removed
#race_obs_I_recent.nominal.REMOVED

### D by race with SNF removed
#race_observed_D_recent.nominal.REMOVED

### Current counts with SNF removed
I_current <- last(cum_counts_NH_update$Idetectcum)
D_current <- last(cum_counts_NH_update$D)

############################################################################################################
### CFR *observed* WITHOUT SNF removed
############################################################################################################

### Get the estimated nominal number of I by race, current
race_obs_I_recent.frac <- apply(race_obs_I_recent.nominal, 2, function(x) round(x/sum(x),4))
race_obs_I_est.final <- round(I_current * race_obs_I_recent.frac)

race_obs_D_recent.frac <- apply(as.data.frame(race_observed_D_recent.nominal), 2, function(x) round(x/sum(x),4))
race_obs_D_est.final <- round(D_current * race_obs_D_recent.frac)

### CFR observed without estimated SNF removed
CFR_obs_WITHOUT_SNF_removed <- round( race_obs_D_est.final / race_obs_I_est.final , 4)

############################################################################################################
### CFR *observed* WITH SNF removed
############################################################################################################

### Get the estimated nominal number of I by race, current, with the appropriate number and distribution of races after SNF cases removed
race_obs_I_recent.REMOVED.frac <- apply(race_obs_I_recent.nominal.REMOVED, 2, function(x) round(x/sum(x),4))
race_obs_I_est.final.REMOVED <- round(I_current * race_obs_I_recent.REMOVED.frac)

race_obs_D_recent.REMOVED.frac <- apply(as.data.frame(race_observed_D_recent.nominal.REMOVED), 2, function(x) round(x/sum(x),4))
race_obs_D_est.final.REMOVED <- round(D_current * race_obs_D_recent.REMOVED.frac)

### CFR observed with estimated SNF removed
CFR_obs_WITH_SNF_removed <- round(race_obs_D_est.final.REMOVED / race_obs_I_est.final.REMOVED , 4)



```


