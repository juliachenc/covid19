
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



