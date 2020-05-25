

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



