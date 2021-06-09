###################################################################################################
## data extracted from LA Times count-level cases, death, hospital, and in-icu data 
## https://github.com/datadesk/california-coronavirus-data

latimes_readin <- function(){
  
  # load COVID county-level hospital data
  hospital_readin <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/cdph-hospital-patient-county-totals.csv",
                                                     stringsAsFactors = TRUE) )
  hospital_readin <- hospital_readin %>% dplyr::filter(county=="Los Angeles")
  hospital_readin$Htot <- hospital_readin$positive_patients + hospital_readin$suspected_patients
  hospital_readin$Q <- hospital_readin$icu_positive_patients + hospital_readin$icu_suspected_patients
  # hospital_readin$Htot <- hospital_readin$positive_patients 
  # hospital_readin$Q <- hospital_readin$icu_positive_patients
  hospital <- select(hospital_readin, c(date, Htot, Q))
  hospital$date <- as.Date(hospital$date)
  
  # latest_data_dates <- latest_data
  # latest_data_dates$date <- as.Date("2020-03-01") + 0:(nrow(latest_data)-1)
  # 
  # data_joined <- left_join(latest_data_dates, hospital, by="date", keep=FALSE)
  
  
  # load COVID county-level case and death data
  case_readin <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-county-totals.csv",
                                                 stringsAsFactors = TRUE) )
  case_readin <- case_readin %>% dplyr::filter(county=="Los Angeles")
  case_readin$Idetectcum <- case_readin$confirmed_cases
  case_readin$I_detect_new <- case_readin$new_confirmed_cases
  case_readin$D <- case_readin$deaths
  case_readin$D_new <- case_readin$new_deaths
  cases <- select(case_readin, c(date,Idetectcum,I_detect_new,D,D_new))
  cases$date <- as.Date(cases$date)
  cases <- cases %>% dplyr::filter(date > "2020-02-29") %>% arrange(date)
  
  # smooth the outlier in new deaths over preceding 2 weeks
  D.max.at <- which(cases$D_new>500)
  D.max.cap <- 200
  to.smooth.14 <- (cases$D_new[D.max.at] - D.max.cap)/14
  cases$D_new[D.max.at] <- D.max.cap
  cases$D_new[c((D.max.at-14):(D.max.at-1))] <- cases$D_new[c((D.max.at-14):(D.max.at-1))] + to.smooth.14
  
  # recalculate cumulative after smoothing new D
  for (i in 2:nrow(cases)){
    cases$D[i] <- cases$D[i-1] + cases$D_new[i]
  }
  
  # join I and D with H and Q
  la_data <- left_join(cases, hospital, by="date", keep=FALSE)
  #la_data[is.na(la_data)] <- 0
  
  # la_data$I_detect_new = zoo::rollmean(la_data$I_detect_new, k = 7, fill = NA, align = 'right') %>% round(digits=0)
  # la_data$D_new = zoo::rollmean(la_data$D_new, k = 7, fill = NA, align = 'right') %>% round(digits=0)
  
  return(la_data)
  
}

#la_data <- latimes_readin()