###```{r SNF-data-frac-calc, include=FALSE}

## INPUTS
# NH.rm.start.date
# input.NH.dates
# full.data.in


### Function to exponentially interpolate counts coming from SNF and remove that fraction from LA cases
NH_frac <- function(NH.rm.start.date,no_obs,NH.var.to.modify,input.NH.dates, full.data.in,var.to.modify){
  date.var.equals.1 <- as.Date(NH.rm.start.date)
  assume.first.NH.date <- as.numeric(date.var.equals.1 - as.Date("2020-03-01"))
  #plot(NH_dates,NH.counts)

  NH.interpolate.t.start <- assume.first.NH.date
  NH.interpolate.t.end <- (input.NH.dates[1]+1)
  NH.interpolate.t <- c(NH.interpolate.t.start, NH.interpolate.t.end)
  NH.interpolate.y <- c(1, NH.var.to.modify[1])

  interpolate_NH <- spline(x=NH.interpolate.t, y=(NH.interpolate.y)^(.5), n = (NH.interpolate.t.end - NH.interpolate.t.start +1), method="natural")
  interpolate_NH$y <- (interpolate_NH$y)^2
  #plot(interpolate_NH)

  NH_dates <- c(1:no_obs)
  NH.counts <- rep(0,no_obs)
  NH.counts[interpolate_NH$x] <- round(interpolate_NH$y)
  NH.counts[((NH.interpolate.t.end)+1):no_obs] <- NH.var.to.modify[3:length(NH.var.to.modify)]
  #plot(NH_dates,NH.counts)

  modify.var <- full.data.in[,var.to.modify]
  frac_NH <- NH.counts/modify.var
  frac_NH[is.nan(frac_NH)]<-0

  # This takes care of whether there are fewer NH input data points than cumcounts data points
  last.NH.date <- input.NH.dates[length(input.NH.dates)]
  if(last.NH.date < no_obs){
    frac_NH[c( (last.NH.date+1) : no_obs)] = rep(frac_NH[last.NH.date], times = length(frac_NH[c( (last.NH.date+1) : no_obs)])  )
  }
  return(frac_NH)
}

###########################################################################
## INTERPOLATE AND MODIFY I, H, V, D TO MODIFY CUMULATIVE AND NEW COUNTS UPDATED WITH NH CASES REMOVED

date.i<-as.numeric(as.Date("2020-04-21") - as.Date("2020-03-01"))

### MODIFY I
NH.var.to.modify <- (NH_counts[[1]] + NH_counts[[2]])
input.NH.dates <- c(1:length(NH.var.to.modify)) + date.i
NH.rm.start.date=as.Date("2020-03-30")
frac_NH_I <- NH_frac(NH.rm.start.date=NH.rm.start.date, no_obs=no_obs, NH.var.to.modify=NH.var.to.modify, input.NH.dates=input.NH.dates,
                     full.data.in = data.in.tmp.orig, var.to.modify = "Idetectcum")
#plot(x=(1:length(frac_NH_I)), y=frac_NH_I)

### MODIFY H
### Assume 0.4 I lead to H
NH.var.to.modify <- 1.5*(NH_counts[[3]])
NH.rm.start.date= NH.rm.start.date+1 #"2020-03-25"
frac_NH_H <- NH_frac(NH.rm.start.date=NH.rm.start.date, no_obs=no_obs, NH.var.to.modify=NH.var.to.modify, input.NH.dates=input.NH.dates,
                     full.data.in = data.in.tmp.orig, var.to.modify = "Htotcum")
#plot(x=(1:length(frac_NH_H)), y=frac_NH_H)

### MODIFY V
NH.var.to.modify <- 0.7*(NH_counts[[3]])
NH.rm.start.date= NH.rm.start.date+1   #"2020-03-27"
frac_NH_V <- NH_frac(NH.rm.start.date=NH.rm.start.date, no_obs=no_obs, NH.var.to.modify=NH.var.to.modify, input.NH.dates=input.NH.dates,
                     full.data.in = data.in.tmp.orig, var.to.modify = "Vcum")
#plot(x=(1:length(frac_NH_V)), y=frac_NH_V)

### MODIFY D
NH.var.to.modify <- NH_counts$D_resident_cum
NH.rm.start.date= NH.rm.start.date+1 #"2020-04-05"

frac_NH_D <- NH_frac(NH.rm.start.date=NH.rm.start.date, no_obs=no_obs, NH.var.to.modify=NH.var.to.modify, input.NH.dates=input.NH.dates,
                     full.data.in = data.in.tmp.orig, var.to.modify = "D")
#plot(x=(1:length(frac_NH_D)), y=frac_NH_D)




###########################################################################
## GET CUMULATIVE AND NEW COUNTS UPDATED WITH NH CASES REMOVED

cum_new_NH_fn <- function(no_obs, cum_orig, frac_NH){
  ### REMOVE ESTIMATED FRACTION OF NH CASES
  cum_NH <- round(cum_orig * (1-frac_NH))

  ### COUNT NEW CASES FROM CUM CASES
  new_NH <- rep(0,no_obs)
  new_NH[1]<-cum_NH[1]
  for (i in 2:length(cum_NH)){
    new_NH[i]<-cum_NH[i]-cum_NH[i-1]
  }

  ### REMOVE NEGATIVES IF EXIST
  if(any(new_NH<0)){
    ### (1) MAKE SURE THERE ARE NO NEGATIVES IN THE NEW CASES
    for (i in 1:no_obs){
      if (new_NH[i]!=0){
        new_NH[i] <- max(new_NH[i],1)
      }
    }
    ### (2) RECOUNT CUMCOUNTS
    cum.recount <- cum_NH
    for (i in 2:no_obs){
      cum.recount[i] <- cum.recount[i-1] + new_NH[i]
    }
    cum_NH <- cum.recount
  }

  return(data.frame(cbind(cum_NH, new_NH)))
}

### I NEW AND CUM
cum_orig <- data.in.tmp.orig$Idetectcum
frac_NH <- frac_NH_I
cases_cum_new <- cum_new_NH_fn(no_obs=no_obs, cum_orig=cum_orig, frac_NH=frac_NH)
Idetectcum <- cases_cum_new[,1]
I_detect_new <- cases_cum_new[,2]

### H NEW AND CUM
cum_orig <- data.in.tmp.orig$Htotcum
frac_NH <- frac_NH_H
cases_cum_new <- cum_new_NH_fn(no_obs=no_obs, cum_orig=cum_orig, frac_NH=frac_NH)
Htotcum <- cases_cum_new[,1]
H_new <- cases_cum_new[,2]

### V NEW AND CUM
cum_orig <- data.in.tmp.orig$Vcum
frac_NH <- frac_NH_V
cases_cum_new <- cum_new_NH_fn(no_obs=no_obs, cum_orig=cum_orig, frac_NH=frac_NH)
Vcum <- cases_cum_new[,1]
V_new <- cases_cum_new[,2]

### D NEW AND CUM
cum_orig <- data.in.tmp.orig$D
frac_NH <- frac_NH_D
cases_cum_new <- cum_new_NH_fn(no_obs=no_obs, cum_orig=cum_orig, frac_NH=frac_NH)
D <- cases_cum_new[,1]
D_new <- cases_cum_new[,2]

### JOIN ALL IN A DATA FRAME
cum_counts_NH_update <- as.data.frame(cbind(Htotcum, D, Vcum, Idetectcum, H_new, D_new, I_detect_new, V_new))

### DATA FORMAT
data.in.tmp <- cum_counts_NH_update %>% select(-c(V_new))


## PLOTTING CODE

### PLOT SNF REMOVED CUM COUNTS AGAINST ORIGINAL CUM COUNTS

plot.NH.removed <- function(no_obs, cum_orig, cum_NH, name.cum_orig, name.cum_NH,name.title){
  x = seq(as.Date("2020-03-01"), by = "day", length.out = no_obs)
  df1 <- data.frame(date = x,counts = cum_orig)
  df2 <- data.frame(date = x,counts = cum_NH)
  df4 <- data.frame(date = x,counts = cum_orig-cum_NH)

  df3 <- df1 %>%  mutate(Type = name.cum_orig) %>%
    bind_rows(df2 %>% mutate(Type = name.cum_NH)) %>%
    bind_rows(df4 %>% mutate(Type = "SNF estimated"))

  p<- ggplot(df3,aes(y = counts,x = date,color = Type)) +
    geom_line() +
    ggtitle(name.title)
}

### I NEW AND CUM
cum_orig <- data.in.tmp.orig$Idetectcum
cum_NH <- Idetectcum
name.cum_orig <- 'Cumul. Infected'
name.cum_NH <- 'Cumul. Infected NH Removed'
name.title <- "Cumulative Infected (Observed)"
p.I <- plot.NH.removed(no_obs=no_obs, cum_orig=cum_orig, cum_NH=cum_NH, name.cum_orig=name.cum_orig, name.cum_NH=name.cum_NH,name.title=name.title)

### H NEW AND CUM
cum_orig <- data.in.tmp.orig$Htotcum
cum_NH <- Htotcum
name.cum_orig <- 'Cumul. Hospital'
name.cum_NH <- 'Cumul. Hospital NH Removed'
name.title <- "Cumulative Hospitalized"
p.H <- plot.NH.removed(no_obs=no_obs, cum_orig=cum_orig, cum_NH=cum_NH, name.cum_orig=name.cum_orig, name.cum_NH=name.cum_NH,name.title=name.title)

### V NEW AND CUM
cum_orig <- data.in.tmp.orig$Vcum
cum_NH <- Vcum
name.cum_orig <- 'Cumul. Ventilations'
name.cum_NH <- 'Cumul. Ventilations NH Removed'
name.title <- "Cumulative Ventilations"
p.V <- plot.NH.removed(no_obs=no_obs, cum_orig=cum_orig, cum_NH=cum_NH, name.cum_orig=name.cum_orig, name.cum_NH=name.cum_NH,name.title=name.title)

### D NEW AND CUM
cum_orig <- data.in.tmp.orig$D
cum_NH <- D
name.cum_orig <- 'Cumul. Deaths'
name.cum_NH <- 'Cumul. Deaths NH Removed'
name.title <- "Cumulative Deaths"
p.D <- plot.NH.removed(no_obs=no_obs, cum_orig=cum_orig, cum_NH=cum_NH, name.cum_orig=name.cum_orig, name.cum_NH=name.cum_NH,name.title=name.title)


