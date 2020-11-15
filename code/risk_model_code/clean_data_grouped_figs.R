
################################################################
## Get data in form for plotting grouped figures
################################################################

profile.prev.data <- risk_table_CFR_FULL %>% select(c(Profile,riskprofile,age, BMI, smoking, comorbidity, freq.PREV.q))
colnames(profile.prev.data)[colnames(profile.prev.data)=="freq.PREV.q"] = "t1"
if (n.times > 1) {
  for (idx in 2:n.times) {    
    name <- times[idx]
    profile.prev.data[,name] <- profile.prev.data$t1 
  }}

data.melted <- melt(profile.prev.data, id = c('Profile', 'riskprofile', 'age', 'BMI', 'smoking', 'comorbidity'))
data.melted$DATE <- data.melted$variable
data.melted$variable <- NULL
data.melted$DATE <- factor(data.melted$DATE, levels = times)
data.melted$prevalence.gen.pop <- data.melted$value
data.melted$value <- NULL

## Merge with everything else
full.data <- merge(data.melted, dataI.melted, by = c('Profile', 'DATE' ) )
full.data <- merge(full.data, dataH.melted, by = c('Profile', 'DATE' ) )
full.data <- merge(full.data, dataQ.melted, by = c('Profile', 'DATE' ) )
full.data <- merge(full.data, dataD.melted, by = c('Profile', 'DATE' ) )

# ## Save
# full.data.orig <- full.data

## Melt prevalence variable
full.data <- full.data %>% 
  gather(keys, values, prevalence.gen.pop:prevalence.D )
full.data$stage <- full.data$keys
full.data$keys <- NULL 
full.data$stage <- factor(full.data$stage, levels =  c("prevalence.gen.pop", "prevalence.I","prevalence.H", "prevalence.Q", "prevalence.D"))

## Name the time periods depicted
times.names <- table.dates
names(times.names) <- times

################################################################
## Plot function for grouped figures
################################################################

grouped.var.figs <- function(full.data, var, var.name, times, table.dates){
  
  ## Name the time periods depicted
  times.names <- table.dates
  names(times.names) <- times
  
  ## Enable using aes_string()
  stage.str <- "stage"
  values.str <- "values"
  
  ## Fig
  grouped.fig <- ggplot(full.data, aes_string(x = stage.str, y = values.str, fill = var)) +
    geom_bar(position="stack", stat = 'identity') + 
    facet_wrap(DATE ~ ., labeller=labeller(DATE = times.names)) +
    labs(title = paste0(var.name, " by stage of disease"), x = NULL, y = "Frequency") +
    scale_x_discrete(labels = c("Prevalence", "Infected", "Hospitalized", "ICU", "Dead")) +
    theme(axis.text.x = element_text(angle = 90))
}


