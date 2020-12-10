
plot.model.single.test <- plot.percent.LAC(traj.CI=traj.0, data.in=latest_data, init.date.data=as.Date("2020-03-01"), time.steps.4plot=time.steps.4plot, ymax=NULL, plot.capacity=NULL, plot.annotations=TRUE, var.to.plot="Itot")
ggplotly(plot.model.single.test)

plot.percent.LAC <- function(traj.CI, data.in, init.date.data=NULL, time.steps.4plot=NULL, ymax=NULL, plot.capacity=NULL, plot.annotations=NULL, var.to.plot=NULL) {

  LACpop <- 1.0079e7

  ## Filter only to variable of interest
  traj.CI <- traj.CI %>%  dplyr::filter(state.name==var.to.plot)

  ## Select only more recent dates
  init.date <- init.date.data
  init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
  startDatePlot <- init.date #- date.offset.4plot #15
  endDatePlot <- startDatePlot + time.steps.4plot #- 40  # the constant 40 because the traj are not aligned to start date
  traj.CI <- traj.CI %>% dplyr::filter(date > startDatePlot) %>% dplyr::filter(date < endDatePlot)

  ## Get as percentage of LAC population
  library(data.table)
  traj.df <- setDT(traj.CI)
  traj.df <- traj.df[, lapply(.SD, function(x) round(x*100 / LACpop, 5)), by = c("date","state.name"), .SDcols = 4:9]

  traj.CI <- as.data.frame(traj.df)

  y.max.in <- max(traj.CI$up_95)

  longnames <- c("Susceptible",
                 "New Obs. Infected",
                 "Current % of LAC population infected (observed infections)",
                 "Cum. Obs. Infected",
                 "Current % of LAC population infected (estimated total infections)",
                 "Cum. Tot. Infected",
                 "New in Hospital",
                 "Current in Hospital",
                 "Cum. in Hospital",
                 "Current in ICU",
                 "Cum. in ICU",
                 "Current Ventilation",
                 "Cum. Ventilation",
                 "New Deaths",
                 "Cum. Deaths",
                 "Recovered")

  names(longnames) <-  c("S",
                         "I_detect_new",
                         "I",
                         "Idetectcum",
                         "Itot",
                         "Itotcum",
                         "H_new",
                         "Htot",
                         "Htotcum",
                         "Q",
                         "Qcum",
                         "V",
                         "Vcum",
                         "D_new",
                         "D",
                         "R"
  )

  ## Colors

  cols.list <- c(
    "salmon",
    "sandybrown",
    "navajowhite3",
    "olivedrab4",
    "olivedrab2",
    "mediumseagreen",
    "mediumaquamarine",
    "mediumturquoise",
    "cyan2",
    "lightskyblue",
    "steelblue2",
    "mediumpurple",
    "mediumorchid",
    "plum1",
    "violetred1",
    "deeppink3",
    "deeppink4"
  )
  names(cols.list) <- names(longnames)
  color.this.var <- as.character(cols.list[var.to.plot])


  ## PLOTTING
  #traj.CI.line <- reshape2::melt(traj.CI[c("date", "state.name", "mean", "median")], id.vars = c("date", "state.name"))
  traj.CI.line <- reshape2::melt(traj.CI[c("date", "state.name", "median")], id.vars = c("date", "state.name"))
  traj.CI.area <- reshape2::melt(traj.CI[c("date", "state.name", "low_95", "low_50", "up_50", "up_95")], id.vars = c("date", "state.name"))
  traj.CI.area$type <- sapply(traj.CI.area$variable, function(x) {str_split(x, "_")[[1]][1]})
  traj.CI.area$CI <- sapply(traj.CI.area$variable, function(x) {str_split(x, "_")[[1]][2]})
  traj.CI.area$variable <- NULL
  traj.CI.area <- reshape2::dcast(traj.CI.area, "date+state.name+CI~type")

  traj.CI$title <- as.character(longnames[var.to.plot])



  ### THE PLOT
  p <- ggplot(traj.CI.area)

  if(!is.null(ymax)){
    p <- p + facet_wrap(~state.name, labeller = labeller(state.name = longnames), scales = "fixed")
  }
  else {
    p <- p + facet_wrap(~state.name, labeller = labeller(state.name = longnames), scales = "free_y")}

  p <- p + geom_ribbon(data = traj.CI.area, aes_string(x = "date", ymin = "low", ymax = "up", alpha = "CI"), fill = color.this.var, show.legend = c(fill=FALSE))
  p <- p + geom_line(data = traj.CI.line, aes_string(x = "date", y = "value", linetype = "variable"), colour = color.this.var, size=1, show.legend = c(colour=FALSE))

  ## ADD LEGENDS
  p <- p + scale_alpha_manual("Percentile", values = c("95" = 0.20, "50" = 0.50), labels = c("95" = "95th", "50" = "50th"))
  p <- p + scale_linetype("Stats")
  p <- p + guides(linetype = guide_legend(order = 1))



  #################
  ## ADD DATE ANNOTATIONS
  if (!is.null(plot.annotations)){
    ######### Create data frame with annotations
    traj.CI.date <- as.data.frame(matrix(NA, nrow=7, ncol=3))
    colnames(traj.CI.date) <- c("date","date.label","y.place")
    traj.CI.date$date <- c(as.Date("2020-03-19"),as.Date("2020-05-08"),as.Date("2020-06-12"),as.Date("2020-07-01"),as.Date("2020-08-18"),as.Date("2020-10-31"),as.Date("2020-11-26"))
    traj.CI.date$date.label <- c("Stage I", "Stage II", "Stage III", "Modifications", "School Year", "Halloween", "Thanksgiving")
    traj.CI.date$y.place <- c(1:7)
    ######### Add data frame with annotations
    p <- p + geom_vline(data=traj.CI.date, aes(xintercept=as.Date(date)), linetype="dashed",colour="azure4", size=.35) +
      # annotate("text", label = traj.CI.date$date.label, x = traj.CI.date$date, y = (y.max.in/2)+(y.max.in/20)*traj.CI.date$y.place, size = 3.5, colour = "black")
      annotate("text", label = traj.CI.date$date.label, x = traj.CI.date$date, y = (y.max.in)-(y.max.in/25)*traj.CI.date$y.place, size = 3.5, colour = "black")
  }


  ## FINAL THEMES AND EDITING
  p <- p + theme_bw() + theme(legend.position = "top", legend.box = "horizontal")
  p <- p + scale_x_date(limits = as.Date(c(startDatePlot,endDatePlot)), date_breaks = "2 weeks" , date_labels = "%d-%b-%y")
  p <- p + theme(axis.text.x = element_text(angle = 90),
                 strip.text.x = element_text(size = 12, face = "bold"))
  p <- p + ylab("% Infected") + xlab(NULL)
  #p <- p + ylab(NULL) + xlab(NULL)


  if(!is.null(ymax)){
    p <- p + scale_y_continuous(limits=c(0,ymax) , labels = scales::comma, breaks=seq(0,ymax,ymax/10))
  }
  else {  p <- p + scale_y_continuous(labels = scales::comma)}

  p

}

get.current.percent(traj.CI=traj.0, date.selected="2020-12-05", var.to.plot="I")


get.current.percent <- function(traj.CI=traj.CI, date.selected=date.selected,var.to.plot=var.to.plot){

## Filter only to variable of interest
traj.CI <- traj.CI %>%  dplyr::filter(state.name==var.to.plot)

## Select only more recent dates
date.selected <- as.Date(date.selected)
init.date <- as.Date("2020-03-01")
init.date <- as.Date(init.date) #as.Date(lubridate::ydm(init.date))
startDatePlot <- init.date #- date.offset.4plot #15
endDatePlot <- startDatePlot + time.steps.4plot #- 40  # the constant 40 because the traj are not aligned to start date
traj.CI <- traj.CI %>% dplyr::filter(date==date.selected)

## Get as percentage of LAC population
library(data.table)
traj.df <- setDT(traj.CI)
traj.df <- traj.df[, lapply(.SD, function(x) round(x*100 / LACpop, 5)), by = c("date","state.name"), .SDcols = 4:9]
traj.CI <- as.data.frame(traj.df)

traj.CI.curr.date = traj.CI %>% filter(date=="2020-12-09")

traj.CI.curr.date <- traj.CI
median.curr <- traj.CI.curr.date$median
max.curr <- traj.CI.curr.date$up_95
min.curr <- traj.CI.curr.date$low_95

current.percentage <- paste0(median.curr, " %", " (95%CI: ",min.curr,",",max.curr,")")

return(current.percentage)
}



