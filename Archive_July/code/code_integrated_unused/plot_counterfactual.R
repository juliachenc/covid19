
## COUNTERFACTUAL

## GENERATE COUNTERFACTUAL SCENARIO TRAJ
scenario = 0
intervention_date = Sys.Date()
ABC.out.mat.CTF <- ABC.out.mat.yesR
ABC.out.mat <- ABC.out.mat.CTF
# number.pars.to.use = 400
# iter = 30
number.pars.to.use = 1000
iter = 20

time.steps=400
sd.redux = NULL
traj.scenario0 <- model.output.to.plot.SIM(ABC.out.mat, par.vec.length=number.pars.to.use, iter=iter, time.steps=time.steps, vars.to.plot = vars.plus.R,
                                          init.date.data="2020-03-01", all=TRUE, scenario.selection = scenario,intervention_date=intervention_date,sd.redux=sd.redux)

## PLOTS
traj.CI = traj.scenario0
date.offset.4plot=0
time.steps.4plot= as.numeric(as.Date("2020-07-18") - as.Date("2020-03-01"))
data.in <- NULL

vars.to.plot <- c("Htot","Q", "V","D_new")
chart.title <- "Healthcare Variables"
#curr.healthcare.variables.COUNTERFACTUAL.50 <- plot.together.50(traj.CI=traj.CI, data.in=NULL, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)

### Plot 95%
curr.healthcare.variables.COUNTERFACTUAL <- plot.together(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
curr.healthcare.variables.COUNTERFACTUAL
# pdf(file = path(fig.dir, "curr.healthcare.variables.COUNTERFACTUAL.pdf"), width=10, height =10)
# curr.healthcare.variables.COUNTERFACTUAL
# dev.off()

### Plot 50%
curr.healthcare.variables.COUNTERFACTUAL.50 <- plot.together.50(traj.CI=traj.CI, data.in=data.in, time.steps.4plot = time.steps.4plot, vars.to.plot = vars.to.plot)
curr.healthcare.variables.COUNTERFACTUAL.50
# pdf(file = path(fig.dir, "curr.healthcare.variables.COUNTERFACTUAL.50.pdf"), width=10, height =10)
# curr.healthcare.variables.COUNTERFACTUAL.50
# dev.off()

## PLOT SINGLE TO GET CI
var.to.plot <- "D"
scenario=0
intervention_date = intervention_date
sd.redux = NULL
use.title=NULL
plot.capacity=NULL
ymax=NULL
traj.CI <- traj.scenario0

yesR.p.H.0 <- plot.model.single(traj.CI=traj.CI, data.in=data.in.tmp, init.date.data="2020-03-01", date.offset.4plot=0,
                                time.steps.4plot=time.steps.4plot, ymax=ymax, plot.capacity=plot.capacity, var.to.plot=var.to.plot,use.title=use.title,
                                scenario=scenario, intervention_date=intervention_date, sd.redux=sd.redux)
ggplotly(yesR.p.H.0)


# 35000 total deaths (95\%CI: 15800, 89500)

AA <- c(35000, 15800, 89500)
AA / 2195







